# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::HTTP.pm,v 0.40 2000/10/15 18:20:55 $
#
# ======================================================================

package SOAP::Transport::HTTP;

use strict;
use vars qw($VERSION);
$VERSION = '0.40';

# ======================================================================

package SOAP::Transport::HTTP::Client;

use vars qw(@ISA);
@ISA = qw(LWP::UserAgent);

sub new { eval "use LWP::UserAgent; use URI"; die if $@;
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
    $self->agent(join '/', 'SOAP::Transport', 'Perl', SOAP::Transport::HTTP->VERSION);
    $self->on_debug(sub {});
  }

  if (@_) {
    my %parameters = @_;
    foreach (grep {defined $parameters{$_}} keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }
  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(endpoint code message is_success status on_debug parameters)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
}

sub send_receive {
  my($self, %parameters) = @_;
  my($envelope, $endpoint, $action) = 
    @parameters{qw(envelope endpoint action)};

  $endpoint ||= $self->endpoint;

  my $req = HTTP::Request->new(POST => $endpoint, HTTP::Headers->new, $envelope);

  $req->header('SOAPAction' => $action);
  $req->header(Host => URI->new($self->endpoint)->host_port);
  $req->content_type('text/xml');
  $req->content_length(length($envelope));

  $self->on_debug->($req->as_string);
    
  my $resp = $self->SUPER::request($req);

  $self->on_debug->($resp->as_string);

# 200 OK
# 204 OK, one-way method
# 3?? Redirects, cache results
# 405/510 POST not supported, try M-POST
#+ 500 Server/Client side error, content can be present

  $self->code($resp->code);
  $self->message($resp->message);
  $self->is_success($resp->is_success);
  $self->status($resp->status_line);

  $resp->content;
}

# ======================================================================

package SOAP::Transport::HTTP::Server;

use SOAP::Lite;
use vars qw(@ISA);
@ISA = qw(SOAP::Server);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
    $self->on_action(sub {
      (my $action = shift) =~ s/^("?)(.+)\1$/$2/;
      die "SOAPAction shall match 'uri#method' if present\n" 
        if $action && $action ne join '#', @_;
    });
  }

  if (@_) {
    my %parameters = @_;
    foreach (grep {defined $parameters{$_}} keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }
  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(request response)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
}

sub handle {
  my $self = shift->new;

  return $self->response(HTTP::Response->new(400)) # BAD_REQUEST
    unless $self->request->method eq 'POST';

  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Request' => 'Content-Type must be text/xml')
    unless $self->request->content_type eq 'text/xml';

  $self->action($self->request->header('SOAPAction'));

  my $response = $self->SUPER::handle($self->request->content) or return;

  $self->response(HTTP::Response->new( 
     $SOAP::Constants::HTTP_ON_SUCCESS_CODE => undef, 
     HTTP::Headers->new('Content-Type' => 'text/xml', 'Content-Length' => length $response), 
     $response,
  ));
}

sub make_fault {
  my $self = shift;
  my $response = $self->SUPER::make_fault(@_);
  $self->response(HTTP::Response->new(
     $SOAP::Constants::HTTP_ON_FAULT_CODE => undef,
     HTTP::Headers->new('Content-Type' => 'text/xml', 'Content-Length' => length $response),
     $response,
  ));
  return;
}

# ======================================================================

package SOAP::Transport::HTTP::CGI;

use vars qw(@ISA);
@ISA = qw(SOAP::Transport::HTTP::Server);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
  }
  return $self;
}

sub handle {
  my $self = shift->new;

  my $content; read(STDIN,$content,$ENV{'CONTENT_LENGTH'});  
  $self->request(HTTP::Request->new( 
    $ENV{'REQUEST_METHOD'} => $ENV{'SCRIPT_NAME'},
    HTTP::Headers->new(
      map { (my $http = uc $_) =~ s/-/_/g; $_ => $ENV{"HTTP_$http"} || $ENV{$http};
          } qw(Content-Type SOAPAction),
    ),
    $content,
  ));
  $self->SUPER::handle;

  my $code = $self->response->code;
  binmode(STDOUT); print STDOUT 
    "Status: $code ", HTTP::Status::status_message($code), 
    "\015\012", $self->response->headers_as_string, 
    "\015\012", $self->response->content;
}

# ======================================================================

package SOAP::Transport::HTTP::Daemon;

use vars qw($AUTOLOAD @ISA);
@ISA = qw(SOAP::Transport::HTTP::Server);

sub new { eval "use HTTP::Daemon"; die if $@;
  my $self = shift;
  my $class = ref($self) || $self;
  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
    $self->{_daemon} = HTTP::Daemon->new(@_);
  }
  return $self;
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->{_daemon}->$method(@_) };
  goto &$AUTOLOAD;
}

sub handle {
  my $self = shift->new;
  while (my $c = $self->accept) {
      while (my $r = $c->get_request) {
          $self->request($r);
          $self->SUPER::handle;
          $c->send_response($self->response)
      }
      $c->close;
      undef $c;
  }
}

# ======================================================================

package SOAP::Transport::HTTP::Apache;

use vars qw(@ISA);
@ISA = qw(SOAP::Transport::HTTP::Server);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
  }
  return $self;
}

sub handler { eval "use Apache; use Apache::Constants qw(OK)"; die if $@;
  my $self = shift->new; 
  my $r = shift || Apache->request; 

  $self->request(HTTP::Request->new( 
    $r->method => $r->uri,
    HTTP::Headers->new(
      'Content-Type' => $r->header_in('Content-type'),
      'SOAPAction' => $r->header_in('SOAPAction'),
    ),
    do { my $buf; $r->read($buf, $r->header_in('Content-length')); $buf; } 
  ));
  $self->SUPER::handle;

  my $header = $self->response->is_success ? 'header_out' : 'err_header_out';
  $r->$header('Content-length' => $self->response->content_length);
  $r->content_type($self->response->content_type);
  $r->status($self->response->code); 
  $r->send_http_header;
  $r->print($self->response->content) unless $r->header_only;

  &OK;
}

*handle = \&handler; # just create alias

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Transport::HTTP - Server/Client side HTTP support for SOAP::Lite for Perl

=head1 SYNOPSIS

  use SOAP::Transport::HTTP;            
  my $server = SOAP::Transport::HTTP::Server->new; # create new server
# set path for deployed modules (see explanation below)
  $server->dispatch_to('/Path/To/Deployed/Modules');
  $server->request(new HTTP::Request); # set request object  HTTP::Request
  $server->handle($content);           # handle request
  $server->response;                   # get response object HTTP::Response

=head1 DESCRIPTION

This class encapsulates all HTTP related logic for a SOAP server,
independent of what web server it's attached to. 
If you want to use this class you should follow simple guideline
mentioned above. 

Following methods are available:

=over 4

=item on_action()

on_action method lets you specify SOAPAction understanding. It accepts
reference to subroutine that takes three parameters: 

  SOAPAction, method_uri and method_name. 

C<SOAPAction> is taken from HTTP header and method_uri and method_name are 
extracted from request's body. Default behavior is match C<SOAPAction> if 
present and ignore it otherwise. You can specify you own, for example 
die if C<SOAPAction> doesn't match with following code:

  $server->on_action(sub {
    (my $action = shift) =~ s/^("?)(.+)\1$/$2/;
    die "SOAPAction shall match 'uri#method'\n" if $action ne join '#', @_;
  });

=item dispatch_to()

dispatch_to lets you specify where you want to dispatch your services 
to. More precisely, you can specify C<PATH>, C<MODULE>, C<method> or 
combination C<MODULE::method>. Example:

  dispatch_to( 
    'PATH/',          # dynamic: load anything from there, any module, any method
    'MODULE',         # static: any method from this module 
    'MODULE::method', # static: specified method from this module
    'method',         # static: specified method from main:: 
  );

If you specify C<PATH/> name of module/classes will be taken from uri as 
path component and converted to Perl module name with substitution 
'::' for '/'. Example:

  urn:My/Examples              => My::Examples
  urn://localhost/My/Examples  => My::Examples
  http://localhost/My/Examples => My::Examples

For consistency first '/' in the path will be ignored.

According to this scheme to deploy new class you should put this
class in one of the specified directories and enjoy its services.
Easy, eh? 

=item handle()

handle method will handle your request. You should provide parameters
with request() method, call handle() and get it back with response() .

=item request()

request method gives you access to HTTP::Request object which you
can provide for Server component to handle request.

=item response()

response method gives you access to HTTP::Response object which 
you can access to get results from Server component after request was
handled.

=back

=head2 SERVICE DEPLOYMENT. STATIC AND DYNAMIC

Let us scrutinize deployment process. Designing your SOAP server you 
can consider two kind of deployment: B<static> and B<dynamic>.
For both static and dynamic deployment you should specify C<MODULE>, 
C<MODULE::method>, C<method> or C<PATH/>. Difference between static and dynamic
deployment is that if module is not present it'll be loaded on
demand. See L</SECURITY> section for detailed description.

Example for B<static> deployment:

  use SOAP::Transport::HTTP;
  use My::Examples;           # module is preloaded 

  SOAP::Transport::HTTP::CGI
    # deployed module should be present here or client will get 'access denied'
    -> dispatch_to('My::Examples') 
    -> handle;

Example for B<dynamic> deployment:

  use SOAP::Transport::HTTP;
  # name is unknown, module will be loaded on demand

  SOAP::Transport::HTTP::CGI
    # deployed module should be present here or client will get 'access denied'
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'My::Examples') 
    -> handle;

For static deployment you should specify MODULE name directly. 
For dynamic deployment you can specify name either directly (in that 
case it'll be required with no restriction) or indirectly, with PATH
(in that case ONLY path that'll be available will be PATH from 
dispatch_to() parameters). For information how to handle this situation
see L</SECURITY> section.

=head2 SECURITY

Due to security reasons if you choose dynamic deployment and specified 
C<PATH/>, current path for perl modules (C<@INC>) will be disabled. 
If you want to access other modules in your included package you have 
several options:

=over 4

=item 1

Switch to static linking:

   use MODULE;
   $server->dispatch_to('MODULE');

It can be usable also when you want to import something specific
from deployed modules: 

   use MODULE qw(import_list);

=item 2

Change C<use> to C<require>. Path is unavailable only during 
initialization part, and it's available again during execution. 
So, if you do C<require> somewhere in your package it'll work.

=item 3

Same thing, but you can do: 

   eval 'use MODULE qw(import_list)'; die if $@;

=item 4

Assign C<@INC> directory in your package and then make C<use>.
Don't forget to put C<@INC> in C<BEGIN{}> block or it won't work:

   BEGIN { @INC = qw(my_directory); use MODULE }

Personally I don't like this method, better options are available.

=back

=head1 EXAMPLES

Consider following examples of SOAP servers:

=over 4

=item CGI:

  use SOAP::Transport::HTTP;

  SOAP::Transport::HTTP::CGI
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
    -> handle
  ;

=item daemon:

  use SOAP::Transport::HTTP;

  my $daemon = SOAP::Transport::HTTP::Daemon
    -> new (LocalAddr => 'localhost', LocalPort => 80)
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
  ;
  print "Contact to SOAP server at ", $daemon->url, "\n";
  $daemon->handle;

=item mod_perl:

httpd.conf:

  <Location /soap>
    SetHandler perl-script
    PerlHandler SOAP::Apache
  </Location>

Apache.pm:

  package SOAP::Apache;

  use SOAP::Transport::HTTP;

  my $server = SOAP::Transport::HTTP::Apache
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method'); 

  sub handler { $server->handler(@_) }

  1;

=back

=head1 DEPENDENCIES

 Crypt::SSLeay             for HTTPS/SSL
 SOAP::Lite, URI           for SOAP::Transport::HTTP::Server
 LWP::UserAgent, URI       for SOAP::Transport::HTTP::Client
 HTTP::Daemon              for SOAP::Transport::HTTP::Daemon
 Apache, Apache::Constants for SOAP::Transport::HTTP::Apache

=head1 SEE ALSO

 See ::CGI, ::Daemon and ::Apache for implementation details.
 See examples/soap.cgi as SOAP::Transport::HTTP::CGI example.
 See examples/soap.daemon as SOAP::Transport::HTTP::Daemon example.
 See examples/My/Apache.pm as SOAP::Transport::HTTP::Apache example.

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
