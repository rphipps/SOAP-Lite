# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::HTTP.pm,v 0.46 2001/01/31 16:30:24 $
#
# ======================================================================

package SOAP::Transport::HTTP;

use strict;
use vars qw($VERSION);
$VERSION = '0.46';

# ======================================================================

package SOAP::Transport::HTTP::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Client LWP::UserAgent);

use SOAP::Lite;

my(%redirect, %mpost);

# hack for HTTP conection that returns Keep-Alive 
# miscommunication (?) between LWP::Protocol and LWP::Protocol::http
# die after timeout
sub patch { 
  local $^W; 
  eval "package LWP::UserAgent; sub redirect_ok {1}";
  { package LWP::Protocol; 
    my $collect = \&collect; # store original  
    *collect = sub {          
      if (defined $_[2]->header('Connection') && $_[2]->header('Connection') eq 'Keep-Alive') {
        my $data = $_[3]->(); 
        my $next = length($$data) == $_[2]->header('Content-Length') ? sub { \'' } : $_[3];
        my $done = 0; $_[3] = sub { $done++ ? &$next : $data };
      }
      goto &$collect;
    };
  }
  *patch = sub {};
};

sub DESTROY { SOAP::Trace::objects('()') }

sub new { require LWP::UserAgent; patch;
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    my(@params, @methods);
    while (@_) { $class->can($_[0]) ? push(@methods, shift() => shift) : push(@params, shift) }
    $self = $class->SUPER::new(@params);
    $self->agent(join '/', 'SOAP::Lite', 'Perl', SOAP::Transport::HTTP->VERSION);
    while (@methods) { my($method, $params) = splice(@methods,0,2);
      $self->$method(ref $params eq 'ARRAY' ? @$params : $params) 
    }
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub send_receive {
  my($self, %parameters) = @_;
  my($envelope, $endpoint, $action) = 
    @parameters{qw(envelope endpoint action)};

  $endpoint ||= $self->endpoint;

  my $method = 'POST';
  my $resp;
  while (1) { 

    # check cache for redirect
    $endpoint = $redirect{$endpoint} if exists $redirect{$endpoint};
    # check cache for M-POST
    $method = 'M-POST' if exists $mpost{$endpoint};

    my $req = HTTP::Request->new($method => $endpoint, HTTP::Headers->new, $envelope);
    $req->proxy_authorization_basic($ENV{'HTTP_proxy_user'}, $ENV{'HTTP_proxy_pass'})
      if ($ENV{'HTTP_proxy_user'} && $ENV{'HTTP_proxy_pass'}); # by Murray Nesbitt 

    if ($method eq 'M-POST') {
      my $prefix = sprintf '%04d', int(rand(1000));
      $req->header(Man => qq!"$SOAP::Constants::NS_ENV"; ns=$prefix!);
      $req->header("$prefix-SOAPAction" => $action);  
    } else {
      $req->header(SOAPAction => $action);
    }
    $req->content_type('text/xml');
    $req->content_length(length($envelope));

    SOAP::Trace::transport($req);
    SOAP::Trace::debug($req->as_string);
    
    $self->SUPER::env_proxy if $ENV{'HTTP_proxy'};

    $resp = $self->SUPER::request($req);

    SOAP::Trace::transport($resp);
    SOAP::Trace::debug($resp->as_string);

    # 100 OK, continue to read?
    if (($resp->code == 510 || $resp->code == 501) && 
        $method ne 'M-POST') { 
      $mpost{$endpoint} = 1;
    } else {
      last;
    }
  }

  $redirect{$endpoint} = $resp->request->url
    if $resp->previous && $resp->previous->is_redirect;

  $self->code($resp->code);
  $self->message($resp->message);
  $self->is_success($resp->is_success);
  $self->status($resp->status_line);

  join '', $resp->content_type =~ m!^multipart/! ? ($resp->headers_as_string, "\n") : '',
           $resp->content;
}

# ======================================================================

package SOAP::Transport::HTTP::Server;

use vars qw(@ISA);
@ISA = qw(SOAP::Server);

use SOAP::Lite;
use URI;

sub DESTROY { SOAP::Trace::objects('()') }

sub new { require LWP::UserAgent;
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new(@_);
    $self->on_action(sub {
      (my $action = shift) =~ s/^("?)(.*)\1$/$2/;
      die "SOAPAction shall match 'uri#method' if present\n" 
        if $action && $action ne join('#', @_) && $action ne join('/', @_);
    });
    SOAP::Trace::objects('()');
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

  if ($self->request->method eq 'POST') {
    $self->action($self->request->header('SOAPAction'));
  } elsif ($self->request->method eq 'M-POST') {
    return $self->response(HTTP::Response->new(510, # NOT EXTENDED
           "Expected Mandatory header with $SOAP::Constants::NS_ENV as unique URI")) 
      if $self->request->header('Man') !~ /^"$SOAP::Constants::NS_ENV";\s*ns\s*=\s*(\d+)/;
    $self->action($self->request->header("$1-SOAPAction"));
  } else {
    return $self->response(HTTP::Response->new(405)) # METHOD NOT ALLOWED
  }

  my $content_type = $self->request->content_type || '';
  # in some environments (PerlEx?) content_type could be empty, so allow it also
  # anyway it'll blow up inside ::Server::handle if something wrong with message
  # TBD: but what to do with MIME encoded messages in THESE environments?
  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Request' => 'Content-Type must be text/xml')
    if $content_type && $content_type ne 'text/xml' && $content_type !~ m!^multipart/!;

  my $response = $self->SUPER::handle(
    join '', $content_type =~ m!^multipart/! ? ($self->request->headers_as_string, "\n") : '', 
             $self->request->content
  ) or return;

  $self->make_response($SOAP::Constants::HTTP_ON_SUCCESS_CODE, $response);
}

sub make_fault {
  my $self = shift;
  $self->make_response($SOAP::Constants::HTTP_ON_FAULT_CODE => $self->SUPER::make_fault(@_));
  return;
}

sub make_response {
  my $self = shift;
  my($code, $response) = @_;
  $response =~ s!(\?>)!$1<?xml-stylesheet type="text/css"?>! if $self->request->content_type eq 'multipart/form-data';
  $self->response(HTTP::Response->new( 
     $code => undef, 
     HTTP::Headers->new(
       'SOAPServer' => join('/', 'SOAP::Lite', 'Perl', SOAP::Transport::HTTP->VERSION),
       'Content-Type' => 'text/xml', 
       'Content-Length' => length $response), 
     $response,
  ));
}

# ======================================================================

package SOAP::Transport::HTTP::CGI;

use vars qw(@ISA);
@ISA = qw(SOAP::Transport::HTTP::Server);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new(@_);
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub handle {
  my $self = shift->new;

  my $content; read(STDIN,$content,$ENV{'CONTENT_LENGTH'} || 0);  
  $self->request(HTTP::Request->new( 
    $ENV{'REQUEST_METHOD'} || '' => $ENV{'SCRIPT_NAME'},
    HTTP::Headers->new(map {(/^HTTP_(.+)/i ? $1 : $_) => $ENV{$_}} keys %ENV),
    $content,
  ));
  $self->SUPER::handle;

  # imitate nph- cgi for IIS (pointed by Murray Nesbitt)
  my $status = defined($ENV{'SERVER_SOFTWARE'}) && $ENV{'SERVER_SOFTWARE'}=~/IIS/
    ? $ENV{SERVER_PROTOCOL} || 'HTTP/1.0' : 'Status:';
  my $code = $self->response->code;
  binmode(STDOUT); print STDOUT 
    "$status $code ", HTTP::Status::status_message($code), 
    "\015\012", $self->response->headers_as_string, 
    "\015\012", $self->response->content;
}

# ======================================================================

package SOAP::Transport::HTTP::Daemon;

use Carp ();
use vars qw($AUTOLOAD @ISA);
@ISA = qw(SOAP::Transport::HTTP::Server);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { require HTTP::Daemon; 
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new();
    $self->{_daemon} = HTTP::Daemon->new(@_) or Carp::croak "Can't create daemon: $!";
    $self->myuri(URI->new($self->url)->canonical->as_string);
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub AUTOLOAD {
  my $method = substr($AUTOLOAD, rindex($AUTOLOAD, '::') + 2);
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

sub DESTROY { SOAP::Trace::objects('()') }

sub new { require Apache;
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new(@_);
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub handler { 
  my $self = shift->new; 
  my $r = shift || Apache->request; 

  $self->request(HTTP::Request->new( 
    $r->method => $r->uri,
    HTTP::Headers->new($r->headers_in),
    do { my $buf; $r->read($buf, $r->header_in('Content-length')); $buf; } 
  ));
  $self->SUPER::handle;

  if ($self->response->is_success) {
    $r->header_out('Content-length' => $self->response->content_length);
    $r->send_http_header($self->response->content_type);
    $r->print($self->response->content);
  } else {
    $r->err_header_out('Content-length' => $self->response->content_length);
    $r->content_type($self->response->content_type);
    $r->custom_response($self->response->code, $self->response->content);
  }
  $self->response->code;
}

*handle = \&handler; # just create alias

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Transport::HTTP - Server/Client side HTTP support for SOAP::Lite

=head1 SYNOPSIS

=over 4

=item Client

  use SOAP::Lite 
    uri => 'http://my.own.site.com/My/Examples',
    proxy => 'http://localhost/', 
  # proxy => 'http://localhost/cgi-bin/soap.cgi', # local CGI server
  # proxy => 'http://localhost/',                 # local daemon server
  # proxy => 'http://localhost/soap',             # local mod_perl server
  # proxy => 'https://localhost/soap',            # local mod_perl SECURE server
  # proxy => 'http://login:password@localhost/cgi-bin/soap.cgi', # local CGI server with authentication
  ;

  print getStateName(1);

=item CGI server

  use SOAP::Transport::HTTP;

  SOAP::Transport::HTTP::CGI
    # specify path to My/Examples.pm here
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
    -> handle
  ;

=item Daemon server

  use SOAP::Transport::HTTP;

  # change LocalPort to 81 if you want to test it with soapmark.pl

  my $daemon = SOAP::Transport::HTTP::Daemon
    -> new (LocalAddr => 'localhost', LocalPort => 80)
    # specify list of objects-by-reference here 
    -> objects_by_reference(qw(My::PersistentIterator My::SessionIterator My::Chat))
    # specify path to My/Examples.pm here
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
  ;
  print "Contact to SOAP server at ", $daemon->url, "\n";
  $daemon->handle;

=item Apache mod_perl server

See F<examples/server/Apache.pm> and L</EXAMPLES> section for more information.

=back

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

=head2 PROXY SETTINGS

You can use any proxy setting you use with LWP::UserAgent modules:

 SOAP::Lite->proxy('http://endpoint.server', 
                   proxy => ['http' => 'http://my.proxy.server']);

or

 $soap->transport->proxy('http' => 'http://my.proxy.server');

should specify proxy server for you. And if you use C<HTTP_proxy_user> 
and C<HTTP_proxy_pass> for proxy authorization SOAP::Lite should know 
how to handle it properly. 

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

=item Apache::Registry:

httpd.conf:

  Alias /mod_perl/ "/Apache/mod_perl/"
  <Location /mod_perl>
    SetHandler perl-script
    PerlHandler Apache::Registry
    PerlSendHeader On
    Options +ExecCGI
  </Location>

soap.mod_cgi (put it in /Apache/mod_perl/ directory mentioned above)

  use SOAP::Transport::HTTP;

  SOAP::Transport::HTTP::CGI
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
    -> handle
  ;

=back

WARNING: dynamic deployment with Apache::Registry will fail, because 
module will be loaded dynamically only for the first time. After that 
it is already in the memory, that will bypass dynamic deployment and 
produces error about denied access. Specify both PATH/ and MODULE name 
in dispatch_to() and module will be loaded dynamically and then will work 
as under static deployment. See examples/server/soap.mod_cgi for example.

=head1 TROUBLESHOOTING

=over 4

=item Dynamic libraries are not found

If you see in webserver's log file something like this: 

Can't load '/usr/local/lib/perl5/site_perl/.../XML/Parser/Expat/Expat.so' 
for module XML::Parser::Expat: dynamic linker: /usr/local/bin/perl:
 libexpat.so.0 is NEEDED, but object does not exist at
/usr/local/lib/perl5/.../DynaLoader.pm line 200.

and you are using Apache web server, try to put into your httpd.conf

 <IfModule mod_env.c>
     PassEnv LD_LIBRARY_PATH
 </IfModule>

=item Apache is crashing with segfaults

If using SOAP::Lite (or XML::Parser::Expat) in combination with mod_perl
causes random segmentation faults in httpd processes try to configure
Apache with:

 RULE_EXPAT=no

See http://archive.covalent.net/modperl/2000/04/0185.xml for more 
details and lot of thanks to Robert Barta (rho@bigpond.net.au) for
explaining this weird behavior.

=back

=head1 DEPENDENCIES

 Crypt::SSLeay             for HTTPS/SSL
 SOAP::Lite, URI           for SOAP::Transport::HTTP::Server
 LWP::UserAgent, URI       for SOAP::Transport::HTTP::Client
 HTTP::Daemon              for SOAP::Transport::HTTP::Daemon
 Apache, Apache::Constants for SOAP::Transport::HTTP::Apache

=head1 SEE ALSO

 See ::CGI, ::Daemon and ::Apache for implementation details.
 See examples/server/soap.cgi as SOAP::Transport::HTTP::CGI example.
 See examples/server/soap.daemon as SOAP::Transport::HTTP::Daemon example.
 See examples/My/Apache.pm as SOAP::Transport::HTTP::Apache example.

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
