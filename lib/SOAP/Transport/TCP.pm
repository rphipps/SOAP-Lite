# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::TCP.pm,v 0.46 2001/01/31 16:30:24 $
#
# ======================================================================

package SOAP::Transport::TCP;

use strict;
use vars qw($VERSION);
$VERSION = '0.46';

use IO::Socket;

# ======================================================================

package SOAP::Transport::TCP::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Client);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    my(@params, @methods);
    while (@_) { $class->can($_[0]) ? push(@methods, shift() => shift) : push(@params, shift) }
    $self = bless {@params} => $class;
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

  my($proto, $server, $port) = split /:/, $endpoint ||= $self->endpoint;

  local $^W; local $@;
  my $sock;
  $sock = new IO::Socket::INET (
    PeerAddr => $server, PeerPort => $port, Proto => $proto, %$self
  ) 
   and $sock->autoflush
   and print $sock $envelope          # write 
   and shutdown($sock => 1)           # signal stop writing
   and my $result = join '', <$sock>; # read response

  my $code = $@;

  $self->code($code);
  $self->message($code);
  $self->is_success(!defined $code || $code eq '');
  $self->status($code);

  return $result;
}

# ======================================================================

package SOAP::Transport::TCP::Server;

use Carp ();
use vars qw($AUTOLOAD @ISA);
@ISA = qw(SOAP::Server);

use SOAP::Lite;

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new();
    $self->{_socket} = IO::Socket::INET->new(@_, Proto => 'tcp') 
      or Carp::croak "Can't open socket: $!";
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub AUTOLOAD {
  my $method = substr($AUTOLOAD, rindex($AUTOLOAD, '::') + 2);
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->{_socket}->$method(@_) };
  goto &$AUTOLOAD;
}

sub handle {
  my $self = shift->new;
  while (my $sock = $self->accept) {
    print $sock $self->SUPER::handle(join '', <$sock>);
    close($sock);
  }
}

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Transport::TCP - Server/Client side TCP support for SOAP::Lite

=head1 SYNOPSIS

  use SOAP::Transport::TCP;

  my $daemon = SOAP::Transport::TCP::Server
    -> new (LocalAddr => 'localhost', LocalPort => 82, Listen => 5, Reuse => 1)
    -> objects_by_reference(qw(My::PersistentIterator My::SessionIterator My::Chat))
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
  ;
  print "Contact to SOAP server at ", join(':', $daemon->sockhost, $daemon->sockport), "\n";
  $daemon->handle;

=head1 DESCRIPTION

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
