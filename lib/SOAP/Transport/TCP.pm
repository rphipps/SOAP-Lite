# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::TCP.pm,v 0.42 2000/11/14 23:14:18 $ 
#
# ======================================================================

package SOAP::Transport::TCP;

use strict;
use vars qw($VERSION);
$VERSION = '0.42';

use IO::Socket;

# ======================================================================

package SOAP::Transport::TCP::Client;

use Carp;
use vars qw(@ISA);
@ISA = qw(SOAP::Client);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
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

  my $sock = new IO::Socket::INET (
    PeerAddr => $server, PeerPort => $port, Proto => $proto, %$self
  ) or croak "Can't open socket: $!";

  local $!;
  $sock->autoflush;
  print $sock $envelope;

  my $code = $!;

  $self->code($code);
  $self->message($code);
  $self->is_success(!defined $code || $code eq '');
  $self->status($code);

  return;
}

# ======================================================================

package SOAP::Transport::TCP::Server;

use Carp;
use vars qw($AUTOLOAD @ISA);
@ISA = qw(SOAP::Server);

use SOAP::Lite;

sub DESTROY { SOAP::Trace::objects('()') }

sub new { eval "use IO::Socket"; die if $@;
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = $class->SUPER::new();
    $self->{_socket} = IO::Socket::INET->new(@_, Proto => 'tcp') 
      or croak "Can't open socket: $!";
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->{_socket}->$method(@_) };
  goto &$AUTOLOAD;
}

sub handle {
  my $self = shift->new;
  while (my $sock = $self->accept) {
    $self->SUPER::handle(join '',<$sock>);
    close($sock);
  }
}

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Transport::TCP - Server/Client side TCP support for SOAP::Lite

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
