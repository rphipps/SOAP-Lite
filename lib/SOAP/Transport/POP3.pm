# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::POP3.pm,v 0.40 2000/10/15 18:20:55 $
#
# ======================================================================

package SOAP::Transport::POP3;

use strict;
use vars qw($VERSION);
$VERSION = '0.40';

# ======================================================================

package SOAP::Transport::POP3::Server;

use Carp;
use SOAP::Lite;
use vars qw(@ISA $AUTOLOAD);
@ISA = qw(SOAP::Server);

sub new { eval "use Net::POP3; use MIME::Parser; use URI::Escape"; die if $@;
  my $self = shift;
  my $class = ref($self) || $self;
    
  unless (ref $self) {
    $self = bless $class->SUPER::new() => $class;
    my($server, $auth) = reverse split /@/, URI::Escape::uri_unescape(shift);
    $self->{_pop3server} = Net::POP3->new($server) or croak "Can't connect to $server: $!";
    $self->{_pop3server}->login(split /:/, $auth) or croak "Can't authenticate to $server"
      if defined $auth;
  }

  if (@_) {
    my %parameters = @_;
    foreach (grep {defined $parameters{$_}} keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }
  return $self;
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->{_pop3server}->$method(@_) };
  goto &$AUTOLOAD;
}

sub handle {
  my $self = shift->new;
  my $messages = $self->list or return;
  foreach my $msgid (keys %$messages) {
    my $entity = MIME::Parser->new(output_to_core => 'ALL')
      -> parse_data($self->get($msgid)) or next;

    next unless $entity->head->get('Content-type') =~ m!^text/xml$!;
    $self->action($entity->head->get('SOAPAction'));
    $self->SUPER::handle($entity->bodyhandle->as_string);
  } continue {
    $self->delete($msgid);
  }
  return scalar keys %$messages;
}

sub make_fault { return }

# ======================================================================

1;
