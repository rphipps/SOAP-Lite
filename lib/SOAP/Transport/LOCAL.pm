# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::LOCAL.pm,v 0.40 2000/10/15 18:20:55 $
#
# ======================================================================

package SOAP::Transport::LOCAL;

use strict;
use vars qw($VERSION);
$VERSION = '0.40';

# ======================================================================

package SOAP::Transport::LOCAL::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Server);

use SOAP::Lite;

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless SOAP::Server->new => $class;
    $self->is_success(1);     # it's difficult to fail in this module
    $self->dispatch_to(@INC);
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

  $self->on_debug->($envelope);
  my $respond = $self->SUPER::handle($envelope);
  $self->on_debug->($respond);

  $respond;
}

# ======================================================================

1;
