# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::MAILTO.pm,v 0.39 2000/10/08 22:55:20 $
#
# ======================================================================

package SOAP::Transport::MAILTO;

use strict;
use vars qw($VERSION);
$VERSION = '0.39';

# ======================================================================

package SOAP::Transport::MAILTO::Client;

sub new { eval "use MIME::Lite; use URI"; die if $@;
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless {} => $class;
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
  my $uri = URI->new($endpoint);
  %parameters = ((map {URI::Escape::uri_unescape($_)} map {split/=/,$_,2} split /[&;]/, $uri->query), 
                 %{$self->parameters});

  my $msg = MIME::Lite->new(
    From       => $parameters{From},
    To         => $uri->authority,
    'Reply-To' => $parameters{'Reply-To'} || $parameters{From},
    Subject    => $parameters{Subject},
    Type       => 'text/xml',
    Encoding   => 'base64',
    Data       => $envelope,
  );
  $msg->add(SOAPAction => $action);

  $self->on_debug->($msg->as_string);
    
  MIME::Lite->send(map {exists $parameters{$_} ? ($_ => $parameters{$_}) : ()} 'smtp', 'sendmail');
  eval { local $SIG{__DIE__}; $msg->send };
  my $code = $@;

  $self->code($code);
  $self->message($code);
  $self->is_success(!defined $code || $code eq '');
  $self->status($code);

  return;
}

# ======================================================================

1;
