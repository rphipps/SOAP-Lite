# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::MAILTO.pm,v 0.41 2000/10/31 01:24:51 $
#
# ======================================================================

package SOAP::Transport::MAILTO;

use strict;
use vars qw($VERSION);
$VERSION = '0.41';

use MIME::Lite; 
use URI;

# ======================================================================

package SOAP::Transport::MAILTO::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Client);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless {@_} => $class;
    SOAP::Trace::objects('()');
  }
  return $self;
}

sub send_receive {
  my($self, %parameters) = @_;
  my($envelope, $endpoint, $action) = 
    @parameters{qw(envelope endpoint action)};

  $endpoint ||= $self->endpoint;
  my $uri = URI->new($endpoint);
  %parameters = (%$self, map {URI::Escape::uri_unescape($_)} map {split/=/,$_,2} split /[&;]/, $uri->query);

  my $msg = MIME::Lite->new(
    From       => $parameters{From},
    To         => $uri->to,
    'Reply-To' => $parameters{'Reply-To'} || $parameters{From},
    Subject    => $parameters{Subject},
    Type       => 'text/xml',
    Encoding   => 'base64',
    Data       => $envelope,
  );
  $msg->add(SOAPAction => $action);

  SOAP::Trace::transport($msg);
  SOAP::Trace::debug($msg->as_string);
    
  MIME::Lite->send(map {exists $parameters{$_} ? ($_ => $parameters{$_}) : ()} 'smtp', 'sendmail');
  eval { local $SIG{__DIE__}; $msg->send };
  (my $code = $@) =~ s/ at .*\n//;

  $self->code($code);
  $self->message($code);
  $self->is_success(!defined $code || $code eq '');
  $self->status($code);

  return;
}

# ======================================================================

1;
