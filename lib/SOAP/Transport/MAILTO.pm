# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::MAILTO.pm,v 0.46 2001/01/31 16:30:24 $
#
# ======================================================================

package SOAP::Transport::MAILTO;

use strict;
use vars qw($VERSION);
$VERSION = '0.46';

use MIME::Lite; 
use URI;

# ======================================================================

package SOAP::Transport::MAILTO::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Client);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
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

=head1 NAME

SOAP::Transport::MAILTO - Client side SMTP/sendmail support for SOAP::Lite

=head1 SYNOPSIS

  use SOAP::Lite;

  SOAP::Lite
    -> uri('http://soaplite.com/My/Examples')                
    -> proxy('mailto:destination.email@address', smtp => 'smtp.server', From => 'your.email', Subject => 'SOAP message')

    # or 
    # -> proxy('mailto:destination.email@address?From=your.email&Subject=SOAP%20message', smtp => 'smtp.server')

    # or if you want to send with sendmail
    # -> proxy('mailto:destination.email@address?From=your.email&Subject=SOAP%20message')

    # or if your sendmail is in undiscoverable place
    # -> proxy('mailto:destination.email@address?From=your.email&Subject=SOAP%20message', sendmail => 'command to run your sendmail')

    -> getStateName(12)
  ;

=head1 DESCRIPTION

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
