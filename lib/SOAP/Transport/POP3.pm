# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::POP3.pm,v 0.44 2000/12/12 23:52:12 $
#
# ======================================================================

package SOAP::Transport::POP3;

use strict;
use vars qw($VERSION);
$VERSION = '0.44';

use Net::POP3; 
use MIME::Parser; 
use URI::Escape; 

# ======================================================================

package SOAP::Transport::POP3::Server;

use Carp ();
use SOAP::Lite;
use vars qw(@ISA $AUTOLOAD);
@ISA = qw(SOAP::Server);

sub new {
  my $self = shift;
    
  unless (ref $self) {
    my $class = ref($self) || $self;
    my($server, $auth) = reverse split /@/, URI::Escape::uri_unescape(shift);
    $self = $class->SUPER::new(@_);
    $self->{_pop3server} = Net::POP3->new($server) or Carp::croak "Can't connect to $server: $!";
    $self->{_pop3server}->login(split /:/, $auth) or Carp::croak "Can't authenticate to $server"
      if defined $auth;
  }
  return $self;
}

sub AUTOLOAD {
  my $method = substr($AUTOLOAD, rindex($AUTOLOAD, '::') + 2);
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

__END__

=head1 NAME

SOAP::Transport::POP3 - Server side POP3 support for SOAP::Lite

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
