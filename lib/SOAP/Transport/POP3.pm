# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::POP3.pm,v 0.46 2001/01/31 16:30:24 $
#
# ======================================================================

package SOAP::Transport::POP3;

use strict;
use vars qw($VERSION);
$VERSION = '0.46';

use Net::POP3; 
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
    $self->SUPER::handle($self->get($msgid));
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

=head1 SYNOPSIS

  use SOAP::Transport::POP3;

  my $server = SOAP::Transport::POP3::Server
    -> new('pop.mail.server')
    # if you want to have all in one place
    # -> new('user:password@pop.mail.server') 
    # specify list of objects-by-reference here 
    -> objects_by_reference(qw(My::PersistentIterator My::SessionIterator My::Chat))
    # specify path to My/Examples.pm here
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
  ;
  # you don't need to use next line if you specified your password in new()
  $server->login('user' => 'password') or die "Can't authenticate to SMTP server\n";

  # handle will return number of processed mails
  # you can organize loop if you want
  $server->handle while sleep 10;

=head1 DESCRIPTION

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
