# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Transport::LOCAL.pm,v 0.42 2000/11/14 23:14:18 $ 
#
# ======================================================================

package SOAP::Transport::LOCAL;

use strict;
use vars qw($VERSION);
$VERSION = '0.42';

# ======================================================================

package SOAP::Transport::LOCAL::Client;

use vars qw(@ISA);
@ISA = qw(SOAP::Client SOAP::Server);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    my(@params, @methods);
    while (@_) { $class->can($_[0]) ? push(@methods, shift() => shift) : push(@params, shift) }
    $self = $class->SUPER::new(@params);
    $self->is_success(1);     # it's difficult to fail in this module
    $self->dispatch_to(@INC);
    while (@methods) { my($method, $params) = splice(@methods,0,2);
      $self->$method(ref $params eq 'ARRAY' ? @$params : $params) 
    }
  }
  return $self;
}

sub send_receive {
  my($self, %parameters) = @_;
  my($envelope, $endpoint, $action) = 
    @parameters{qw(envelope endpoint action)};

  SOAP::Trace::debug($envelope);
  my $respond = $self->SUPER::handle($envelope);
  SOAP::Trace::debug($respond);

  $respond;
}

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Transport::LOCAL - Client side no-transport support for SOAP::Lite

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
