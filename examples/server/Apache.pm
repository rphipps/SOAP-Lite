package SOAP::Apache;

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

use SOAP::Transport::HTTP;

my $server = SOAP::Transport::HTTP::Apache
  # specify list of objects-by-reference here 
  -> objects_by_reference(qw(My::PersistentIterator My::SessionIterator My::Chat))
  # specify path to My/Examples.pm here
  -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
; 

sub handler { $server->handler(@_) }

1;