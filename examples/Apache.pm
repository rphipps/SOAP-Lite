package SOAP::Apache;

use SOAP::Transport::HTTP;

my $server = SOAP::Transport::HTTP::Apache
  # specify path to My/Examples.pm here
  -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
; 

sub handler { $server->handler(@_) }

1;