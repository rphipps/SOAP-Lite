#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Transport::HTTP;

SOAP::Transport::HTTP::CGI
  # specify path to My/Examples.pm here
  -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
  -> handle
;
