#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use UDDI::Lite +autodispatch =>
  proxy => 'http://uddi.microsoft.com/inquire',
;

my $list = find_business(name => 'microsoft');
my $bis = $list->businessInfos;
for ($bis->businessInfo) {
  my $s = $_->serviceInfos->serviceInfo;
  print $s->name, ' ', $s->businessKey, "\n";
}
