#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use UDDI::Lite +autodispatch =>
  proxy => 'http://test.uddi.microsoft.com/inquire',
;

my $list = find_business(name => 'micro');
my $bis = $list->businessInfos;
for ($bis->businessInfo) {
  my $s = $_->serviceInfos->serviceInfo;
  print $s->name, ' ', $s->businessKey, "\n";
}
