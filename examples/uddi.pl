#!/usr/bin/perl

use UDDI::Lite +autodispatch =>
  proxy => 'http://test.uddi.microsoft.com/inquire',
;

my $list = find_business(name => old);
my $bis = $list->businessInfos;
for ($bis->businessInfo) {
  my $s = $_->serviceInfos->serviceInfo;
  print $s->Name, ' ', $s->BusinessKey, "\n";
}
