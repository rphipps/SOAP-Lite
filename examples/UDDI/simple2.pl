#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use UDDI::Lite 
  import => ['UDDI::Data'], 
  import => ['UDDI::Lite'],
  proxy => 'http://uddi.microsoft.com/inquire'
;

print find_business(name('microsoft'))
  -> businessInfos->businessInfo->serviceInfos->serviceInfo->name;                         
