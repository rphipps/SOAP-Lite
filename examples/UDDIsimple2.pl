#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use UDDI::Lite 
  import => ['UDDI::Data' => ':all'], 
  import => ['UDDI::Lite' => ':inquire'],
  proxy => 'http://test.uddi.microsoft.com/inquire'
;

print find_business(name('micro'))
  -> businessInfos->businessInfo->serviceInfos->serviceInfo->name;                         
