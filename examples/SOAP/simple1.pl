#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite;

# simple object interface
print SOAP::Lite 
  -> uri('http://simon.fell.com/calc')
  -> proxy('http://www.razorsoft.net/ssss4c/soap.asp')
  -> doubler([10,20,30,50,100])
  -> result ->[1];
