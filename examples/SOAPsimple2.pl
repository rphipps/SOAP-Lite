#!/bin/env perl 
#!d:\perl\bin\perl.exe 

# using autodispatch feature
use SOAP::Lite +autodispatch => 
  uri => 'http://simon.fell.com/calc',
  proxy => 'http://www.razorsoft.net/ssss4c/soap.asp'
;

print doubler([10,20,30,50,100])->[2];                             
