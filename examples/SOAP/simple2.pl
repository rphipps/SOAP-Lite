#!perl -w
#!d:\perl\bin\perl.exe 

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

# using autodispatch feature
use SOAP::Lite +autodispatch => 
  uri => 'http://simon.fell.com/calc',
  proxy => 'http://soap.4s4c.com/ssss4c/soap.asp'
;

print doubler([10,20,30,50,100])->[2];                             
