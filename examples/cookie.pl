#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite;

$r = SOAP::Lite 
  -> uri('urn:lemurlabs-Fortune')
  -> proxy('http://www.lemurlabs.com/rpcrouter')
  -> getFortuneByDictionary('work')
  -> result || '';

print $r && ref($r = SOAP::Deserializer->deserialize($r)) && ($r = $r->valueof('//fortune') || '') 
  ? "Your fortune cookie:\n$r" : "No fortune cookies for you today\n";
