#!/bin/env perl 
#!d:\perl\bin\perl.exe 

BEGIN { warn "Started...\n" }

# import interface. All methods from loaded schema imported by default
use SOAP::Lite
  schema => 'http://www.xmethods.net/sd/StockQuoteService.wsdl',
  # schema => 'file:/your/local/path/StockQuoteService.wsdl',
  # schema => 'file:./StockQuoteService.wsdl',
;

warn "Loaded...\n";
print getQuote('MSFT'), "\n";
