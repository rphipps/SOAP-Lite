#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite;

# object interface
print SOAP::Lite
  -> schema('http://www.xmethods.net/sd/StockQuoteService.wsdl')
  -> getQuote('MSFT'), "\n";
