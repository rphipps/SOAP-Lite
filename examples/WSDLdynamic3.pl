#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite;

# object interface
my $service = SOAP::Lite
  -> schema('http://www.xmethods.net/sd/StockQuoteService.wsdl');

print $service->getQuote('MSFT'), "\n";
