#!perl -w
#!d:\perl\bin\perl.exe 

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

use SOAP::Lite;

# object interface
my $service = SOAP::Lite
  -> service('http://www.xmethods.net/sd/StockQuoteService.wsdl');

print $service->getQuote('MSFT'), "\n";
