#!perl -w
#!d:\perl\bin\perl.exe 

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

BEGIN { warn "Started...\n" }

# import interface. All methods from loaded service are imported by default
use SOAP::Lite
  service => 'http://www.xmethods.net/sd/StockQuoteService.wsdl',
  # service => 'file:/your/local/path/StockQuoteService.wsdl',
  # service => 'file:./StockQuoteService.wsdl',
;

warn "Loaded...\n";
print getQuote('MSFT'), "\n";
