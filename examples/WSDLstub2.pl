#!/bin/env perl 
#!d:\perl\bin\perl.exe 

# stub interface (created with stubmaker)
use StockQuoteService;

print StockQuoteService->getQuote('MSFT'), "\n";
