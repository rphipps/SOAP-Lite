#!/bin/env perl 
#!d:\perl\bin\perl.exe 

# stub interface (created with stubmaker)
use StockQuoteService;

my $service = StockQuoteService->new;
print $service->getQuote('MSFT'), "\n";
