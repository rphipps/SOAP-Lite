#!perl -w
#!d:\perl\bin\perl.exe 

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

# stub interface (created with stubmaker.pl)
use StockQuoteService ':all';

print getQuote('MSFT'), "\n";
