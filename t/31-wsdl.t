#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

use SOAP::Lite
  on_fault => sub {
    my $soap = shift;
    my $res = shift;
    ref $res ? warn(join "\n", "--- SOAP FAULT ---", $res->faultcode, $res->faultstring, $res->faultdetail, '') 
             : warn(join "\n", "--- TRANSPORT ERROR ---", $soap->transport->status, '');
    return new SOAP::SOM;
  }
;

my($a, $s, $r, $serialized, $deserialized);

# ------------------------------------------------------
use SOAP::Test;

$s = SOAP::Lite->uri('http://something/somewhere')->proxy('http://services.xmethods.net/soap')->on_fault(sub{});
eval { $s->transport->timeout($SOAP::Test::TIMEOUT = $SOAP::Test::TIMEOUT) };
$r = $s->test_connection;

unless ($s->transport->is_success || $s->transport->status =~ /Internal Server Error/i) {
  print "1..0 # Skip: ", $s->transport->status, "\n"; exit;
}
# ------------------------------------------------------

plan tests => 2;

{
# Service description (WSDL) (http://www.xmethods.com/)
  print "Service description (WSDL) test(s)...\n";
  $s = SOAP::Lite
    -> schema('http://www.xmethods.net/sd/StockQuoteService.wsdl');

  ok($s->getQuote('MSFT') > 1);
  ok(SOAP::Lite
    -> schema('http://www.xmethods.net/sd/StockQuoteService.wsdl')
    -> getQuote('MSFT')  > 1);
}