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
    ref $res ? warn(join "\n", "--- SOAP FAULT ---", $res->faultcode, $res->faultstring, '') 
             : warn(join "\n", "--- TRANSPORT ERROR ---", $soap->transport->status, '');
    return new SOAP::SOM;
  }
;

my($a, $s, $r, $serialized, $deserialized);

# ------------------------------------------------------
use SOAP::Test;

$s = SOAP::Lite->uri('http://something/somewhere')->proxy('http://services.xmethods.net/soap/servlet/rpcrouter')->on_fault(sub{});
eval { $s->transport->timeout($SOAP::Test::TIMEOUT = $SOAP::Test::TIMEOUT) };
$r = $s->test_connection;

unless (defined $r && defined $r->envelope) {
  print "1..0 # Skip: ", $s->transport->status, "\n"; 
  exit;
}
# ------------------------------------------------------

plan tests => 9;

{
# Service description (WSDL) (http://www.xmethods.net/)
  print "Service description (WSDL) test(s)...\n";
  $s = SOAP::Lite
    -> service('http://www.xmethods.net/sd/StockQuoteService.wsdl');

  ok($s->getQuote('MSFT') > 1);
  ok(SOAP::Lite
    -> service('http://www.xmethods.net/sd/StockQuoteService.wsdl')
    -> getQuote('MSFT') > 1);

  # WSDL with <import> element and multiple ports (non-SOAP bindings)
  ok(SOAP::Lite
    -> service('http://www.xmethods.net/sd/StockQuoteImport.wsdl')
    -> getQuote('MSFT') > 1);

  my $schema = SOAP::Schema
    -> schema('http://www.xmethods.net/sd/StockQuoteService.wsdl')
    -> parse('StockQuoteService');

  foreach (keys %{$schema->services}) {
    eval { $schema->stub($_) } or die;
  }

  print "Service description static stub test(s)...\n";
  ok(StockQuoteService->getQuote('MSFT') > 1);

  ok(defined StockQuoteService->self);

  ok(StockQuoteService->self->call);

  print "Service description static stub with import test(s)...\n";
  StockQuoteService->import(':all');

  ok(getQuote('MSFT') > 1);

  ok(defined StockQuoteService->self);

  ok(StockQuoteService->self->call);
}
