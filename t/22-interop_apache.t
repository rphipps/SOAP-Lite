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
    ref $res ? warn(join " ", "SOAP FAULT:", $res->faultdetail || $res->faultstring, "\n") 
             : warn(join " ", "TRANSPORT ERROR:", $soap->transport->status, "\n");
    return new SOAP::SOM;
  }
;

my($a, $s, $r);

# ------------------------------------------------------
use SOAP::Test;

$s = SOAP::Lite->uri('http://something/somewhere')->proxy('http://services.xmethods.net:80/soap/servlet/rpcrouter')->on_fault(sub{});
eval { $s->transport->timeout($SOAP::Test::TIMEOUT = $SOAP::Test::TIMEOUT) };
$r = $s->test_connection;

unless ($s->transport->is_success || $s->transport->status =~ /Internal Server Error/i) {
  print "1..0 # Skip: ", $s->transport->status, "\n"; exit;
}
# ------------------------------------------------------

plan tests => 8;

{
# XMethod's JavaSOAP server (http://xmethods.com/detail.html?id=11)
  print "XMethod's JavaSOAP server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('urn:xmethodsInterop')
    -> proxy('http://services.xmethods.net:80/soap/servlet/rpcrouter')
  ;

  $a = 'SOAP::Lite';
  $r = $s->echoString($a)->result;
  ok($r eq $a); 

  $a = ['a', 'b'];
  $r = $s->echoStringArray($a)->result;
  ok(ref $r && join('', @$r) eq join('', @$a)); 

  $a = 11;
  $r = $s->echoInteger($a)->result;
  ok($r == $a); 

  $a = [1, 3, 5];
  $r = $s->echoIntegerArray($a)->result;
  ok(ref $r && join('', @$r) == join('', @$a)); 

  $a = 11.02;
  $r = $s->echoFloat($a)->result;
  ok($r == $a); 

  $a = [1.1, 3.3, 5.5];
  $r = $s->echoFloatArray($a)->result;
  ok(ref $r && join('', @$r) eq join('', @$a)); 

  $a = {varString => 'b', varInt => 2, varFloat => 95.7};
  $r = $s->echoStruct(
    SOAP::Data->type('xx:SOAPStruct' => $a)
              ->attr({'xmlns:xx' => 'http://www.xmethods.com/services'})
  )->result;
  ok(ref $r && join('', sort values %$r) eq join('', sort values %$a)); 

  $a = [
    {varString => 'b', varInt => 2, varFloat => 95.7}, 
    {varString => 'c', varInt => 3, varFloat => 85.7},
    {varString => 'd', varInt => 4, varFloat => 75.7},
  ];
  $r = $s->echoStructArray(SOAP::Data->attr({'xmlns:xx' => 'http://www.xmethods.com/services'} =>
      [map {SOAP::Data->type('xx:SOAPStruct' => $_)    
                      ->attr({'xmlns:xx' => 'http://www.xmethods.com/services'}) 
           } @$a])
  )->result;
  ok(ref $r && join('', map { sort values %$_ } @$r) eq join('', map { sort values %$_ } @$a)); 
}
