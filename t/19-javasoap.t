#!./perl

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

my $proxy = 'http://localhost:8080/examples/rpcrouter/rpcrouter.jsp';

$s = SOAP::Lite->uri('http://something/somewhere')->proxy($proxy)->on_fault(sub{});
eval { $s->transport->timeout(3) };
$r = $s->test_connection;

unless ($s->transport->is_success || $s->transport->status =~ /Internal Server Error/i) {
  print "1..0 # Skip: ", $s->transport->status, "\n"; exit;
}

plan tests => 2;

{
# Local server with Apache SOAP (http://xml.apache.org/soap)
  print "Apache SOAP server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('urn:xmltoday-delayed-quotes')
    -> proxy($proxy)
  ;

  ok($s->getQuote('MSFT')->result > 0);
  ok($s->getQuote(SOAP::Data->name(symbol => 'MSFT'))->result > 0);
}
