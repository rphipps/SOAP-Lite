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
    ref $res ? warn(join " ", "SOAP FAULT:", $res->faultdetail, "\n") 
             : warn(join " ", "TRANSPORT ERROR:", $soap->transport->status, "\n");
    return new SOAP::SOM;
  }
;

my($a, $s, $r, $serialized, $deserialized);

$s = SOAP::Lite->uri('http://something/somewhere')->proxy('http://services.xmethods.net/soap')->on_fault(sub{});
eval { $s->transport->timeout(3) };
$r = $s->test_connection;

unless ($s->transport->is_success || $s->transport->status =~ /Internal Server Error/i) {
  print "1..0 # Skip: ", $s->transport->status, "\n"; exit;
}

plan tests => 19;

{
# Public test server with Frontier implementation (http://soap.weblogs.com/)
  print "Frontier server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('/examples')
    -> on_action(sub { sprintf '"%s"', @_ })
    -> proxy('http://superhonker.userland.com/')
  ;

  ok($s->getCurrentTime()->result); 
  ok($s->getStateName(1)->result eq 'Alabama'); 
  ok($s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s+$/); 

  $r = $s->getStateList([1,2,3,4])->result;
  ok(ref $r && $r->[0] eq 'Alabama'); 

  $r = $s->getStateStruct(SOAP::Data->type(ordered_hash => [item1 => 1, item2 => 4]))->result;
  ok(ref $r && $r->{state4} eq 'Arkansas'); 

# Public test server with XSLT implementation (http://soap.develop.com/)
  print "XSLT server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('uuid:84124454-ff27-4c41-8f21-dff5f2aa241d')
    -> namespace('env')
    -> encodingspace('enc')
    -> proxy('http://soap.develop.com/xsltwire/calculator.xslt')
  ;

  ok($s->call('icalc:Add' => SOAP::Data->name(a => 10), 
                                         SOAP::Data->name(b => 3))->result == 13 
   );

# Public test server with COM implementation (http://www.zaks.demon.co.uk/com/4s4c/)
  print "COM server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('http://simon.fell.com/calc')
    -> proxy('http://www.razorsoft.net/ssss4c/soap.asp')
  ;

  $r = $s->doubler(name SOAP::Data nums => [10,20,30,50,100])->result;
  ok(ref $r && $r->[1] == 40);

# Real server with ASP implementation (http://www.soap-wrc.com/webservices/)
  print "ASP server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('www.soap-wrc.com')
    -> proxy('http://www.soap-wrc.com/webservices/soapv11.asp')
    -> on_fault(sub{ref$_[1]?return$_[1]:return})
  ;

  import SOAP::Data 'name'; # no import by default

  $r = $s->addResource(
    name(Login => 'login'),
    name(Password => 'password'),
    name(Caption => 'caption'),
    name(Description => 'description'),
    name(URL => 'http://yahoo.com'),
  );
  ok(ref $r && $r->faultcode eq 'SOAP-ENV:Client');
  # Password should be wrong. Put yours if you have it. 
  # Remember: this is the real server

# Public server with SOAP::Lite/ApacheSOAP implementations (http://www.xmethods.com/)
  print "XMethods (SOAP::Lite/ApacheSOAP) server test(s)...\n";
  print "All connections are keep-alive\n";
  $s = SOAP::Lite                             
    -> uri('urn:xmethods-BNPriceCheck')                
    -> proxy('http://services.xmethods.net/soap/servlet/rpcrouter');

  my $isbn = '0672319225';
  $r = $s->getPrice(SOAP::Data->type(string => $isbn))->result || 0;
  print "Price for ISBN$isbn is \$$r\n";
  ok($r > 20 && $r < 60);

  $s = SOAP::Lite                             
    -> uri('urn:xmethods-CurrencyExchange')                
    -> proxy('http://services.xmethods.net/soap');

  $r = $s->getRate(SOAP::Data->name(country1 => 'England'), 
                   SOAP::Data->name(country2 => 'Japan'))
         ->result;
  print "Currency rate for England/Japan is $r\n";
  ok($r > 1);

  $s = SOAP::Lite                             
    -> uri('urn:xmethods-delayed-quotes')                
    -> proxy('http://services.xmethods.net/soap');

  $r = $s->getQuote('MSFT')->result;
  print "Quote for MSFT symbol is $r\n";
  ok($r > 1);

  ok(SOAP::Lite
    -> uri('urn:xmethods-DomainChecker')                
    -> proxy('http://services.xmethods.net/soap')
    -> checkDomain('yahoo.com')
    -> result eq 'unavailable');

  ok(SOAP::Lite
    -> uri('urn:xmethods-CATraffic')                
    -> proxy('http://services.xmethods.net/soap/servlet/rpcrouter')
    -> getTraffic(type SOAP::Data string => 101)
    -> result =~ /US 101/);

  ok(SOAP::Lite
    -> uri('urn:xmethods-Temperature')                
    -> proxy('http://services.xmethods.net/soap/servlet/rpcrouter')
    -> getTemp(type SOAP::Data string => 64151)
    -> result =~ /\./);

  ok(SOAP::Lite
    -> uri('urn:xmethodsSoapPing')                
    -> proxy('http://services.xmethods.net/perl/soaplite.cgi')
    -> pingHost(name SOAP::Data hostname => 'www.yahoo.com')
    -> result == 1);

  print "BabelFish translator server test(s)...\n";
  ok(SOAP::Lite                             
    -> uri('urn:xmethodsBabelFish')                
    -> proxy('http://services.xmethods.net/perl/soaplite.cgi')
    -> BabelFish(SOAP::Data->name(translationmode => 'en_it'), 
                 SOAP::Data->name(sourcedata => 'I want to work'))
    -> result =~ /^Desidero lavorare$/);

  print "DevelopMentor's Perl server test(s)...\n";
  ok(SOAP::Lite                             
    -> uri('urn:soap-perl-test')                
    -> proxy('http://soapl.develop.com/soap?class=SPTest')
    -> add(SOAP::Data->name(a => 3), SOAP::Data->name(b => 4))
    -> result == 7);

# Public server with Microsoft implementations (http://beta.search.microsoft.com/search/MSComSearchService.asmx)
  print "Microsoft's server test(s)...\n";
  ok(SOAP::Lite 
    -> uri('http://tempuri.org/')
    -> proxy('http://beta.search.microsoft.com/search/MSComSearchService.asmx')
    -> on_action(sub{join'',@_})
    -> GetVocabulary(SOAP::Data->name('~:Query' => 'something_very_unusual'))
    -> valueof('//Found') eq 'false');

  ok(SOAP::Lite 
    -> uri('http://tempuri.org/')
    -> proxy('http://beta.search.microsoft.com/search/MSComSearchService.asmx')
    -> on_action(sub{join'',@_})
    -> GetBestBets(SOAP::Data->name('~:Query' => 'data'))
    -> result -> {VocabularyLastcache} =~ /T/);

}
