# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..127\n"; }
END {print "not ok 1\n" unless $loaded;}
use SOAP::Lite;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

use ExtUtils::MakeMaker qw(prompt);

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
my $test = 1;

{ # check serialization
  print "Arrays, structs, refs serialization test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(1, [1,2], {a=>3}, \4);

  $test++; print $serialized =~ 
    m!<c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\1><SOAP-ENV:Array(?: xsi:type="SOAP-ENV:Array"| SOAP-ENC:arrayType="xsd:int\[2\]"){2}><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\2><c-gensym\2 xsi:type="xsd:int">2</c-gensym\2></SOAP-ENV:Array><SOAP-ENV:Struct xsi:type="SOAP-ENV:SOAPStruct"><a xsi:type="xsd:int">3</a></SOAP-ENV:Struct><c-gensym(\d+)><c-gensym(\d+) xsi:type="xsd:int">4</c-gensym\4></c-gensym\3>!
    ? "ok $test\n" : "not ok $test\n";
}  

{ # check simple circlular references
  print "Simple circlular references (\$a=\\\$a) serialization test(s)...\n";

  $a = \$a;
  $serialized = join '', SOAP::Serializer->serialize($a);

  $test++; print $serialized =~ 
    m!<c-gensym(\d+) id="ref-0x(\w+)"><c-gensym(\d+) href="#ref-0x\2"/></c-gensym\1>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check complex circlular references
  print "Complex circlular references serialization test(s)...\n";

  $a = { a => 1 }; my $b = { b => $a }; $a->{a} = $b;
  $serialized = join '', SOAP::Serializer->serialize($a);

  $test++; print $serialized =~ 
    m!<SOAP-ENV:Struct(?: xsi:type="SOAP-ENV:SOAPStruct"| id="ref-0x(\w+)"){2}><a(?: xsi:type="SOAP-ENV:SOAPStruct"| id="ref-0x\w+"){2}><b(?: xsi:type="SOAP-ENV:SOAPStruct"| href="#ref-0x\1"){2}/></a></SOAP-ENV:Struct>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check multirefs
  print "Multireferences serialization test(s)...\n";

  $a = 1; my $b = \$a;

  $serialized = join '', SOAP::Serializer->new(multirefinplace=>1)->serialize($b, $b);
  $test++; print $serialized =~ 
    m!<c-gensym(\d+) id="ref-0x(\w+)"><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\3></c-gensym\1><c-gensym\d+ href="#ref-0x\2"/>!
    ? "ok $test\n" : "not ok $test\n";
  

  $serialized = join '', SOAP::Serializer->serialize($b, $b);
  $test++; print $serialized =~ 
    m!<c-gensym\d+ href="#ref-0x(\w+)"/><c-gensym\d+ href="#ref-0x\1"/><c-gensym(\d+) id="ref-0x\1"><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\3></c-gensym\2>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check base64, XML encoding
  print "base64, XML encoding test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize("\0\1\2\3   \4\5\6", '<123>&amp</123>');

  $test++; print $serialized =~ 
    m!<c-gensym(\d+) xsi:type="SOAP-ENC:base64">AAECAyAgIAQFBg==</c-gensym\1><c-gensym(\d+) xsi:type="xsd:string">&lt;123>&amp;amp&lt;/123></c-gensym\2>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check objects and SOAP::Data 
  print "Blessed references and SOAP::Data encoding test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(SOAP::Data->uri('some_urn' => bless {a => 1} => 'ObjectType'));

  $test++; print $serialized =~ 
    m!<namesp(\d+):ObjectType xsi:type="namesp\1:ObjectType" xmlns:namesp\1="some_urn"><a xsi:type="xsd:int">1</a></namesp\1:ObjectType>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check root, mustUnderstand
  print "root and mustUnderstand attributes with SOAP::Data test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(SOAP::Data->root(1 => 1)->name('rootandunderstand')->mustUnderstand(1));

  $test++; print $serialized =~ 
    m!<rootandunderstand( xsi:type="xsd:int"| SOAP-ENV:mustUnderstand="1"| SOAP-ENV:root="1"){3}>1</rootandunderstand>!
    ? "ok $test\n" : "not ok $test\n";
}

{ # check deserialization of envelope with result
  print "Deserialization of envelope with result test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<m:doublerResponse xmlns:m="http://simon.fell.com/calc">
<nums xsi:type="SOAP-ENC:Array" SOAP-ENC:arrayType="xsd:int[5]">
<item xsi:type="xsd:int">20</item>
<item xsi:type="xsd:int">40</item>
<item xsi:type="xsd:int">60</item>
<item xsi:type="xsd:int">100</item>
<item xsi:type="xsd:int">200</item>
</nums>
</m:doublerResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  $test++; print $deserialized->result->[2] == 60 ? "ok $test\n" : "not ok $test\n";
  $test++; print ref $deserialized->body eq 'Body' ? "ok $test\n" : "not ok $test\n";
}

{ # check deserialization of envelope with fault
  print "Deserialization of envelope with fault test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsd="http://www.w3.org/1999/XMLSchema" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
<SOAP-ENV:Body>
<SOAP-ENV:Fault><faultcode>SOAP-ENV:Client</faultcode><faultstring>Application Error</faultstring><detail>Invalid Password</detail></SOAP-ENV:Fault></SOAP-ENV:Body></SOAP-ENV:Envelope>
');

  $test++; print $deserialized->faultcode eq 'SOAP-ENV:Client' ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->faultstring eq 'Application Error' ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->faultdetail eq 'Invalid Password' ? "ok $test\n" : "not ok $test\n";
}

{ # check deserialization of circular references
  print "Deserialization of circular references test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Struct id="ref-0xb61350"><a id="ref-0xb61374"><b href="#ref-0xb61350"/></a></SOAP-ENV:Struct>
');

  $test++; print ref $deserialized->valueof('/Struct') eq ref $deserialized->valueof('//b') ? "ok $test\n" : "not ok $test\n";
}

{ # check SOAP::SOM 
  print "SOM test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<m:doublerResponse xmlns:m="http://simon.fell.com/calc">
<nums>
<item1 xsi:type="xsd:int">20</item1>
<item1 xsi:type="xsd:int">40</item1>
<item2 xsi:type="xsd:int">60</item2>
<item2 xsi:type="xsd:int">100</item2>
<item3 xsi:type="xsd:int">200</item3>
</nums>
</m:doublerResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  # should return STRING '/Envelope/Body/[1]/[1]'
  my $result = SOAP::SOM::result; 
  $test++; print $deserialized->valueof("$result/[1]") == 20 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof("$result/[3]") == 60 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof("$result/[5]") == 200 ? "ok $test\n" : "not ok $test\n";

  # match should return true/false in boolean context (and object ref otherwise)
  $test++; print $deserialized->match('aaa') ? "not ok $test\n" : "ok $test\n";

  # should return same string as above
  $test++; print $deserialized->match(SOAP::SOM->result) ? "ok $test\n" : "not ok $test\n";

  $test++; print $deserialized->valueof('[1]') == 20 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof('[3]') == 60 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof('[5]') == 200 ? "ok $test\n" : "not ok $test\n";

  $deserialized->match('//Body/[1]/[1]'); # match path and change current node on success
  $test++; print $deserialized->valueof('[1]') == 20 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof('[3]') == 60 ? "ok $test\n" : "not ok $test\n";
  $test++; print $deserialized->valueof('[5]') == 200 ? "ok $test\n" : "not ok $test\n";
}

{ # check output parameters   
  print "Output parameters test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
  <mehodResponse>
    <res1>name1</res1>
    <res2>name2</res2>
    <res3>name3</res3>
  </mehodResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');
  my @paramsout = $deserialized->paramsout;

  $test++; print $paramsout[0] eq 'name2' && $paramsout[1] eq 'name3'
    ? "ok $test\n" : "not ok $test\n";
}

{ # check nonqualified namespace   
  print "Nonqualified namespace test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('
<SOAP-ENV:Envelope  xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<doublerResponse xmlns="http://simon.fell.com/calc">
<nums xsi:type="SOAP-ENC:Array" SOAP-ENC:arrayType="xsd:int[5]">
<item xsi:type="xsd:int">20</item>
<item xsi:type="xsd:int">40</item>
<item xsi:type="xsd:int">60</item>
<item xsi:type="xsd:int">100</item>
<item xsi:type="xsd:int">200</item>
</nums>
</doublerResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  $test++; print $deserialized->namespaceuriof(SOAP::SOM::method) eq 'http://simon.fell.com/calc'
    ? "ok $test\n" : "not ok $test\n";

  $test++; print $deserialized->namespaceuriof('//doublerResponse') eq 'http://simon.fell.com/calc'
    ? "ok $test\n" : "not ok $test\n";

}

{ # check for serialization with SOAPStruct (for interoperability with ApacheSOAP)
  print "Serialization w/out SOAPStruct test(s)...\n";

  $a = { a => 1 };

  $test++; print SOAP::Serializer->serialize($a) =~ m!SOAPStruct! 
    ? "ok $test\n" : "not ok $test\n";

  $test++; print SOAP::Serializer->autotype(0)->serialize($a) !~ m!SOAPStruct! 
    ? "ok $test\n" : "not ok $test\n";
}

{ # check serialization/deserialization of simple types  
  print "Serialization/deserialization of simple types test(s)...\n";

  $a = 'abc234xyz';

  $serialized = SOAP::Serializer->serialize(SOAP::Data->type(hex => $a));

  $test++; print $serialized =~ 
    m!<c-gensym(\d+) xsi:type="xsd:hex">61626332333478797a</c-gensym(\d+)>!
    ? "ok $test\n" : "not ok $test\n";
  
  $test++; print SOAP::Deserializer->deserialize($serialized)->valueof('/') eq $a 
    ? "ok $test\n" : "not ok $test\n";

  $a = <<'EOBASE64';

qwertyuiop[]asdfghjkl;'zxcvbnm,./
QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?
`1234567890-=\
~!@#$%^&*()_+|

EOBASE64

  $serialized = SOAP::Serializer->serialize($a);

  $test++; print $serialized =~ 
    m!<c-gensym(\d+) xsi:type="SOAP-ENC:base64">CnF3ZXJ0eXVpb3BbXWFzZGZnaGprbDsnenhjdmJubSwuLwpRV0VSVFlVSU9Qe31BU0RGR0hKS0w6IlpYQ1ZCTk08Pj8KYDEyMzQ1Njc4OTAtPVwKfiFAIyQlXiYqKClfK3wKCg==</c-gensym\1>!
    ? "ok $test\n" : "not ok $test\n";
  
  $test++; print SOAP::Deserializer->deserialize($serialized)->valueof('/') eq $a 
    ? "ok $test\n" : "not ok $test\n";
}

{ # check serialization/deserialization of blessed reference  
  print "Serialization/deserialization of blessed reference test(s)...\n";

  $a = SOAP::Deserializer->deserialize(
    SOAP::Serializer->serialize(bless {a => 1} => 'SOAP::Lite')
  )->valueof('/');
  $test++; print ref $a eq 'SOAP::Lite' && UNIVERSAL::isa($a => 'HASH') ? "ok $test\n" : "not ok $test\n";

  $a = SOAP::Deserializer->deserialize(
    SOAP::Serializer->serialize(bless [a => 1] => 'SOAP::Lite')
  )->valueof('/');
  $test++; print ref $a eq 'SOAP::Lite' && UNIVERSAL::isa($a => 'ARRAY') ? "ok $test\n" : "not ok $test\n";
}

{ # check serialization/deserialization of undef/empty elements  
  print "Serialization/deserialization of undef/empty elements test(s)...\n";

  { local $^W; # suppress warnings
  $a = undef;
  $serialized = SOAP::Serializer->serialize(SOAP::Data->type(negativeInteger => $a));
  $test++; print SOAP::Deserializer->deserialize($serialized)->valueof('/') == $a
    ? "ok $test\n" : "not ok $test\n";

  my $type = 'nonstandardtype';
  $serialized = SOAP::Serializer->serialize(SOAP::Data->type($type => $a));
  $test++; print ref SOAP::Deserializer->deserialize($serialized)->valueof('/') eq $type
    ? "ok $test\n" : "not ok $test\n";
  }
}

{ # check header serialization/deserialization   
  print "Header serialization/deserialization test(s)...\n";

  $serialized = SOAP::Serializer->method( # same as ->envelope(method =>
      'mymethod', 1, 2, 3, 
      SOAP::Header->name(t1 => 5)->attr({'~V:mustUnderstand' => 1}),
      SOAP::Header->name(t2 => 7)->mustUnderstand(2),
  );
  $deserialized = SOAP::Deserializer->deserialize($serialized);

  my $t1 = $deserialized->match(SOAP::SOM::header)->dataof('t1');
  my $t2 = $deserialized->dataof('t2');
  my @paramsin = $deserialized->paramsin;

  $test++; print $t2->type eq 'xsd:int' ? "ok $test\n" : "not ok $test\n";
  $test++; print $t2->mustUnderstand == 1 ? "ok $test\n" : "not ok $test\n";
  $test++; print @paramsin == 3 ? "ok $test\n" : "not ok $test\n";
}

{
  package My::PingPong;

  sub new { 
    my $self = shift;
    my $class = ref($self) || $self;
    bless {_num=>shift} => $class;
  }

  sub next {
    my $self = shift;
    $self->{_num}++;
  }

  sub value {
    my $self = shift;
    $self->{_num};
  }
}

print "
This test sends a live SOAP call to your local web server (CGI implementation) with SOAP interface. See example in SOAP::Transport::HTTP.pm.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=13; print "skipped 13 test(s)\n"; 
} else {
# Local server with Perl implementation (http://www.geocities.com/paulclinger/soap.html)
  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('http://my.own.site.com/My/Examples')                
    -> proxy('http://localhost/cgi-bin/soap.cgi')
  ;

  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/ ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  $test++; print ref $r && $r->{item2} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  $test++; print $result == $param1 && $param2->value == 24 ? "ok $test\n" : "not ok $test\n"; 

  print "Header manipulating test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  $test++; print $a->header->{my} eq '123123' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $a->headers eq '123123' ? "ok $test\n" : "not ok $test\n"; 

  print "Object autobinding and SOAP:: prefix test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/cgi-bin/soap.cgi')";

  eval { SOAP->new(1) };
  $test++; print $@ =~ /^URI is not specified/ ? "ok $test\n" : "not ok $test\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:/A/B', proxy => 'http://localhost/cgi-bin/soap.cgi')";

  # should call My::PingPong, not A::B
  my $p = My::PingPong->SOAP::new(10);
  my $next = $p->SOAP::next;
  $test++; print $p->value == $next+1 ? "ok $test\n" : "not ok $test\n";

  print "VersionMismatch test(s)...\n";

  {
    local $SOAP::Constants::NS_ENV = 'http://schemas.xmlsoap.org/new/envelope/';
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/cgi-bin/soap.cgi')
      -> on_fault(sub{})
    ;
    $r = $s->dosomething;
    $test++; print $r->faultcode eq 'VersionMismatch' ? "ok $test\n" : "not ok $test\n";
  }

  print "Parameters-by-name test(s)...\n";
  print "You can see warning about AUTOLOAD for non-method...\n" if $^W;

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'http://my.own.site.com/My/Parameters', 
        proxy => 'http://localhost/cgi-bin/soap.cgi')";

  my @parameters = (
    SOAP::Data->name(b => 222), 
    SOAP::Data->name(c => 333), 
    SOAP::Data->name(a => 111)
  );

  $test++; print byname(@parameters) eq "a=111, b=222, c=333" ? "ok $test\n" : "not ok $test\n";

  print "SOAPAction test(s)...\n";
  {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/cgi-bin/soap.cgi')
      -> on_action(sub{'""'})
    ;
    $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 

    $s-> on_action(sub{'"wrong_SOAPAction_here"'})
      -> on_fault(sub{});

    $test++; print $s->getStateName(1)->faultdetail =~ /SOAPAction shall match/ ? "ok $test\n" : "not ok $test\n"; 
  }
}

print "
This test sends a live SOAP call to your local web server (daemon implementation) with SOAP interface. See example in SOAP::Transport::HTTP.pm.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=15; print "skipped 15 test(s)\n"; 
} else {
# Local server with Perl implementation (http://www.geocities.com/paulclinger/soap.html)
  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('urn:/My/Examples')                
    -> proxy('http://localhost/')
  ;

  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/ ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  $test++; print ref $r && $r->{item2} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  $test++; print $result == $param1 && $param2->value == 24 ? "ok $test\n" : "not ok $test\n"; 

  print "Header manipulating test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  $test++; print $a->header->{my} eq '123123' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $a->headers eq '123123' ? "ok $test\n" : "not ok $test\n"; 

  print "Object autobinding and SOAP:: prefix test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/')";

  eval { SOAP->new(1) };
  $test++; print $@ =~ /^URI is not specified/ ? "ok $test\n" : "not ok $test\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:/A/B', proxy => 'http://localhost/')";

  # should call My::PingPong, not A::B
  my $p = My::PingPong->SOAP::new(10);
  my $next = $p->SOAP::next;
  $test++; print $p->value == $next+1 ? "ok $test\n" : "not ok $test\n";

  print "VersionMismatch test(s)...\n";

  {
    local $SOAP::Constants::NS_ENV = 'http://schemas.xmlsoap.org/new/envelope/';
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/')
      -> on_fault(sub{})
    ;
    $r = $s->dosomething;
    $test++; print $r->faultcode eq 'VersionMismatch' ? "ok $test\n" : "not ok $test\n";
  }

  print "Objects-by-reference test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/')";

  print "Session iterator\n";
  my $r = My::SessionIterator->new(10); $r->next;  
  $test++; print $r->next == 11 ? "ok $test\n" : "not ok $test\n";

  print "Persistent iterator\n";
  $r = My::PersistentIterator->new(10); $r->next; 
  my $first = $r->next;   

  $r = My::PersistentIterator->new(10); $r->next; 
  $test++; print $r->next == $first+2 ? "ok $test\n" : "not ok $test\n";

  print "Parameters-by-name test(s)...\n";
  print "You can see warning about AUTOLOAD for non-method...\n" if $^W;

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'http://my.own.site.com/My/Parameters', 
        proxy => 'http://localhost/')";

  my @parameters = (
    SOAP::Data->name(b => 222), 
    SOAP::Data->name(c => 333), 
    SOAP::Data->name(a => 111)
  );

  $test++; print byname(@parameters) eq "a=111, b=222, c=333" ? "ok $test\n" : "not ok $test\n";

  print "SOAPAction test(s)...\n";
  {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/')
      -> on_action(sub{'""'})
    ;
    $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 

    $s-> on_action(sub{'"wrong_SOAPAction_here"'})
      -> on_fault(sub{});

    $test++; print $s->getStateName(1)->faultdetail =~ /SOAPAction shall match/ ? "ok $test\n" : "not ok $test\n"; 
  }
}

print "
This test sends a live SOAP call to your local Apache server (mod_perl implementation) with SOAP interface. See example in SOAP::Transport::HTTP.pm.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=15; print "skipped 15 test(s)\n"; 
} else {
# Local server with Perl implementation (http://www.geocities.com/paulclinger/soap.html)
  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('urn:/My/Examples')                
    -> proxy('http://localhost/soap')
  ;

  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/ ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  $test++; print ref $r && $r->{item2} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  $test++; print $result == $param1 && $param2->value == 24 ? "ok $test\n" : "not ok $test\n"; 

  print "Header manipulating test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  $test++; print $a->header->{my} eq '123123' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $a->headers eq '123123' ? "ok $test\n" : "not ok $test\n"; 

  print "Object autobinding and SOAP:: prefix test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/soap')";

  eval { SOAP->new(1) };
  $test++; print $@ =~ /^URI is not specified/ ? "ok $test\n" : "not ok $test\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:/A/B', proxy => 'http://localhost/soap')";

  # should call My::PingPong, not A::B
  my $p = My::PingPong->SOAP::new(10);
  my $next = $p->SOAP::next;
  $test++; print $p->value == $next+1 ? "ok $test\n" : "not ok $test\n";

  print "VersionMismatch test(s)...\n";

  {
    local $SOAP::Constants::NS_ENV = 'http://schemas.xmlsoap.org/new/envelope/';
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/soap')
      -> on_fault(sub{})
    ;
    $r = $s->dosomething;
    $test++; print $r->faultcode eq 'VersionMismatch' ? "ok $test\n" : "not ok $test\n";
  }

  print "Objects-by-reference test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/soap')";

  print "Session iterator\n";
  my $r = My::SessionIterator->new(10); $r->next;  
  $test++; print $r->next == 11 ? "ok $test\n" : "not ok $test\n";

  print "Persistent iterator\n";
  $r = My::PersistentIterator->new(10); $r->next; 
  my $first = $r->next;   

  $r = My::PersistentIterator->new(10); $r->next; 
  $test++; print $r->next == $first+2 ? "ok $test\n" : "not ok $test\n";

  print "Parameters-by-name test(s)...\n";
  print "You can see warning about AUTOLOAD for non-method...\n" if $^W;

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'http://my.own.site.com/My/Parameters', 
        proxy => 'http://localhost/soap')";

  my @parameters = (
    SOAP::Data->name(b => 222), 
    SOAP::Data->name(c => 333), 
    SOAP::Data->name(a => 111)
  );

  $test++; print byname(@parameters) eq "a=111, b=222, c=333" ? "ok $test\n" : "not ok $test\n";

  print "SOAPAction test(s)...\n";
  {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/soap')
      -> on_action(sub{'""'})
    ;
    $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 

    $s-> on_action(sub{'"wrong_SOAPAction_here"'})
      -> on_fault(sub{});

    $test++; print $s->getStateName(1)->faultdetail =~ /SOAPAction shall match/ ? "ok $test\n" : "not ok $test\n"; 
  }
}

print "
This test sends a live SOAP call to your local Apache server (Apache::Registry implementation) with SOAP interface. See example in SOAP::Transport::HTTP.pm.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=15; print "skipped 15 test(s)\n"; 
} else {
# Local server with Perl implementation (http://www.geocities.com/paulclinger/soap.html)
  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('urn:/My/Examples')                
    -> proxy('http://localhost/mod_perl/soap.mod_cgi')
  ;

  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/ ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  $test++; print ref $r && $r->{item2} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  $test++; print $result == $param1 && $param2->value == 24 ? "ok $test\n" : "not ok $test\n"; 

  print "Header manipulating test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  $test++; print $a->header->{my} eq '123123' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $a->headers eq '123123' ? "ok $test\n" : "not ok $test\n"; 

  print "Object autobinding and SOAP:: prefix test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/mod_perl/soap.mod_cgi')";

  eval { SOAP->new(1) };
  $test++; print $@ =~ /^URI is not specified/ ? "ok $test\n" : "not ok $test\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:/A/B', proxy => 'http://localhost/mod_perl/soap.mod_cgi')";

  # should call My::PingPong, not A::B
  my $p = My::PingPong->SOAP::new(10);
  my $next = $p->SOAP::next;
  $test++; print $p->value == $next+1 ? "ok $test\n" : "not ok $test\n";

  print "VersionMismatch test(s)...\n";

  {
    local $SOAP::Constants::NS_ENV = 'http://schemas.xmlsoap.org/new/envelope/';
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/mod_perl/soap.mod_cgi')
      -> on_fault(sub{})
    ;
    $r = $s->dosomething;
    $test++; print $r->faultcode eq 'VersionMismatch' ? "ok $test\n" : "not ok $test\n";
  }

  print "Objects-by-reference test(s)...\n";

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/mod_perl/soap.mod_cgi')";

  print "Session iterator\n";
  my $r = My::SessionIterator->new(10); $r->next;  
  $test++; print $r->next == 11 ? "ok $test\n" : "not ok $test\n";

  print "Persistent iterator\n";
  $r = My::PersistentIterator->new(10); $r->next; 
  my $first = $r->next;   

  $r = My::PersistentIterator->new(10); $r->next; 
  $test++; print $r->next == $first+2 ? "ok $test\n" : "not ok $test\n";

  print "Parameters-by-name test(s)...\n";
  print "You can see warning about AUTOLOAD for non-method...\n" if $^W;

  eval "use SOAP::Lite +autodispatch 
    => (uri => 'http://my.own.site.com/My/Parameters', 
        proxy => 'http://localhost/mod_perl/soap.mod_cgi')";

  my @parameters = (
    SOAP::Data->name(b => 222), 
    SOAP::Data->name(c => 333), 
    SOAP::Data->name(a => 111)
  );

  $test++; print byname(@parameters) eq "a=111, b=222, c=333" ? "ok $test\n" : "not ok $test\n";

  print "SOAPAction test(s)...\n";
  {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy('http://localhost/mod_perl/soap.mod_cgi')
      -> on_action(sub{'""'})
    ;
    $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 

    $s-> on_action(sub{'"wrong_SOAPAction_here"'})
      -> on_fault(sub{});

    $test++; print $s->getStateName(1)->faultdetail =~ /SOAPAction shall match/ ? "ok $test\n" : "not ok $test\n"; 
  }
}

print "
This test sends a live SOAP call to your local SECURE server (CGI implementation) with SOAP interface. See example in SOAP::Transport::HTTP.pm.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=7; print "skipped 7 test(s)\n"; 
} else {
# Local server with Perl implementation (http://www.geocities.com/paulclinger/soap.html)
  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('http://my.own.site.com/My/Examples')                
    -> proxy('https://localhost/cgi-bin/soap.cgi')
  ;

  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/ ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  $test++; print ref $r && $r->{item2} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  $test++; print $result == $param1 && $param2->value == 24 ? "ok $test\n" : "not ok $test\n"; 

  print "Header manipulating test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  $test++; print $a->header->{my} eq '123123' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $a->headers eq '123123' ? "ok $test\n" : "not ok $test\n"; 
}

print "
This test sends a live SOAP call to your local Apache server with SOAP interface and Quote service deployed (http://localhost:8080/examples/rpcrouter/rpcrouter.jsp)
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'yes') =~ /^\s*y/i) {
  $test+=2; print "skipped 2 test(s)\n"; 
} else {
# Local server with Apache SOAP (http://xml.apache.org/soap)
  print "Apache SOAP server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('urn:xmltoday-delayed-quotes')
    -> proxy('http://localhost:8080/examples/rpcrouter/rpcrouter.jsp')
  ;

  $test++; print $s->getQuote('MSFT')->result > 0 ? "ok $test\n" : "not ok $test\n";
  $test++; print $s->getQuote(SOAP::Data->name(symbol => 'MSFT'))->result > 0 ? "ok $test\n" : "not ok $test\n";
}

print "
This test sends a live SOAP call to several public test servers available on Internet.
If you're not connected to the internet, please skip this test.
";
if (ExtUtils::MakeMaker::prompt('Do you want me to skip this test?', 'no') =~ /^\s*y/i) {
  $test+=17; print "skipped 17 test(s)\n"; 
} else {
# Public test server with Frontier implementation (http://soap.weblogs.com/)
  print "Frontier server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('/examples')
    -> on_action(sub { sprintf '"%s"', @_ })
    -> proxy('http://superhonker.userland.com/')
  ;

  $test++; print $s->getCurrentTime()->result ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateName(1)->result eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 
  $test++; print $s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s+$/ ? "ok $test\n" : "not ok $test\n"; 

  $r = $s->getStateList([1,2,3,4])->result;
  $test++; print ref $r && $r->[0] eq 'Alabama' ? "ok $test\n" : "not ok $test\n"; 

  $r = $s->getStateStruct(SOAP::Data->type(ordered_hash => [item1 => 1, item2 => 4]))->result;
  $test++; print ref $r && $r->{state4} eq 'Arkansas' ? "ok $test\n" : "not ok $test\n"; 

# Public test server with XSLT implementation (http://soap.develop.com/)
  print "XSLT server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('uuid:84124454-ff27-4c41-8f21-dff5f2aa241d')
    -> namespace('env')
    -> encodingspace('enc')
    -> proxy('http://soap.develop.com/xsltwire/calculator.xslt')
  ;

  $test++; print $s->call('icalc:Add' => SOAP::Data->name(a => 10), 
                                         SOAP::Data->name(b => 3))->result == 13 
    ? "ok $test\n" : "not ok $test\n";

# Public test server with COM implementation (http://www.zaks.demon.co.uk/com/4s4c/)
  print "COM server test(s)...\n";
  $s = SOAP::Lite 
    -> uri('http://simon.fell.com/calc')
    -> proxy('http://www.razorsoft.net/ssss4c/soap.asp')
  ;

  $r = $s->doubler(name SOAP::Data nums => [10,20,30,50,100])->result;
  $test++; print ref $r && $r->[1] == 40 ? "ok $test\n" : "not ok $test\n";

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
  $test++; print ref $r && $r->faultcode eq 'SOAP-ENV:Client' ? "ok $test\n" : "not ok $test\n";
  # Password should be wrong. Put yours if you have it. 
  # Remember: this is the real server

# Public server with SOAP::Lite/ApacheSOAP implementations (http://www.xmethods.com/)
  print "XMethods (SOAP::Lite/ApacheSOAP) server test(s)...\n";
  $s = SOAP::Lite                             
    -> uri('urn:xmethods-BNPriceCheck')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter');

  my $isbn = '0672319225';
  $r = $s->getPrice(SOAP::Data->type(string => $isbn))->result;
  print "Price for ISBN$isbn is \$$r\n";
  $test++; print $r > 20 && $r < 60 ? "ok $test\n" : "not ok $test\n";

  $s = SOAP::Lite                             
    -> uri('urn:xmethods-CurrencyExchange')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter');

  $r = $s->getRate(SOAP::Data->name(country1 => 'England'), 
                   SOAP::Data->name(country2 => 'Japan'))
         ->result;
  print "Currency rate for England/Japan is $r\n";
  $test++; print $r > 1 ? "ok $test\n" : "not ok $test\n";

  $s = SOAP::Lite                             
    -> uri('urn:xmethods-delayed-quotes')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter');

  $r = $s->getQuote('MSFT')->result;
  print "Quote for MSFT symbol is $r\n";
  $test++; print $r > 1 ? "ok $test\n" : "not ok $test\n";

  $s = SOAP::Lite                             
    -> uri('urn:xmethods-delayed-quotes')                
    -> proxy('http://services.xmethods.com:9090/soap');

  print "Connect to server with keep-alive\n";
  $r = $s->getQuote('MSFT')->result;
  $test++; print $r > 1 ? "ok $test\n" : "not ok $test\n";

  $test++; print SOAP::Lite
    -> uri('urn:xmethods-DomainChecker')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter')
    -> checkDomain('yahoo.com')
    -> result eq 'unavailable' ? "ok $test\n" : "not ok $test\n";

  $test++; print SOAP::Lite
    -> uri('urn:xmethods-CATraffic')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter')
    -> getTraffic(type SOAP::Data string => 101)
    -> result =~ /US 101/ ? "ok $test\n" : "not ok $test\n";

  $test++; print SOAP::Lite
    -> uri('urn:xmethods-Temperature')                
    -> proxy('http://services.xmethods.com:8080/soap/servlet/rpcrouter')
    -> getTemp(type SOAP::Data string => 64151)
    -> result =~ /\./ ? "ok $test\n" : "not ok $test\n";

  $test++; print SOAP::Lite
    -> uri('urn:xmethods-soapPing')                
    -> proxy('http://services.xmethods.com/perl/soap.pl?class=soapPing')
    -> pingHost(name SOAP::Data hostname => 'www.yahoo.com')
    -> result == 1 ? "ok $test\n" : "not ok $test\n";

  print "BabelFish translator server test(s)...\n";
  $test++; print SOAP::Lite                             
    -> uri('urn:xmethodsBabelFish')                
    -> proxy('http://services.xmethods.com/perl/soaplite.cgi')
    -> BabelFish(SOAP::Data->name(translationmode => 'en_it'), 
                 SOAP::Data->name(sourcedata => 'I want to work'))
    -> result =~ /^Desidero lavorare$/ ? "ok $test\n" : "not ok $test\n";

  print "DevelopMentor's Perl server test(s)...\n";
  $test++; print SOAP::Lite                             
    -> uri('urn:soap-perl-test')                
    -> proxy('http://soapl.develop.com/soap?class=SPTest')
    -> add(SOAP::Data->name(a => 3), SOAP::Data->name(b => 4))
    -> result == 7 ? "ok $test\n" : "not ok $test\n";
}
