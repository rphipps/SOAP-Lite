#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

BEGIN { plan tests => 37 }

use SOAP::Lite;

my($a, $s, $r, $serialized, $deserialized);

{ # check root, mustUnderstand
  print "root and mustUnderstand attributes with SOAP::Data test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(SOAP::Data->root(1 => 1)->name('rootandunderstand')->mustUnderstand(1));

  ok($serialized =~ m!<rootandunderstand( xsi:type="xsd:int"| SOAP-ENV:mustUnderstand="1"| SOAP-ENV:root="1"){3}>1</rootandunderstand>!);
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

  ok($deserialized->result->[2] == 60);
  ok(ref $deserialized->body eq 'Body');
}

{ # check deserialization of envelope with fault
  print "Deserialization of envelope with fault test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsd="http://www.w3.org/1999/XMLSchema" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
<SOAP-ENV:Body>
<SOAP-ENV:Fault><faultcode>SOAP-ENV:Client</faultcode><faultstring>Application Error</faultstring><detail>Invalid Password</detail></SOAP-ENV:Fault></SOAP-ENV:Body></SOAP-ENV:Envelope>
');

  ok($deserialized->faultcode eq 'SOAP-ENV:Client');
  ok($deserialized->faultstring eq 'Application Error');
  ok($deserialized->faultdetail eq 'Invalid Password');
}

{ # check deserialization of circular references
  print "Deserialization of circular references test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Struct prefix:id="123" xmlns:prefix="aaa" id="ref-0xb61350"><a id="ref-0xb61374"><b href="#ref-0xb61350"/></a></SOAP-ENV:Struct>
');

  ok(ref $deserialized->valueof('/Struct') eq ref $deserialized->valueof('//b'));

  ok($deserialized->dataof('/Struct')->attr->{'prefix:id'} == 123);
  ok(! exists $deserialized->dataof('/Struct')->attr->{'id'});
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
  ok($deserialized->valueof("$result/[1]") == 20);
  ok($deserialized->valueof("$result/[3]") == 60);
  ok($deserialized->valueof("$result/[5]") == 200);

  # match should return true/false in boolean context (and object ref otherwise)
  ok($deserialized->match('aaa') ? 0 : 1);

  # should return same string as above
  ok($deserialized->match(SOAP::SOM->result));

  ok($deserialized->valueof('[1]') == 20);
  ok($deserialized->valueof('[3]') == 60);
  ok($deserialized->valueof('[5]') == 200);

  $deserialized->match('//Body/[1]/[1]'); # match path and change current node on success
  ok($deserialized->valueof('[1]') == 20);
  ok($deserialized->valueof('[3]') == 60);
  ok($deserialized->valueof('[5]') == 200);
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

  ok($paramsout[0] eq 'name2' && $paramsout[1] eq 'name3');
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

  ok($deserialized->namespaceuriof(SOAP::SOM::method) eq 'http://simon.fell.com/calc');
  ok($deserialized->namespaceuriof('//doublerResponse') eq 'http://simon.fell.com/calc');
}

{ # check for serialization with SOAPStruct (for interoperability with ApacheSOAP)
  print "Serialization w/out SOAPStruct test(s)...\n";

  $a = { a => 1 };

  ok(SOAP::Serializer->serialize($a) =~ m!SOAPStruct!);
  ok(SOAP::Serializer->autotype(0)->serialize($a) !~ m!SOAPStruct!);
}

{ # check header/envelope serialization/deserialization   
  print "Header/Envelope serialization/deserialization test(s)...\n";

  $serialized = SOAP::Serializer->method( # same as ->envelope(method =>
      'mymethod', 1, 2, 3, 
      SOAP::Header->name(t1 => 5)->attr({'~V:mustUnderstand' => 1}),
      SOAP::Header->name(t2 => 7)->mustUnderstand(2),
  );
  $deserialized = SOAP::Deserializer->deserialize($serialized);

  my $t1 = $deserialized->match(SOAP::SOM::header)->dataof('t1');
  my $t2 = $deserialized->dataof('t2');
  my @paramsin = $deserialized->paramsin;

  ok($t2->type eq 'xsd:int');
  ok($t2->mustUnderstand == 1);
  ok(@paramsin == 3);

  $serialized = SOAP::Serializer->method( # same as ->envelope(method =>
      SOAP::Data->name('mymethod')->attr({something => 'value'}), 1, 2, 3, 
  );
  ok($serialized =~ /<mymethod something="value">/);

  $serialized = SOAP::Serializer
    -> namespace('')
    -> method('mymethod');
  ok($serialized =~ m!<Envelope(?: xmlns="http://schemas.xmlsoap.org/soap/envelope/"| encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"| xmlns:xsd="http://www.w3.org/1999/XMLSchema")+><Body><mymethod/></Body></Envelope>!);

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/1999/XMLSchema"><SOAP-ENV:Body><getStateName><c-gensym5 xsi:type="xsd:int">1</c-gensym5></getStateName></SOAP-ENV:Body></SOAP-ENV:Envelope>');
  ok(! defined $deserialized->namespaceuriof('//getStateName'));

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0" encoding="UTF-8"?><SOAP-ENV:Envelope xmlns="a" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsd="http://www.w3.org/1999/XMLSchema"><SOAP-ENV:Body><getStateName><c-gensym5 xsi:type="xsd:int">1</c-gensym5></getStateName></SOAP-ENV:Body></SOAP-ENV:Envelope>');
  ok($deserialized->namespaceuriof('//getStateName') eq 'a');

}

{ # Map type serialization/deserialization
  print "Map type serialization/deserialization test(s)...\n";

  my $key = "\0\1";
  $serialized = SOAP::Serializer->method(aa => SOAP::Data->type(map => {a => 123, $key => 456})->name('maaap'));

  { local $^W; # disable warning on implicit map encoding
    my $implicit = SOAP::Serializer->method(aa => SOAP::Data->name(maaap => {a => 123, $key => 456}));
    ok($implicit eq $serialized);
  }
  ok($serialized =~ /xmlsoap:Map/);
  ok($serialized =~ m!xmlns:xmlsoap="http://xml.apache.org/xml-soap"!);

  $deserialized = SOAP::Deserializer->deserialize($serialized);
  $a = $deserialized->valueof('//maaap');
  ok(UNIVERSAL::isa($a => 'HASH'));
  ok(ref $a && $a->{$key} == 456);
}
