#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

BEGIN { plan tests => 90 }

use SOAP::Lite;

my($a, $s, $r, $serialized, $deserialized);

{ # check root, mustUnderstand
  print "root and mustUnderstand attributes with SOAP::Data test(s)...\n";

  $serialized = SOAP::Serializer->serialize(SOAP::Data->root(1 => 1)->name('rootandunderstand')->mustUnderstand(1));

  ok($serialized =~ m!<rootandunderstand( xsi:type="xsd:int"| SOAP-ENV:mustUnderstand="1"| SOAP-ENC:root="1"| xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"| xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"| xmlns:xsd="http://www.w3.org/1999/XMLSchema"){7}>1</rootandunderstand>!);
}

{ # check deserialization of envelope with result
  print "Deserialization of envelope with result test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
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
  ok((my @array = $deserialized->paramsall) == 1);
  ok(ref $deserialized->body eq 'HASH'); # not blessed anymore since 0.51
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
<Struct prefix:id="123" xmlns:prefix="aaa" id="ref-0xb61350"><a id="ref-0xb61374"><b href="#ref-0xb61350"/></a></Struct>
');

  ok(ref $deserialized->valueof('/Struct') eq ref $deserialized->valueof('//b'));

  ok($deserialized->dataof('/Struct')->attr->{'{aaa}id'} == 123); 
  ok(exists $deserialized->dataof('/Struct')->attr->{'id'});
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

{ # check for Array of Array serialization 
  print "Array of Array serialization test(s)...\n";

  $serialized = SOAP::Serializer
    -> readable(1)
    -> method('mymethod' => [[1, 2], [3, 4]]);

  ok($serialized =~ m!SOAP-ENC:arrayType="SOAP-ENC:Array\[2\]"!);
}

{ # check for serialization with SOAPStruct
  print "Serialization w/out SOAPStruct test(s)...\n";

  $a = { a => 1 };

  ok(SOAP::Serializer->serialize($a) =~ m!SOAPStruct!);
  ok(SOAP::Serializer->autotype(0)->serialize($a) !~ m!SOAPStruct!);

  $serialized = SOAP::Serializer->namespaces({})->maptype({SOAPStruct => ""})->serialize({a => 1});
  ok($serialized =~ m!<c-gensym(\d+)><a xsi:type="xsd:int">1</a></c-gensym\1>!);

  $serialized = SOAP::Serializer->namespaces({})->maptype({SOAPStruct => undef})->serialize({a => 1});
  ok($serialized =~ m!<c-gensym(\d+)><a xsi:type="xsd:int">1</a></c-gensym\1>!);
}

{ # check header/envelope serialization/deserialization   
  print "Header/Envelope serialization/deserialization test(s)...\n";

  $serialized = SOAP::Serializer->method( # same as ->envelope(method =>
      'mymethod', 1, 2, 3, 
      SOAP::Header->name(t1 => 5)->mustUnderstand(1),
      SOAP::Header->name(t2 => 7)->mustUnderstand(2),
  );
  $deserialized = SOAP::Deserializer->deserialize($serialized);

  my $t1 = $deserialized->match(SOAP::SOM::header)->dataof('t1');
  my $t2 = $deserialized->dataof('t2');
  my @paramsin = $deserialized->paramsin;
  my @paramsall = $deserialized->paramsall;

  ok($t2->type =~ /^int$/);
  ok($t2->mustUnderstand == 1);
  ok(@paramsin == 3);
  ok(@paramsall == 3);

  eval { $deserialized->result(1) };
  ok($@ =~ /Method 'result' is readonly/);

  $serialized = SOAP::Serializer->method( # same as ->envelope(method =>
      SOAP::Data->name('mymethod')->attr({something => 'value'}), 1, 2, 3, 
  );
  ok($serialized =~ /<mymethod something="value">/);

  $serialized = SOAP::Serializer
    -> envprefix('')
    -> method('mymethod');
  ok($serialized =~ m!<Envelope(?: xmlns:namesp\d+="http://schemas.xmlsoap.org/soap/envelope/"| namesp\d+:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"| xmlns:xsd="http://www.w3.org/1999/XMLSchema"){5}><Body><mymethod/></Body></Envelope>!);

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

{ # Stringified type serialization
  print "Stringified type serialization test(s)...\n";

  $serialized = SOAP::Serializer->serialize(bless { a => 1, _current => [] } => 'SOAP::SOM');
  ok($serialized =~ m!<SOAP__SOM( xsi:type="SOAP-ENC:SOAP__SOM"| xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"| xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"| xmlns:xsd="http://www.w3.org/1999/XMLSchema"){5}><a xsi:type="xsd:int">1</a><_current(?: SOAP-ENC:arrayType="xsd:ur-type\[0\]"| xsi:type="SOAP-ENC:Array"){2}/></SOAP__SOM>!);
}

{ # Serialization of non-allowed element
  print "Serialization of non-allowed element test(s)...\n";

  eval { $serialized = SOAP::Serializer->serialize(SOAP::Data->name('---' => 'aaa')) };

  ok($@ =~ /^Element/);
}

{ # Custom serialization of blessed reference
  print "Custom serialization of blessed reference test(s)...\n";

  eval q!
    sub SOAP::Serializer::as_My__Own__Class {
      my $self = shift;
      my($value, $name, $type, $attr) = @_;
      return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:string'}, join ', ', map {"$_ => $value->{$_}"} sort keys %$value];
    }
    1;
  ! or die;

  $serialized = SOAP::Serializer->serialize(bless {a => 1, b => 2} => 'My::Own::Class');
  ok($serialized =~ m!<My__Own__Class( xsi:type="xsd:string"| xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"| xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"| xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"| xmlns:xsd="http://www.w3.org/1999/XMLSchema"){5}>a => 1, b => 2</My__Own__Class>!);
}

{ # Multirefs serialization
  print "Multirefs serialization test(s)...\n";

  my $b = { b => 2 };
  my $a = { a => $b };
  my $c = { c1 => $a, c2 => $a };

  $serialized = SOAP::Serializer->autotype(0)->method(a => $c);
  ok($serialized =~ m!<SOAP-ENV:Body><a><c-gensym(\d+)><c1 href="#ref-(\d+)"/><c2 href="#ref-\2"/></c-gensym\1></a><c-gensym(\d+) id="ref-(\d+)"><b>2</b></c-gensym\3><c-gensym(\d+) id="ref-\2"><a href="#ref-\4"/></c-gensym\5></SOAP-ENV:Body>! ||
     $serialized =~ m!<SOAP-ENV:Body><a><c-gensym(\d+)><c1 href="#ref-(\d+)"/><c2 href="#ref-\2"/></c-gensym\1></a><c-gensym(\d+) id="ref-\2"><a href="#ref-(\d+)"/></c-gensym\3><c-gensym(\d+) id="ref-\4"><b>2</b></c-gensym\5></SOAP-ENV:Body>! ||
     $serialized =~ m!<SOAP-ENV:Body><a><c-gensym(\d+)><c2 href="#ref-(\d+)"/><c1 href="#ref-\2"/></c-gensym\1></a><c-gensym(\d+) id="ref-(\d+)"><b>2</b></c-gensym\3><c-gensym(\d+) id="ref-\2"><a href="#ref-\4"/></c-gensym\5></SOAP-ENV:Body>! ||
     $serialized =~ m!<SOAP-ENV:Body><a><c-gensym(\d+)><c2 href="#ref-(\d+)"/><c1 href="#ref-\2"/></c-gensym\1></a><c-gensym(\d+) id="ref-\2"><a href="#ref-(\d+)"/></c-gensym\3><c-gensym(\d+) id="ref-\4"><b>2</b></c-gensym\5></SOAP-ENV:Body>!);

  $serialized = SOAP::Serializer->autotype(0)->namespaces({})->serialize($c);
  ok($serialized =~ m!<c-gensym(\d+)><c1 href="#ref-(\d+)"/><c2 href="#ref-\2"/><c-gensym(\d+) id="ref-(\d+)"><b>2</b></c-gensym\3><c-gensym(\d+) id="ref-\2"><a href="#ref-\4"/></c-gensym\5></c-gensym\1>! ||
     $serialized =~ m!<c-gensym(\d+)><c1 href="#ref-(\d+)"/><c2 href="#ref-\2"/><c-gensym(\d+) id="ref-\2"><a href="#ref-(\d+)"/></c-gensym\3><c-gensym(\d+) id="ref-\4"><b>2</b></c-gensym\5></c-gensym\1>! ||
     $serialized =~ m!<c-gensym(\d+)><c2 href="#ref-(\d+)"/><c1 href="#ref-\2"/><c-gensym(\d+) id="ref-(\d+)"><b>2</b></c-gensym\3><c-gensym(\d+) id="ref-\2"><a href="#ref-\4"/></c-gensym\5></c-gensym\1>! ||
     $serialized =~ m!<c-gensym(\d+)><c2 href="#ref-(\d+)"/><c1 href="#ref-\2"/><c-gensym(\d+) id="ref-\2"><a href="#ref-(\d+)"/></c-gensym\3><c-gensym(\d+) id="ref-\4"><b>2</b></c-gensym\5></c-gensym\1>!);

  my $root = SOAP::Deserializer->deserialize($serialized)->root;

  ok($root->{c1}->{a}->{b} == 2);
  ok($root->{c2}->{a}->{b} == 2);
}

{ # Serialization of multirefs shared between Header and Body
  print "Serialization of multirefs shared between Header and Body test(s)...\n";

  $a = { b => 2 };

  $serialized = SOAP::Serializer->autotype(0)->method(a => SOAP::Header->value($a), $a);
  ok($serialized =~ m!<SOAP-ENV:Header><c-gensym\d+ href="#ref-(\d+)"/></SOAP-ENV:Header><SOAP-ENV:Body><a><c-gensym\d+ href="#ref-\1"/></a><c-gensym(\d+) id="ref-\1"><b>2</b></c-gensym\2></SOAP-ENV:Body>!);
}

{ # Deserialization with typecast
  print "Deserialization with typecast test(s)...\n";

  my $desc = 0;
  my $typecasts = 0;
  eval { 
    package MyDeserializer; 
    @MyDeserializer::ISA = 'SOAP::Deserializer';
    sub typecast;
    *typecast = sub { shift; 
      my($value, $name, $attrs, $childs, $type) = @_;
      $desc = "$name @{[scalar @$childs]}" if $name eq 'a';
      $typecasts++;
      return;
    };
    1;
  } or die;

  $deserialized = MyDeserializer->deserialize('<a><b>1</b><c>2</c></a>');
  ok($desc eq 'a 2'); #! fix "if $name eq 'a'", because $name is QName now ('{}a')
  ok($typecasts == 5);
}

{ # Deserialization with wrong encodingStyle
  print "Deserialization with wrong encodingStyle test(s)...\n";

  eval { $deserialized = SOAP::Deserializer->deserialize(
'<a 
   SOAP-ENV:encodingStyle="http://schemas.microsoft.com/soap/encoding/clr/1.0 http://schemas.xmlsoap.org/soap/encoding/"
   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
>1</a>') };
  ok(!$@ && $deserialized);

  eval { $deserialized = SOAP::Deserializer->deserialize(
'<a 
   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"
   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
>1</a>') };
  ok(!$@ && $deserialized);

  eval { $deserialized = SOAP::Deserializer->deserialize(
'<a 
   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/something"
   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
>1</a>') };
  ok(!$@ && $deserialized);

  eval { $deserialized = SOAP::Deserializer->deserialize(
'<a>1</a>') };
  ok(!$@ && $deserialized);

  eval { $deserialized = SOAP::Deserializer->deserialize(
'<a 
   SOAP-ENV:encodingStyle=""
   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
>1</a>') };
  ok(!$@ && $deserialized);
}

{ # Deserialization with root attribute
  print "Deserialization with root attribute test(s)...\n";

  # root="0", should skip
  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<m:doublerResponse1 SOAP-ENC:root="0" xmlns:m="http://soaplite.com/">
<nums>1</nums>
</m:doublerResponse1>
<m:doublerResponse2 xmlns:m="http://soaplite.com/">
<nums>2</nums>
</m:doublerResponse2>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  ok($deserialized->result == 2);

  # root="0", but in wrong namespace
  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<m:doublerResponse1 root="0" xmlns:m="http://soaplite.com/">
<nums>1</nums>
</m:doublerResponse1>
<m:doublerResponse2 xmlns:m="http://soaplite.com/">
<nums>2</nums>
</m:doublerResponse2>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  ok($deserialized->result == 1);

  # root="1"
  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"
	 xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/"
	 xmlns:xsd="http://www.w3.org/1999/XMLSchema"
	 xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance"
	SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
<SOAP-ENV:Body>
<m:doublerResponse1 SOAP-ENC:root="1" xmlns:m="http://soaplite.com/">
<nums>1</nums>
</m:doublerResponse1>
<m:doublerResponse2 xmlns:m="http://www.soaplite.com/2">
<nums>2</nums>
</m:doublerResponse2>
<m:doublerResponse2 xmlns:m="http://www.soaplite.com/3">
<nums>3</nums>
</m:doublerResponse2>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
');

  ok($deserialized->result == 1);
  ok($deserialized->valueof('//{http://www.soaplite.com/2}doublerResponse2/nums') == 2);
  ok($deserialized->valueof('//{http://www.soaplite.com/3}doublerResponse2/nums') == 3);
  my @nums = $deserialized->valueof('//doublerResponse2/nums');
  ok(@nums == 2);
  ok($nums[0] == 2 && $nums[1] == 3);
}

{ 
  print "Deserialization with null elements test(s)...\n";

  $deserialized = SOAP::Deserializer->deserialize('<?xml version="1.0"?>
<SOAP-ENV:Envelope xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsd="http://www.w3.org/1999/XMLSchema" SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
<SOAP-ENV:Body>
<namesp23:object_infoResponse xmlns:namesp23="http://localhost/Test">
<SOAP-ENC:Array xsi:type="SOAP-ENC:Array" SOAP-ENC:arrayType="SOAP-ENC:Array[]">
<item xsi:type="xsd:string">1</item>
<item xsi:type="xsd:string">2</item>
<item xsi:null="1"/>
<item xsi:null="1"/>
<item xsi:type="xsd:string">5</item>
<item xsi:type="xsd:string"/>
<item xsi:type="xsd:string">7</item>
</SOAP-ENC:Array>
</namesp23:object_infoResponse>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
')->result;

  ok(scalar @$deserialized == 7);
  ok(! defined $deserialized->[2]);
  ok(! defined $deserialized->[3]);
  ok($deserialized->[5] eq '');
}

{
  print "Deserialization with xsi:type='string' test(s)...\n";

  $a = 'SOAP::Lite';
  $deserialized = SOAP::Deserializer->deserialize(qq!<inputString xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" xsi:type="string" xmlns="http://schemas.xmlsoap.org/soap/encoding/">$a</inputString>!)->root;

  ok($deserialized eq $a);
}

{
  print "Serialization with explicit typing test(s)...\n";

  $serialized = SOAP::Serializer
    ->uri('bla')
    ->method(a => SOAP::Data->name('return')->type(int => 1));
  ok($serialized =~ /xsd:int/);

  $serialized = SOAP::Serializer
    ->uri('bla')
    ->method(a => SOAP::Data->name('return')->type(noint => 1));
  ok($serialized =~ /namesp\d+:noint/);
}

{
  print "Serialization with explicit namespaces test(s)...\n";

  $serialized = SOAP::Serializer->serialize(SOAP::Data->name('b' => 1));
  ok($serialized =~ m!<b !);

  $serialized = SOAP::Serializer->serialize(SOAP::Data->name('c:b' => 1));
  ok($serialized =~ m!<c:b !);

  $serialized = SOAP::Serializer->serialize(SOAP::Data->name('{a}b' => 1));
  ok($serialized =~ m!<namesp\d+:b ! && $serialized =~ m!xmlns:namesp\d+="a"!);

  $serialized = SOAP::Serializer->serialize(SOAP::Data->name('{}b' => 1));
  ok($serialized =~ m!<b ! && $serialized =~ m!xmlns=""!);
}

{
  print "Serialization without specified typemapping test(s)...\n";

  $serialized = SOAP::Serializer->uri('b')->method(a => bless {a => 1} => 'A');
  ok($serialized =~ m!<A xsi:type="namesp\d+:A">!);

  # higly questionably, but that's how it is
  $serialized = SOAP::Serializer->method(a => bless {a => 1} => 'A');
  ok($serialized =~ m!<A xsi:type="SOAP-ENC:A">!);
}

{
  print "Check for memory leaks test(s)...\n";

  my %calls;
  {
    SOAP::Lite->import(trace => [objects => sub { 
      $calls{$2}{$1}++ if (caller(2))[3] =~ /^(.+)::(.+)$/;
    }]);

    my $soap = SOAP::Lite
      -> uri("Echo")
      -> proxy("http://services.soaplite.com/echo.cgi");
  }
  foreach (keys %{$calls{new}}) {
    ok(exists $calls{DESTROY}{$_});
  }

  %calls = ();
  {
    local $SOAP::Constants::DO_NOT_USE_XML_PARSER = 1;
    my $soap = SOAP::Lite
      -> uri("Echo")
      -> proxy("http://services.soaplite.com/echo.cgi");
  }
  foreach (keys %{$calls{new}}) {
    ok(exists $calls{DESTROY}{$_});
  }

  SOAP::Lite->import(trace => '-objects');
}
