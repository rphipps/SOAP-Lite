#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

BEGIN { plan tests => 20 }

use SOAP::Lite;

my($a, $s, $r, $serialized, $deserialized);

{ # check 'use ...'
  print "'use SOAP::Lite ...' test(s)...\n";

  eval 'use SOAP::Lite 99.99'; # hm, definitely should fail

  ok($@ =~ /99\.99 required/);
}

{ # check serialization
  print "Arrays, structs, refs serialization test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(1, [1,2], {a=>3}, \4);

  ok($serialized =~ m!<c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\1><SOAP-ENC:Array(?: xsi:type="SOAP-ENC:Array"| SOAP-ENC:arrayType="xsd:int\[2\]"){2}><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\2><c-gensym\2 xsi:type="xsd:int">2</c-gensym\2></SOAP-ENC:Array><c-gensym(\d+) xsi:type="SOAPStruct"><a xsi:type="xsd:int">3</a></c-gensym\3><c-gensym(\d+)><c-gensym(\d+) xsi:type="xsd:int">4</c-gensym\5></c-gensym\4>!);
}  

{ # check simple circular references
  print "Simple circular references (\$a=\\\$a) serialization test(s)...\n";

  $a = \$a;
  $serialized = join '', SOAP::Serializer->serialize($a);

  ok($serialized =~ m!<c-gensym(\d+) id="ref-(\w+)"><c-gensym(\d+) href="#ref-\2"/></c-gensym\1>!);
}

{ # check complex circlular references
  print "Complex circlular references serialization test(s)...\n";

  $a = { a => 1 }; my $b = { b => $a }; $a->{a} = $b;
  $serialized = join '', SOAP::Serializer->serialize($a);

  ok($serialized =~ m!<c-gensym(\d+)(?: xsi:type="SOAPStruct"| id="ref-(\w+)"){2}><a(?: xsi:type="SOAPStruct"| id="ref-\w+"){2}><b(?: xsi:type="SOAPStruct"| href="#ref-\2"){2}/></a></c-gensym\1>!);
}

{ # check multirefs
  print "Multireferences serialization test(s)...\n";

  $a = 1; my $b = \$a;

  $serialized = join '', SOAP::Serializer->new(multirefinplace=>1)->serialize($b, $b);

  ok($serialized =~ m!<c-gensym(\d+) id="ref-(\w+)"><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\3></c-gensym\1><c-gensym\d+ href="#ref-\2"/>!);

  $serialized = join '', SOAP::Serializer->serialize($b, $b);

  ok($serialized =~ m!<c-gensym\d+ href="#ref-(\w+)"/><c-gensym\d+ href="#ref-\1"/><c-gensym(\d+) id="ref-\1"><c-gensym(\d+) xsi:type="xsd:int">1</c-gensym\3></c-gensym\2>!);
}

{ # check base64, XML encoding of elements and attributes 
  print "base64, XML encoding of elements and attributes test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize("\0\1\2\3   \4\5\6", '<123>&amp</123>');

  ok($serialized =~ m!<c-gensym(\d+) xsi:type="SOAP-ENC:base64">AAECAyAgIAQFBg==</c-gensym\1><c-gensym(\d+) xsi:type="xsd:string">&lt;123>&amp;amp&lt;/123></c-gensym\2>!);

  $serialized = join '', SOAP::Serializer->serialize(
    SOAP::Data->name(name=>'value')->attr({attr => '<123>"&amp"</123>'})
  );

  ok($serialized eq qq!<name xsi:type="xsd:string" attr="&lt;123>&quot;&amp;amp&quot;&lt;/123>">value</name>!);
}

{ # check objects and SOAP::Data 
  print "Blessed references and SOAP::Data encoding test(s)...\n";

  $serialized = join '', SOAP::Serializer->serialize(SOAP::Data->uri('some_urn' => bless {a => 1} => 'ObjectType'));

  ok($serialized =~ m!<namesp(\d+):ObjectType xsi:type="namesp\1:ObjectType" xmlns:namesp\1="some_urn"><a xsi:type="xsd:int">1</a></namesp\1:ObjectType>!);
}

{ # check for serialization with SOAPStruct (for interoperability with ApacheSOAP)
  print "Serialization w/out SOAPStruct test(s)...\n";

  $a = { a => 1 };

  ok(SOAP::Serializer->serialize($a) =~ m!SOAPStruct!); 
  ok(SOAP::Serializer->autotype(0)->serialize($a) !~ m!SOAPStruct!); 
}

{ # check serialization/deserialization of simple types  
  print "Serialization/deserialization of simple types test(s)...\n";

  $a = 'abc234xyz';

  $serialized = SOAP::Serializer->serialize(SOAP::Data->type(hex => $a));

  ok($serialized =~ m!<c-gensym(\d+) xsi:type="xsd:hex">61626332333478797a</c-gensym(\d+)>!);
  ok(SOAP::Deserializer->deserialize($serialized)->root eq $a); 

  $a = <<"EOBASE64";
qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?`1234567890-=\~!@#$%^&*()_+|
EOBASE64

  $serialized = SOAP::Serializer->serialize($a);

  ok(index($serialized, quotemeta(q!qwertyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM&lt;>?`1234567890-=~\!@#0^&amp;*()_+|!)));
  ok(SOAP::Deserializer->deserialize($serialized)->root eq $a);

  $a = <<"EOBASE64";

qwertyuiop[]asdfghjkl;'zxcvbnm,./
QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>?
\x00

EOBASE64

  $serialized = SOAP::Serializer->serialize($a);

  ok($serialized =~ /base64/);
}

{ # check serialization/deserialization of blessed reference  
  print "Serialization/deserialization of blessed reference test(s)...\n";

  $a = SOAP::Deserializer->deserialize(
    SOAP::Serializer->serialize(bless {a => 1} => 'SOAP::Lite')
  )->root;

  ok(ref $a eq 'SOAP::Lite' && UNIVERSAL::isa($a => 'HASH'));

  $a = SOAP::Deserializer->deserialize(
    SOAP::Serializer->serialize(bless [a => 1] => 'SOAP::Lite')
  )->root;

  ok(ref $a eq 'SOAP::Lite' && UNIVERSAL::isa($a => 'ARRAY'));
}

{ # check serialization/deserialization of undef/empty elements  
  print "Serialization/deserialization of undef/empty elements test(s)...\n";

  { local $^W; # suppress warnings
    $a = undef;
    $serialized = SOAP::Serializer->serialize(SOAP::Data->type(negativeInteger => $a));

    ok(SOAP::Deserializer->deserialize($serialized)->root == $a);

    my $type = 'nonstandardtype';
    $serialized = SOAP::Serializer->serialize(SOAP::Data->type($type => $a));

    ok(ref SOAP::Deserializer->deserialize($serialized)->root eq $type);
  }
}

