#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

BEGIN { plan tests => 8 }

use SOAP::Lite;

my($a, $s, $r, $serialized, $deserialized);

my %tests = (
  'XML only' => <<'EOM',
<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" 
                   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" 
                   xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" 
                   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" 
                   xmlns:xsd="http://www.w3.org/1999/XMLSchema">
<SOAP-ENV:Body>
<namesp1:add xmlns:namesp1="http://www.soaplite.com/Calculator">
  <c-gensym5 xsi:type="xsd:int">2</c-gensym5>
  <c-gensym7 xsi:type="xsd:int">5</c-gensym7>
</namesp1:add>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
EOM

  'message with headers' => <<'EOM',
Content-Type: text/xml

<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" 
                   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" 
                   xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" 
                   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" 
                   xmlns:xsd="http://www.w3.org/1999/XMLSchema">
<SOAP-ENV:Body>
<namesp1:add xmlns:namesp1="http://www.soaplite.com/Calculator">
  <c-gensym5 xsi:type="xsd:int">2</c-gensym5>
  <c-gensym7 xsi:type="xsd:int">5</c-gensym7>
</namesp1:add>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>
EOM

  'singlepart MIME' => <<'EOM',
Content-Type: Multipart/Related; boundary=MIME_boundary; type="text/xml"; start="<calc061400a.xml@soaplite.com>"

--MIME_boundary
Content-Type: text/xml; charset=UTF-8
Content-Transfer-Encoding: 8bit
Content-ID: <calc061400a.xml@soaplite.com>

<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" 
                   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" 
                   xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" 
                   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" 
                   xmlns:xsd="http://www.w3.org/1999/XMLSchema">
<SOAP-ENV:Body>
<namesp1:add xmlns:namesp1="http://www.soaplite.com/Calculator">
  <c-gensym5 xsi:type="xsd:int">2</c-gensym5>
  <c-gensym7 xsi:type="xsd:int">5</c-gensym7>
</namesp1:add>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>

--MIME_boundary--
EOM

  'multipart MIME' => <<'EOM',
Content-Type: Multipart/Related; boundary=MIME_boundary; type="text/xml"; start="<calc061400a.xml@soaplite.com>"

--MIME_boundary
Content-Type: text/xml; charset=UTF-8
Content-Transfer-Encoding: 8bit
Content-ID: <calc061400a.xml@soaplite.com>

<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" 
                   SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/" 
                   xmlns:xsi="http://www.w3.org/1999/XMLSchema-instance" 
                   xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" 
                   xmlns:xsd="http://www.w3.org/1999/XMLSchema">
<SOAP-ENV:Body>
<namesp1:add xmlns:namesp1="http://www.soaplite.com/Calculator">
  <c-gensym5 href="cid:calc061400a.a@soaplite.com"/>
  <c-gensym7 href="cid:calc061400a.b@soaplite.com"/>
</namesp1:add>
</SOAP-ENV:Body>
</SOAP-ENV:Envelope>

--MIME_boundary
Content-Type: text/plain
Content-Transfer-Encoding: binary
Content-ID: <calc061400a.a@soaplite.com>

2
--MIME_boundary
Content-Type: text/plain
Content-Transfer-Encoding: binary
Content-ID: <calc061400a.b@soaplite.com>

5
--MIME_boundary--
EOM
);

my $is_mimeparser = eval { SOAP::MIMEParser->new; 1 };
my $reason = $@ unless $is_mimeparser;
print "MIME tests will be skipped: $reason" if defined $reason;
my $package = 'package Calculator; sub add { $_[1] + $_[2] }; 1';

{ 
  print "Server handler test(s)...\n";

  my $server = SOAP::Server->dispatch_to('Calculator');
  foreach (keys %tests) {
    my $result = SOAP::Deserializer->deserialize($server->handle($tests{$_}));
    $_ =~ /XML/ || $is_mimeparser ? ok(($result->faultdetail || '') =~ /Failed to access class \(Calculator\)/) 
                                  : skip($reason => undef);
  }

  eval $package or die;

  foreach (keys %tests) {
    my $result = SOAP::Deserializer->deserialize($server->handle($tests{$_}));
    print "$_: ", $result->result || $result->faultdetail, "\n";
    $_ =~ /XML/ || $is_mimeparser ? ok(($result->result || 0) == 7) 
                                  : skip($reason => undef);
  }
}
