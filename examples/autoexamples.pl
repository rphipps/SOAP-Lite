#!/bin/env perl 

use SOAP::Lite +autodispatch 
  => (uri => 'urn:/My/Examples', 
      proxy => 'http://localhost/', 
      # proxy => 'http://localhost/cgi-bin/soap.cgi', # local CGI server
      # proxy => 'http://localhost/',                 # local daemon server
      # proxy => 'http://localhost/soap',             # local mod_perl server
      on_fault => sub {
        my($soap, $res) = @_;
        ref $res ? die(join "\n", "--- SOAP FAULT ---", $res->faultcode, $res->faultstring, $res->faultdetail, '') 
                 : die(join "\n", "--- TRANSPORT ERROR ---", $soap->transport->status, '');
      }
     )
;

print getStateName(1), "\n\n";
print getStateNames(12,24,26,13), "\n\n";
print getStateList([11,12,13,42])->[0], "\n\n";
print getStateStruct({item1 => 10, item2 => 4})->{item2}, "\n\n";
