#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite +autodispatch 
  => (uri => 'urn:', 
      proxy => 'http://localhost/', 
      # proxy => 'http://localhost/cgi-bin/soap.cgi', # local CGI server
      # proxy => 'http://localhost/',                 # local daemon server
      # proxy => 'http://localhost/soap',             # local mod_perl server
      # proxy => 'https://localhost/soap',            # local mod_perl SECURE server
      on_fault => sub {
        my($soap, $res) = @_;
        ref $res ? die(join "\n", "--- SOAP FAULT ---", $res->faultcode, $res->faultstring, $res->faultdetail, '') 
                 : die(join "\n", "--- TRANSPORT ERROR ---", $soap->transport->status, '');
      }
     )
;

my $e = new My::Chatbot::Eliza 'Your name';
print "Talk, please\n> ";
while (<>) {
  print $e->transform;
} continue {
  print "\n> ";
}
