#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use My::PingPong;
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

  # you can manipulate same object on remote and local machine
  my $p = My::PingPong->new(10);           # local
  # my $p = My::PingPong->SOAP::new(10);   # same thing remotely
  print 'remote: ', $p->SOAP::next, "\n";  # remote
  print 'local: ', $p->next, "\n";         # local
  print 'remote: ', $p->SOAP::value, "\n"; # remote


