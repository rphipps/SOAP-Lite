#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite +autodispatch => 
  uri => 'http://my.own.site.com/My/ParametersByName',
  proxy => 'http://localhost/', 
# proxy => 'http://localhost/cgi-bin/soap.cgi', # local CGI server
# proxy => 'http://localhost/',                 # local daemon server
# proxy => 'http://localhost/soap',             # local mod_perl server
# proxy => 'https://localhost/soap',            # local mod_perl SECURE server
  on_fault => sub { my($soap, $res) = @_; 
    die ref $res ? $res->faultdetail : $soap->transport->status, "\n";
  }
;

my @parameters = (
  SOAP::Data->name(b => 222), 
  SOAP::Data->name(c => 333), 
  SOAP::Data->name(a => 111)
);

print "Parameters: ", join(' ', map {$_->value} @parameters), "\n";
print "By order: ", byorder(@parameters), "\n";
print "By name: ", byname(@parameters), "\n";
