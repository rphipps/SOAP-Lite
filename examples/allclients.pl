#!perl -w
#!d:\perl\bin\perl.exe 

# -- SOAP::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

use SOAP::Lite 
  uri => 'http://my.own.site.com/My/Examples',
  proxy => 'http://localhost/', 
# proxy => 'http://localhost/cgi-bin/soap.cgi', # local CGI server
# proxy => 'http://localhost/',                 # local daemon server
# proxy => 'http://localhost/soap',             # local mod_perl server
# proxy => 'https://localhost/soap',            # local mod_perl SECURE server
# proxy => 'tcp:localhost:82',                  # local tcp server
# proxy => 'ftp://login:password@ftp.somewhere.com/relative/path/to/file.xml', # ftp server
# proxy => 'ftp://login:password@ftp.somewhere.com//absolute/path/to/file.xml', # ftp server
# proxy => 'http://login:password@localhost/cgi-bin/soap.cgi', # local CGI server with authentication
# proxy => ['mailto:destination.email@address', smtp => 'smtp.server', From => 'your.email', Subject => 'SOAP message'], # smtp server
;

print getStateName(1);
