#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use strict;
use UDDI::Lite 
  import => ['UDDI::Data'], 
  import => ['UDDI::Lite'],
  proxy => "https://some.server.com/endpoint_fot_publishing_API",
;

my $name = 'Sample business';

print "Authorizing...\n";
my $auth = get_authToken({userID => 'USERID', cred => 'CRED'})->authInfo;
my $busent = businessEntity(name($name))->operator('soaplite.com');

print "Saving business '$name'...\n";
my $newent = save_business($auth, $busent)->businessEntity;
my $newkey = $newent->businessKey;

print "Created...\n";
print $newkey, "\n";
print $newent->discoveryURLs->discoveryURL, "\n";

print "Deleting '$newkey'...\n";
my $result = delete_business($auth, $newkey)->result;

print $result->errInfo, "\n";
