#!perl -w
#!d:\perl\bin\perl.exe 

# -- UDDI::Lite -- soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

# You may run these tests/examples for UDDI publishing API against
# UDDI registry that was kindly provided with following disclamer:
# "This is just a free demo registry provided by ICZ Prague, IBM Czech
# Republic and KPNQwest Czechia. Use commercial test registries for 
# serious work." 
# Thanks to Petr Janata <petr.janata@i.cz> for help and support

use strict;
use UDDI::Lite 
  import => ['UDDI::Data'], 
  import => ['UDDI::Lite'],
  proxy => "http://srv.trebic.cz:8080/uddi/servlet/uddi",
;

my $name = 'Sample business ' . $$ . time; # just to make it unique

print "Authorizing...\n";
my $auth = get_authToken({userID => 'wstkDemo', cred => 'wstkPwd'})->authInfo;
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
