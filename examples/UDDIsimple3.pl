#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use strict;
use UDDI::Lite 
  import => ['UDDI::Data' => ':all'], 
  import => ['UDDI::Lite' => ':find', ':get'],
  proxy => 'http://test.uddi.microsoft.com/inquire',
;

my @parameters = (
  findQualifiers([findQualifier('sortByNameAsc'), 
                  findQualifier('caseSensitiveMatch')]), 
  name('M'),

# OR

#  findQualifiers(findQualifier('sortByNameAsc',
#                               'caseSensitiveMatch')), 
#  name('M'),
);

my $b = find_business(@parameters);
for ($b->businessInfos->businessInfo) {    
  print $_->name, "\n";    
  if ($_->name eq "Microsoft Corporation") {	
    my $key = $_->businessKey;	
    print "$key\n";	
    my $e = get_businessDetail($key)->businessEntity;	
    my @services = $e->businessServices->businessService;	
    for (@services) {	    
      print "  ", $_->name, "\n";	
    }    
  }
}
