#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use strict;
use SOAP::Lite;
use Data::Dumper;

@ARGV >= 1 or die "Usage: $0 proxy [uri [commands...]]\n";
my($proxy, $uri) = (shift, shift);
my %can;
my $soap = SOAP::Lite->proxy($proxy);
                $soap->uri($uri) if $uri;
print "Usage: method[(parameters)]\n> ";
while (defined($_ = shift || <>)) {
  my($method) = /\s*(\w+)/;
  $can{$method} = $soap->can($method) unless exists $can{$method};
  my $res = eval "\$soap->$_";
  $@                               ? warn(join "\n", "--- SYNTAX ERROR ---", $@, '') :
  $can{$method} && ref($res) ne 'SOAP::SOM'
                                   ? warn(join "\n", "--- METHOD RESULT ---", $res || '', '') :
  defined($res) && $res->fault     ? warn(join "\n", "--- SOAP FAULT ---", $res->faultcode, $res->faultstring, $res->faultdetail, '') :
  !$soap->transport->is_success    ? warn(join "\n", "--- TRANSPORT ERROR ---", $soap->transport->status, '') :
                                     warn(join "\n", "--- SOAP RESULT ---", Dumper($res->result), '')
} continue {
  print "\n> ";
}

__END__

=head1 NAME

SOAPsh.pl - Interactive shell for SOAP calls

=head1 SYNOPSIS

  perl SOAPsh.pl http://superhonker.userland.com/ /examples 
  > on_action(sub{sprintf '"%s"',@_}) # you'll need it only for Userland's server
  > getStateName(2)
  > getStateNames(1,2,3,7)
  > getStateList([1,9])
  > getStateStruct({a=>1, b=>24})
  > Ctrl-Z/Ctrl-D

or

  # all parameters after uri will be executed as methods
  perl SOAPsh.pl http://www.razorsoft.net/ssss4c/soap.asp http://simon.fell.com/calc doubler([10,20,30])
  > Ctrl-Z/Ctrl-D

=head1 DESCRIPTION

SOAPsh.pl is a shell for making SOAP calls. It takes two parameters:
mandatory endpoint and optional uri (actually it'll tell you about it 
if you try to run it). Additional commands can follow.

After that you'll be able to run any methods of SOAP::Lite, like autotype, 
readable, encoding etc. You can run it the same way as you do it in 
your Perl script. You'll see output from method, result of SOAP call,
detailed info on SOAP faulure or transport error.

For full list of available methods see documentation for SOAP::Lite.

Along with methods of SOAP::Lite you'll be able (and that's much more 
interesting) run any SOAP methods you know about on remote server and
see processed results. You can even switch on debugging (with call 
something like: C<on_debug(sub{print@_})>) and see SOAP code with 
headers sent and recieved.

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
