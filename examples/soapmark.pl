#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use strict;
use Benchmark;
use SOAP::Lite on_fault => sub {my($soap, $res) = @_; die ref $res ? $res->faultdetail : $soap->transport->status, "\n"};
use My::Examples;

my %dests = (
  local              => ['local://localhost/cgi-bin/soap.cgi' => 'http://soaplite.com/My/Examples'],
  mod_perl           => ['http://localhost/soap/' => 'http://soaplite.com/My/Examples'],
  CGI                => ['http://localhost/cgi-bin/soap.cgi' => 'http://soaplite.com/My/Examples'],
  daemon             => ['http://localhost:81/' => 'http://soaplite.com/My/Examples'],
  'Apache::Registry' => ['http://localhost/mod_perl/soap.mod_cgi' => 'http://soaplite.com/My/Examples'],
  tcpip              => ['tcp:localhost:81' => 'http://soaplite.com/My/Examples'],
  direct             => ['' => 'My::Examples'],
);

my $s;

my %tests = (
  simple => sub {$s->getStateName(1)},
  array =>  sub {$s->getStateName((1) x 100)},
);

my $testnum = 3;
my $testtime = 5;
my %result;

print STDERR <<DISCLAYMER;

This test should be used only for comparison different Perl implementations 
running in your environment. 

'tcpip' tests use one-way calls (twice less de/serializations than others)

DISCLAYMER

print STDERR "All tests may take up to ", %dests * %tests * $testnum * $testtime, " sec\n";

foreach my $dest (keys %dests) {
  my($proxy, $uri) = @{$dests{$dest}};
  $s = $proxy ? SOAP::Lite->proxy($proxy)->uri($uri) : $uri;
  foreach my $test (keys %tests) {
    printf STDERR "%s [%s] ", $dest, $test;
    eval {$tests{$test}->()}; warn('skipped due ', $@), next if $@;
    my($tps) = 0;
    for (1..$testnum) {
      my $r = Benchmark::runfor($tests{$test}, $testtime);
      my($pu, $ps, $n) = @{$r}[1,2,5];
      $tps += $n / ($pu + $ps);
      print STDERR ".";
    }
    printf STDERR " %.5s calls/s\n", $result{$dest}{$test} = $tps / $testnum;
  }
}