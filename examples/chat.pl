#!/bin/env perl 
#!d:\perl\bin\perl.exe 

use SOAP::Lite +autodispatch => 
  uri => 'http://my.own.site.com/My/Parameters',
  proxy => 'http://localhost/soap',
# proxy => 'http://localhost/',                 # local daemon server
# proxy => 'http://localhost/soap',             # local mod_perl server
# proxy => 'https://localhost/soap',            # local mod_perl SECURE server
  on_fault => sub { my($soap, $res) = @_; 
    die ref $res ? $res->faultdetail : $soap->transport->status, "\n";
  }
;

my $nick = shift;
my $c = My::Chat->join($nick);
my %whois = %{$c->whois};
print map { "$_ [" . mktime($whois{$_}) . "]\n" } keys %whois;
for (;;) {
  print map { 
    my($nick, $text, $time) = @$_; chomp($text); 
    join '', mktime($time), ' ', $nick, '>', ' ', $text, "\n"; 
  } @{$c->get};
  print "$nick> ";
  defined($_ = <>) or last;
  $c->send($_) if $_ =~ /\S/;
}
$c->quit;

sub mktime { use POSIX qw(strftime); strftime("%m/%d %H:%M", localtime shift) }
