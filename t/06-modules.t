#!/bin/env perl 

BEGIN {
  unless(grep /blib/, @INC) {
    chdir 't' if -d 't';
    unshift @INC, '../lib' if -d '../lib';
  }
}

use strict;
use Test;

BEGIN { plan tests => 11 }

foreach (qw(
  SOAP::Lite 
  SOAP::Transport::HTTP SOAP::Transport::MAILTO
  SOAP::Transport::FTP SOAP::Transport::TCP
  SOAP::Transport::IO SOAP::Transport::LOCAL
  SOAP::Transport::POP3
  UDDI::Lite
  XML::Parser::Lite
  Apache::SOAP
)) {
  eval "require $_";
  ok(!$@ || $@ =~ /Can't locate|XML::Parser::Lite requires/);
}
