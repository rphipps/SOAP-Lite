# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Test.pm,v 0.45 2001/01/16 00:38:04 $
#
# ======================================================================

package SOAP::Test;

use 5.004;
use vars qw($VERSION $TIMEOUT);
$VERSION = '0.45';

$TIMEOUT = 5;

# ======================================================================

package My::PingPong; # we'll use this package in our tests

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  bless {_num=>shift} => $class;
}

sub next {
  my $self = shift;
  $self->{_num}++;
}

sub value {
  my $self = shift;
  $self->{_num};
}

# ======================================================================

package SOAP::Test::Server;

use strict;
use Test;
use SOAP::Lite on_fault => sub{ref $_[1] ? $_[1] : new SOAP::SOM};

sub run_for {
  my $proxy = shift or die "Proxy/endpoint is not specified";

  # ------------------------------------------------------
  my $s = SOAP::Lite->uri('http://something/somewhere')->proxy($proxy)->on_fault(sub{});
  eval { $s->transport->timeout($SOAP::Test::TIMEOUT) };
  my $r = $s->test_connection;

  unless ($s->transport->is_success || $s->transport->status =~ /Internal Server Error/i) {
    print "1..0 # Skip: ", $s->transport->status, "\n"; exit;
  }
  # ------------------------------------------------------

  plan tests => 30;

  print "Perl SOAP server test(s)...\n";

  $s = SOAP::Lite
    -> uri('urn:/My/Examples')                
    -> proxy($proxy)
  ;

  ok($s->getStateName(1)->result eq 'Alabama'); 
  ok($s->getStateNames(1,4,6,13)->result =~ /^Alabama\s+Arkansas\s+Colorado\s+Illinois\s*$/); 

  $r = $s->getStateList([1,2,3,4])->result;
  ok(ref $r && $r->[0] eq 'Alabama'); 

  $r = $s->getStateStruct({item1 => 1, item2 => 4})->result;
  ok(ref $r && $r->{item2} eq 'Arkansas'); 

  print "Autobinding of output parameters test(s)...\n";

  $s->uri('urn:/My/Parameters');
  my $param1 = 10;
  my $param2 = SOAP::Data->name('myparam' => 12);
  my $result = $s->autobind($param1, $param2)->result;
  ok($result == $param1 && $param2->value == 24); 

  print "Header manipulation test(s)...\n";

  $a = $s->addheader(2, SOAP::Header->name(my => 123)); 
  ok(ref $a->header && $a->header->{my} eq '123123'); 
  ok($a->headers eq '123123'); 

  print "Object autobinding and SOAP:: prefix test(s)...\n";

  eval "use SOAP::Lite +autodispatch =>
    uri => 'urn:', proxy => '$proxy'; 1" or die;

  ok(SOAP::Lite->autodispatched);

  eval { SOAP->new(1) };
  ok($@ =~ /^URI is not specified/);

  eval "use SOAP::Lite +autodispatch =>
    uri => 'urn:/A/B', proxy => '$proxy'; 1" or die;

  # should call My::PingPong, not A::B
  my $p = My::PingPong->SOAP::new(10);
  ok(ref $p && $p->SOAP::next+1 == $p->value);

  print "VersionMismatch test(s)...\n";

  {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy($proxy)
      -> on_fault(sub{})
    ;
    $s->serializer->attr({%{$s->serializer->attr}, 'xmlns:~V' => 'http://schemas.xmlsoap.org/new/envelope/'});
    $r = $s->dosomething;
    ok(ref $r && $r->faultcode =~ /:VersionMismatch/);
  }

  print "Objects-by-reference test(s)...\n";

  eval "use SOAP::Lite +autodispatch =>
    uri => 'urn:', proxy => '$proxy'; 1" or die;

  print "Session iterator\n";
  $r = My::SessionIterator->new(10); 
  if (!ref $r || exists $r->{id}) {
    ok(ref $r && $r->next && $r->next == 11);
  } else {
    skip('No persistent objects (o-b-r) supported on server side' => undef);
  }

  print "Persistent iterator\n";
  $r = My::PersistentIterator->new(10); 
  if (!ref $r || exists $r->{id}) {
    my $first = ($r->next, $r->next) if ref $r;   

    $r = My::PersistentIterator->new(10);
    ok(ref $r && $r->next && $r->next == $first+2);
  } else {
    skip('No persistent objects (o-b-r) supported on server side' => undef);
  }

  { local $^W; # disable warnings about deprecated AUTOLOADing for nonmethods
    print "Parameters-by-name test(s)...\n";
    print "You can see warning about AUTOLOAD for non-method...\n" if $^W;

    eval "use SOAP::Lite +autodispatch => 
      uri => 'http://my.own.site.com/My/Parameters', proxy => '$proxy'; 1" or die;

    my @parameters = (
      SOAP::Data->name(b => 222), 
      SOAP::Data->name(c => 333), 
      SOAP::Data->name(a => 111)
    );

    # switch to 'main' package, because nonqualified methods should be there
    ok(main::byname(@parameters) eq "a=111, b=222, c=333");

    print "Function call test(s)...\n";
    print "You can see warning about AUTOLOAD for non-method...\n" if $^W;
    ok(main::echo(11) == 11);
  }

  print "SOAPAction test(s)...\n";
  if ($proxy =~ /^tcp:/) {
    for (1..2) {skip('No SOAPAction checks for tcp: protocol on server side' => undef)}
  } else {
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Examples')                
      -> proxy($proxy)
      -> on_action(sub{'""'})
    ;
    ok($s->getStateName(1)->result eq 'Alabama'); 

    $s-> on_action(sub{'"wrong_SOAPAction_here"'});
    ok($s->getStateName(1)->faultdetail =~ /SOAPAction shall match/); 
  }

  {
    print "Die in server method test(s)...\n";
    my $s = SOAP::Lite
      -> uri('http://my.own.site.com/My/Parameters')                
      -> proxy($proxy)
    ;
    ok($s->die_simply()->faultdetail =~ /Something bad/);
    my $detail = $s->die_with_object()->dataof(SOAP::SOM::faultdetail . '/[1]');
    ok(ref $detail && $detail->name =~ /(^|:)something$/);
  }

  print "Method with attributes test(s)...\n";

  $s = SOAP::Lite
    -> uri('urn:/My/Examples')                
    -> proxy($proxy)
  ;

  ok($s->call(SOAP::Data->name('getStateName')->attr({xmlns => 'urn:/My/Examples'}), 1)->result eq 'Alabama');

  print "Call with empty uri test(s)...\n";
  $s = SOAP::Lite
    -> uri('')                
    -> proxy($proxy)
  ;

  ok($s->getStateName(1)->faultdetail =~ /Denied access to method \(getStateName\) in class \(main\)/);

  ok($s->call('a:getStateName' => 1)->faultdetail =~ /Denied access to method \(getStateName\) in class \(main\)/);

  print "Memory refresh test(s)...\n";

  # Funny test. 
  # Let's forget about ALL settings we did before with 'use SOAP::Lite...'
  SOAP::Lite->self(undef); 
  ok(!defined SOAP::Lite->self);

  print "Call without uri test(s)...\n";
  $s = SOAP::Lite
    -> proxy($proxy)
  ;

  ok($s->getStateName(1)->faultdetail =~ /Denied access to method \(getStateName\) in class \(main\)/);

  print "Different settings for method and namespace test(s)...\n";

  ok($s->call(SOAP::Data
    ->name('getStateName')
    ->attr({xmlns => 'urn:/My/Examples'}), 1)->result eq 'Alabama');

  ok($s->call(SOAP::Data
    ->name('a:getStateName')
    ->attr({'xmlns:~' => 'urn:/My/Examples'}), 1)->result eq 'Alabama');

  ok($s->call(SOAP::Data
    ->name('a:getStateName')
    ->attr({'xmlns:a' => 'urn:/My/Examples'}), 1)->result eq 'Alabama');

  eval { $s->call(SOAP::Data->name('a:getStateName')) };

  ok($@ =~ /Can't find namespace for method \(a:getStateName\)/);

  $s->serializer->attr->{xmlns} = 'urn:/My/Examples';
  ok($s->getStateName(1)->result eq 'Alabama');

  eval "use SOAP::Lite 
    uri => 'urn:/My/Examples', proxy => '$proxy'; 1" or die;

  print "Global settings test(s)...\n";
  $s = new SOAP::Lite;

  ok($s->getStateName(1)->result eq 'Alabama');
}

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Test - Test framework for SOAP::Lite

=head1 SYNOPSIS

  use SOAP::Test;

  SOAP::Test::Server::run_for('http://localhost/cgi-bin/soap.cgi');

=head1 DESCRIPTION

SOAP::Test provides simple framework for testing server implementations.
Specify your address (endpoint) and run provided tests against your server.
See t/1*.t for examples.

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
