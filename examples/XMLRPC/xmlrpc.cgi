#!/usr/bin/env perl

# -- XMLRPC::Lite -- services.soaplite.com -- Copyright (C) 2001 Paul Kulchenko --

use XMLRPC::Transport::HTTP;

my $server = XMLRPC::Transport::HTTP::CGI
  -> dispatch_to('methodName')
  -> handle
;

# -- methods are here --

BEGIN { @main::ISA = 'XMLRPC::Server::Parameters' }

sub methodName {
  my $self = shift;
  my $method = $_[-1]->method;
  # method's name here is 'something.methodname'
  # we'll take only methodname part of it
  $method =~ s/\w+\.//;
  return $self->$method(@_);
}

sub whichToolkit { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  return +{
    toolkitDocsUrl => 'http://www.soaplite.com/', 
    toolkitName => 'XMLRPC::Lite', 
    toolkitVersion => XMLRPC::Lite->VERSION, 
    toolkitOperatingSystem => $^O,
  }
}

sub countTheEntities { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $string = shift;
  my $res;
  $res->{ctLeftAngleBrackets} = ($string =~ s/<//g) || 0;
  $res->{ctRightAngleBrackets} = ($string =~ s/>//g) || 0;
  $res->{ctAmpersands} = ($string =~ s/&//g) || 0;
  $res->{ctApostrophes} = ($string =~ s/'//g) || 0;
  $res->{ctQuotes} = ($string =~ s/"//g) || 0;
  return $res;
}

sub arrayOfStructsTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $array = shift;
  my $curly_sum = 0;
  for my $struct (@$array) {
    $curly_sum += $struct->{'curly'};
  }
  return $curly_sum;
}

sub easyStructTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $struct = shift;
  return $struct->{'moe'} + $struct->{'larry'} + $struct->{'curly'};
}

sub echoStructTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  return shift;
}

sub manyTypesTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  pop;
  return [@_];
}

sub moderateSizeArrayCheck { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $array = shift;
  return join('', $array->[0], $array->[-1]);
}

sub nestedStructTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $calendar = shift;
  my $april_1_2000 = $calendar->{'2000'}{'04'}{'01'};
  return ($april_1_2000->{moe} + $april_1_2000->{larry}
    + $april_1_2000->{curly});
}

sub simpleStructReturnTest { shift if UNIVERSAL::isa($_[0] => __PACKAGE__);
  my $number = shift;
  return +{ 
    times10 => $number * 10,
    times100 => $number * 100,
    times1000 => $number * 1000 
  };
}
