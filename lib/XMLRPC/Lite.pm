# ======================================================================
#
# Copyright (C) 2000-2001 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: XMLRPC::Lite.pm,v 0.50 2001/04/18 11:45:14 $
#
# ======================================================================

package XMLRPC::Lite;

use SOAP::Lite;
use strict;
use vars qw($VERSION);
$VERSION = '0.50';

# ======================================================================

package XMLRPC::Constants;

BEGIN {
  no strict 'refs';
  for (qw(
    FAULT_CLIENT FAULT_SERVER 
    HTTP_ON_SUCCESS_CODE HTTP_ON_FAULT_CODE
    DO_NOT_USE_XML_PARSER DO_NOT_USE_CHARSET
  )) {
    *$_ = \${'SOAP::Constants::' . $_}
  }
}

# ======================================================================

package XMLRPC::Data;

@XMLRPC::Data::ISA = qw(SOAP::Data);

# ======================================================================

package XMLRPC::Serializer;

@XMLRPC::Serializer::ISA = qw(SOAP::Serializer);

sub new {
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new(
      typelookup => {
        base64 => [10, sub {$_[0] =~ /[^\x09\x0a\x0d\x20-\x7f]/}, 'as_base64'],
        int    => [20, sub {$_[0] =~ /^[+-]?\d+$/}, 'as_int'],
        double => [30, sub {$_[0] =~ /^(-?(?:\d+(?:\.\d*)?|\.\d+|NaN|INF)|([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?)$/}, 'as_double'],
        dateTime => [35, sub {$_[0] =~ /^\d{8}T\d\d:\d\d:\d\d$/}, 'as_dateTime'],
        string => [40, sub {1}, 'as_string'],
      },
      @_
    );
  }
  return $self;
}

sub envelope {
  my $self = shift->new;
  my $type = shift;

  my($body);
  if ($type eq 'method') {
    my $method = shift or die "Unspecified method for XMLRPC call\n";
    if ($method eq 'methodNameResponse') {
      $body = XMLRPC::Data->name(methodResponse => \XMLRPC::Data->value(
        XMLRPC::Data->type(params => [@_])->attr({_preserve => 1})
      ));
    } else {
      $body = XMLRPC::Data->name(methodCall => \XMLRPC::Data->value(
        XMLRPC::Data->type(methodName => UNIVERSAL::isa($method => 'XMLRPC::Data') ? $method->name : $method),
        XMLRPC::Data->type(params => [@_])->attr({_preserve => 1})
      ));
    }
  } elsif ($type eq 'fault') {
    $body = XMLRPC::Data->name(methodResponse => 
      \XMLRPC::Data->type(fault => {faultCode => $_[0], faultString => $_[1]}),
    );
  } else {
    die "Wrong type of envelope ($type) for XMLRPC call\n";
  }

  my($encoded) = $self->encode_object($body);
  return join '', qq!<?xml version="1.0" encoding="@{[$self->encoding]}"?>!,
                  $self->xmlize($encoded);
}

sub encode_object { 
  my $self = shift;
  my @encoded = $self->SUPER::encode_object(@_);
  return $encoded[0]->[0] =~ /^(?:array|struct|i4|int|boolean|string|double|dateTime\.iso8601|base64)$/o 
    ? ['value', {}, [@encoded]] : @encoded;
}

sub encode_array {
  my($self, $array) = @_;

  return ['array', {}, [
    ['data', {}, [map {$self->encode_object($_)} @$array]]
  ]];
}

sub encode_hash {
  my($self, $hash) = @_;

  return ['struct', {}, [
    map {
      ['member', {}, [['name', {}, $_], $self->encode_object($hash->{$_})]]
    } keys %$hash
  ]];
}

sub as_methodName {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return ['methodName', $attr, $value];
}

sub as_params {
  my $self = shift;
  my($params, $name, $type, $attr) = @_;

  return ['params', $attr, [
    map {
      ['param', {}, [$self->encode_object($_)]]
    } @$params
  ]];
}

sub as_fault {
  my($self, $fault) = @_;

  return ['fault', {}, [$self->encode_object($fault)]];
}

sub BEGIN {
  no strict 'refs';
  for my $type (qw(double i4 int)) {
    my $method = 'as_' . $type;
    *$method = sub {
      my($self, $value) = @_;
      return [$type, {}, $value];
    }
  }
}

sub as_base64 {
  my $self = shift;
  my $value = shift;
  require MIME::Base64;
  return ['base64', {}, MIME::Base64::encode_base64($value,'')];
}

sub as_string {
  my $self = shift;
  my $value = shift;
  return ['string', {}, $self->SUPER::can('encode_data')->($value)];
}

sub as_dateTime {
  my $self = shift;
  my $value = shift;
  return ['dateTime.iso8601', {}, $value];
}

sub as_boolean {
  my $self = shift;
  my $value = shift;
  return ['boolean', {}, $value ? 1 : 0];
}

# ======================================================================

package XMLRPC::SOM;

@XMLRPC::SOM::ISA = qw(SOAP::SOM);

sub BEGIN {
  no strict 'refs';
  my %path = (
    root  => '/',
    envelope => '/[1]',
    method => '/methodCall/methodName',
    fault => '/methodResponse/fault',
  );
  for my $method (keys %path) {
    *$method = sub { 
      my $self = shift;
      ref $self or return $path{$method};
      Carp::croak "Method '$method' is readonly and doesn't accept any parameters" if @_;
      $self->valueof($path{$method});
    };
  }
  my %results = (
    result    => '/methodResponse/params/[1]',
    paramsin  => '/methodCall/params/param',
    paramsall => '/methodResponse/params/param',
  );
  for my $method (keys %results) {
    *$method = sub { 
      my $self = shift;
      ref $self or return $results{$method};
      Carp::croak "Method '$method' is readonly and doesn't accept any parameters" if @_;
      defined $self->fault ? undef : $self->valueof($results{$method});
    };
  }
}

# ======================================================================

package XMLRPC::Deserializer;

@XMLRPC::Deserializer::ISA = qw(SOAP::Deserializer);

sub deserialize {
  bless shift->SUPER::deserialize(@_) => 'XMLRPC::SOM';
}

sub decode_value {
  my $self = shift;
  my $ref = shift;
  my($name, $attrs, $childs, $value) = @$ref;

  if ($name eq 'value') {
    $childs ? $self->decode_value($childs->[0]) : $value;
  } elsif ($name eq 'array') {
    return [map {($self->decode_object($_))[1]} @{$childs->[0]->[2] || []}];
  } elsif ($name eq 'struct') { 
    return {map {
      my %hash = map {$_->[0] => $_->[2] || $_->[3]} @{$_->[2] || []};
      ($hash{name} => ($self->decode_object($hash{value}->[0]))[1]);
    } @{$childs || []}};
  } elsif ($name eq 'base64') {
    require MIME::Base64; 
    MIME::Base64::decode_base64($value);
  } elsif ($name =~ /^(?:int|i4|boolean|string|double|dateTime\.iso8601|methodName)$/) {
    return $value;
  } elsif ($name =~ /^(?:params)$/) {
    return [map {($self->decode_object($_))[1]} @{$childs || []}];
  } elsif ($name =~ /^(?:methodResponse|methodCall)$/) {
    return +{map {$self->decode_object($_)} @{$childs || []}};
  } elsif ($name =~ /^(?:param|fault)$/) {
    return ($self->decode_object($childs->[0]))[1];
  } else {
    die "wrong element '$name'\n";
  }
}

# ======================================================================

package XMLRPC::Server::Parameters;

@XMLRPC::Server::Parameters::ISA = qw(SOAP::Server::Parameters);

# ======================================================================

package XMLRPC::Lite;

@XMLRPC::Lite::ISA = qw(SOAP::Lite);

sub new {
  my $self = shift;

  unless (ref $self) {
    my $class = ref($self) || $self;
    $self = $class->SUPER::new(
      serializer => XMLRPC::Serializer->new,
      deserializer => XMLRPC::Deserializer->new,
      on_action => sub {return},
      @_
    );
  }
  return $self;
}

# ======================================================================

1;

__END__

=head1 NAME

XMLRPC::Lite - client and server implementation of XML-RPC protocol 

=head1 SYNOPSIS

=over 4

=item Client

  use XMLRPC::Lite;
  print XMLRPC::Lite
      -> proxy('http://betty.userland.com/RPC2')
      -> call('examples.getStateStruct', {state1 => 12, state2 => 28})
      -> result;

=item CGI server

  use XMLRPC::Transport::HTTP;

  my $server = XMLRPC::Transport::HTTP::CGI
    -> dispatch_to('methodName')
    -> handle
  ;

=item Daemon server

  use XMLRPC::Transport::HTTP;

  my $daemon = XMLRPC::Transport::HTTP::Daemon
    -> new (LocalPort => 80)
    -> dispatch_to('methodName')
  ;
  print "Contact to XMLRPC server at ", $daemon->url, "\n";
  $daemon->handle;

=back

=head1 DESCRIPTION

XMLRPC::Lite is a Perl modules which provides a simple nterface to the
XML-RPC protocol both on client and server side. Based on SOAP::Lite module,
it gives you access to all features and transports available in that module.

See F<t/26-xmlrpc.t> for client examples and F<examples/XMLRPC/*> for server 
implementations.

=head1 DEPENDENCIES

 SOAP::Lite

=head1 SEE ALSO

 SOAP::Lite

=head1 COPYRIGHT

Copyright (C) 2000-2001 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
