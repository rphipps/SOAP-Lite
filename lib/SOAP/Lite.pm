# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: SOAP::Lite.pm,v 0.42 2000/11/14 23:14:18 $ 
#
# ======================================================================

package SOAP::Lite;

use 5.004;
use strict;
use vars qw($VERSION);
$VERSION = '0.42';

# ======================================================================

package SOAP::Constants;

use vars qw($NSMASK);

$NSMASK = '[a-zA-Z_:][\w.\-:]*'; 

use vars qw($NEXT_ACTOR $NS_XSD $NS_XSI $NS_ENV $NS_ENC 
            $FAULT_CLIENT $FAULT_SERVER $FAULT_VERSION_MISMATCH
            $HTTP_ON_FAULT_CODE $HTTP_ON_SUCCESS_CODE $CNS);

$FAULT_CLIENT = 'Client';
$FAULT_SERVER = 'Server';
$FAULT_VERSION_MISMATCH = 'VersionMismatch';

$HTTP_ON_SUCCESS_CODE = 200; # OK
$HTTP_ON_FAULT_CODE   = 500; # INTERNAL_SERVER_ERROR

$NEXT_ACTOR = 'http://schemas.xmlsoap.org/soap/actor/next';

# schema namespaces                                    
$NS_XSD = 'http://www.w3.org/1999/XMLSchema';          
$NS_XSI = 'http://www.w3.org/1999/XMLSchema-instance'; 

# soap namespaces
$NS_ENV = 'http://schemas.xmlsoap.org/soap/envelope/';
$NS_ENC = 'http://schemas.xmlsoap.org/soap/encoding/';

# internal mark for current namespace
$CNS = 'xmlns:-';

# ======================================================================

package SOAP::Transport;

use vars qw($AUTOLOAD);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  return $self if ref $self;

  SOAP::Trace::objects('()');
  return bless {} => $class;
}

sub proxy {
  my $self = shift->new;
  my $class = ref $self;

  return $self->{_proxy} unless @_;
  $_[0] =~ /^(\w+):/ or die "proxy: transport protocol not specified\n";
  my $protocol = uc $1; # untainted now
  # https: should be done through Transport::HTTP.pm
  for ($protocol) { s/^HTTPS$/HTTP/; }

  (my $protocol_class = "${class}::$protocol") =~ s/-/_/g;
  no strict 'refs';
  unless (defined %{"$protocol_class\::Client::"} && UNIVERSAL::can("$protocol_class\::Client" => 'new')) {
    eval "require $protocol_class";
    die "Unsupported protocol '$protocol'\n" if $@ =~ /^Can't locate/;
    die if $@;
  }
  $protocol_class .= "::Client";
  return $self->{_proxy} = $protocol_class->new(endpoint => shift, @_);
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->proxy->$method(@_) };
  goto &$AUTOLOAD;
}

# ======================================================================

package SOAP::Header;

use vars qw(@ISA);
@ISA = qw(SOAP::Data);

# ======================================================================

package SOAP::Data;

use vars qw(@ISA @EXPORT_OK);
use Exporter;
use Carp;

@ISA = qw(Exporter);
@EXPORT_OK = qw(name type attr value uri actor encodingStyle);

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless {_attr => {}, _value => [], _signature => []} => $class;
    SOAP::Trace::objects('()');
  }

  if (@_) { my %parameters = @_;
    foreach (keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }
  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(name attr)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
      @_ ? ($self->{$field} = shift, $self->value(@_), return $self) 
         : (return $self->{$field});
    }
  }
  for my $method (qw(type)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
      @_ ? ($self->{$field} = shift, $self->value(@_), return $self) 
         : (defined $self->{$field} || 
            (($self->{$field}) = map {$self->{_attr}->{$_}} # =~/^$SOAP::Constants::NSMASK:(.+)/; $1||$_
                                 grep {/^xsi:$method$/o} keys %{$self->{_attr}}),
            return $self->{$field});
    }
  }
  for my $method (qw(root mustUnderstand)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
      @_ ? ($self->{$field} = (shift() ? 1 : 0), 
            $self->{_attr} = {%{$self->{_attr}}, "~V:$method" => $self->{$field}}, 
            $self->value(@_), return $self) 
         : (return defined $self->{$field} 
              ? $self->{$field} 
              : (($self->{$field}) = map {$self->{_attr}->{$_}} grep {/(^|:)$method$/o} keys %{$self->{_attr}})
           );
    }
  }
  for my $method (qw(actor encodingStyle)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
      @_ ? ($self->{$field} = shift(), 
            $self->{_attr} = {%{$self->{_attr}}, "~V:$method" => $self->{$field}}, 
            $self->value(@_), return $self) 
         : (return defined $self->{$field} 
              ? $self->{$field} 
              : (($self->{$field}) = map {$self->{_attr}->{$_}} grep {/(^|:)$method$/o} keys %{$self->{_attr}})
           );
    }
  }
}

sub urn {
  carp "'SOAP::Data->urn' method is deprecated. Instead use 'uri'" if $^W;
  shift->uri(@_);
}

sub uri {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
  @_ ? ($self->{_uri} = shift, $self->{_attr} = {%{$self->{_attr}}, 'xmlns:~' => $self->{_uri}}, 
        $self->value(@_), return $self) 
     : (return $self->{_uri});
}

sub set_value {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
  $self->{_value} = [@_];
  return $self; 
}

sub value {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
  @_ ? ($self->set_value(@_), return $self) 
     : wantarray ? return @{$self->{_value}} : return $self->{_value}->[0];
}

sub signature {
  my $self = UNIVERSAL::isa($_[0], __PACKAGE__) ? shift->new : __PACKAGE__->new;
  @_ ? ($self->{_signature} = shift, return $self) : (return $self->{_signature});
}

# ======================================================================

package SOAP::Serializer;

use Carp;

BEGIN {
  # namespaces and anonymous data structures
  my $ns   = 0; 
  my $name = 0; 
  my $prefix = 'c-';
  use overload; # protect from overloading stringifications in inherited classes
  sub gen_id   { overload::StrVal(shift) =~ /\((0x\w+)\)/; $1 }
  sub gen_ns   { 'namesp' . ++$ns } 
  sub gen_name { join '', $prefix, 'gensym', ++$name } 
  sub prefix   { $prefix =~ s/^[^\-]+-/$_[1]-/; $_[0]; }
}

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless {
      _uri => 'urn:' . do { (my $uri = __PACKAGE__) =~ s/::/__/g; $uri },
      _indent => 0,
      _autotype => 1,
      _readable => 0,
      _multirefinplace => 0,
      _attr => {
        'xmlns:~C' => $SOAP::Constants::NS_ENC,
        'xmlns:~V' => $SOAP::Constants::NS_ENV,
        'xmlns:xsd' => $SOAP::Constants::NS_XSD,
        'xmlns:xsi' => $SOAP::Constants::NS_XSI,
        '~V:encodingStyle' => $SOAP::Constants::NS_ENC,
      },
      _seen => {},
      _typelookup => {
        base64 => [10, sub {shift =~ /[^\x09\x0a\x0d\x20-\x7f]/}, 'as_base64'],
        int    => [20, sub {shift =~ /^[+-]?\d+$/}, 'as_int'],
        float  => [30, sub {shift =~ /^(-?(?:\d+(?:\.\d*)?|\.\d+|NaN|INF)|([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?)$/}, 'as_float'],
        string => [40, sub {1}, 'as_string'],
      },
      _namespace => 'SOAP-ENV',
      _encodingspace => 'SOAP-ENC',
      _encoding => 'UTF-8',
      _objectstack => {},
      _signature => [],
      _on_nonserialized => sub {carp "Cannot marshall @{[ref shift]} reference" if $^W; return},
    } => $class;
    SOAP::Trace::objects('()');
  }

  if (@_) { my %parameters = @_;
    foreach (keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }
  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(readable indent seen autotype typelookup uri attr
                     namespace encodingspace multirefinplace encoding signature
                     on_nonserialized)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
  for my $method (qw(method fault freeform)) { # aliases for envelope
    *$method = sub { shift->new->envelope($method => @_) }
  }
}

sub header {
  croak "'SOAP::Serializer->header' method is deprecated. Instead use 'SOAP::Header'";
}

sub multiref_object {
  my $self = shift;
  my $object = shift;
  my $id = gen_id($object);
  my $seen = $self->seen;
  $seen->{$id}->{count}++;
  $seen->{$id}->{multiref} = $seen->{$id}->{count} > 1 ? $seen->{$id}->{count} : 0;
  $seen->{$id}->{value} = $object;
  $seen->{$id}->{anchor} = "ref-$id";
  $seen->{$id}->{recursive} ||= 0;
  return $id;
}

sub get_multirefs {
  my $self = shift;
  my $seen = $self->seen;
  return if $self->multirefinplace;
  return sort { ref($a) cmp ref($b) }
         map  { $seen->{$_}->{value} } 
         grep { $seen->{$_}->{multiref} && 
               !$seen->{$_}->{recursive} } keys %$seen;
}

sub recursive_object { shift->seen->{gen_id(shift)}->{recursive} = 1 }

sub is_href { 
  my $self = shift;
  my $seen = $self->seen->{+shift};
  return 0 if $seen->{count}-- == $seen->{multiref} && $seen->{recursive};
  return $seen->{count} < $seen->{multiref}-1 if $self->multirefinplace;
  return $seen->{count} && $seen->{multiref};
}

sub multiref_anchor { 
  my $seen = shift->seen->{+shift};
  return $seen->{multiref} ? $seen->{anchor} : undef;
}

# ----------------------------------------------------------------------

sub qualify { $_[1] =~ /:/ ? $_[1] : join ':', $_[0], $_[1] }

sub encode_object {
  my($self, $object, $name, $type, $attr) = @_;

  return $self->encode_scalar($object, $name, $type, $attr) unless ref $object;

  $self->multiref_object($object);

  use vars '%objectstack';           # we'll play with symbol table 
  local %objectstack = %objectstack; # want to see objects ONLY in the current tree
  # did we see this object in current tree? Seems to be recursive refs
  $self->recursive_object($object) if ++$objectstack{gen_id($object)} > 1;
  # return if we already saw it twice. It should be already properly serialized
  return if $objectstack{gen_id($object)} > 2;

  if (UNIVERSAL::isa($object => 'SOAP::Data')) { 
    $object->name($name) unless defined $object->name;
    my @realvalues = $object->value;
    return [$object->name || gen_name, $object->attr] unless @realvalues;

    my $method = 'as_' . ($object->type || '-'); # dummy type if not defined
    # try to call method specified for this type
    my @values =  map { 
         $self->can($method) && $self->$method($_, $object->name || gen_name, $object->type, $object->attr)
      || $self->typecast($_, $object->name || gen_name, $object->type, $object->attr)
      || $self->encode_object($_, $object->name, $object->type, $object->attr)
    } @realvalues;
    $object->signature([map {join $;, $_->[0], $_->[1]->{'xsi:type'} || ''} @values]) if @values;
    return @values;
  } 
  my($class, $ref) = ($object =~ /(?:(.+)=)?(.+)\(/);
  $class =~ s/::/__/g if defined $class;
  $name = $class if defined $class && !defined $name;
  $type = $class if $self->autotype && !defined $type;
  return 
    $ref eq 'SCALAR' ? $self->encode_scalar($object, $name, $type, $attr) :
    $ref eq 'ARRAY'  ? $self->encode_array($object, $name, $type, $attr) :
    $ref eq 'HASH'   ? $self->encode_hash($object, $name, $type, $attr) :
                       $self->on_nonserialized->($object); 
}

sub encode_scalar {
  my $self = shift; 
  my $value = shift;
  my $name = shift || gen_name;
  my $type = shift;
  my $attr = shift || {};

  # null reference
  return [$name, {%$attr, 'xsi:null' => 1}] unless defined $value;

  # object reference
  return [$name, $attr, [$self->encode_object($$value)], gen_id($value)] if ref $value;

  # defined type
  return [$name, {%$attr, 'xsi:type' => qualify('xsd'=>$type)}, $value] if defined $type;

  # autodefined type 
  if ($self->autotype) {
    my $lookup = $self->typelookup;
    for (sort {$lookup->{$a}->[0] <=> $lookup->{$b}->[0]} keys %$lookup) {
      my $typecast = $lookup->{$_}->[2];
      $lookup->{$_}->[1]->($value) and return $self->can($typecast) 
        ? $self->$typecast($value, $name, $type, $attr) 
# TD: add check for CODE reference. Die on error
        : $typecast->($value, $name, $type, $attr);
    }
  }

  # invariant
  return [$name, $attr, $value];
}

sub encode_array {
  my($self, $array, $name, $type, $attr) = @_;
  my $items = gen_name;

# TD: add support for multidimensional, partially transmitted and sparse arrays
  my @items = map {$self->encode_object($_, $items)} @$array;
  my $num = @items;
  my($arraytype, %types) = '-';
  for (@items) { $arraytype = $_->[1]->{'xsi:type'} || '-'; $types{$arraytype}++ }
  $arraytype = sprintf "%s\[$num]", keys %types > 1 || $arraytype eq '-' ? 'xsd:ur-type' : $arraytype;

  $type = 'Array' if $self->autotype && !defined $type; # make ApacheSOAP users happy
  return [$name || '~V:Array', {%{$attr || {}}, '~C:arrayType' => $arraytype, 'xsi:type' => $type}, [@items], gen_id($array)];
}

sub encode_hash {
  my($self, $hash, $name, $type, $attr) = @_;
  $type = 'SOAPStruct' if $self->autotype && !defined $type; # make ApacheSOAP users happy
  return [$name || '~V:Struct', {%{$attr || {}}, 'xsi:type' => $type}, [map {$self->encode_object($hash->{$_}, $_)} keys %$hash], gen_id($hash)];
}

# ----------------------------------------------------------------------

sub as_base64 {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  require MIME::Base64;
  return [$name, {%{$attr || {}}, 'xsi:type' => '~C:base64'}, MIME::Base64::encode_base64($value,'')];
}

sub as_hex { 
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:hex'}, join '', map {sprintf "%x", ord} split '', $value];
}

sub as_int {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:int'}, $value];
}

sub as_float {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:float'}, $value];
}

sub as_string {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  $value =~ s/([&<])/$1 eq '&'?'&amp;':'&lt;'/eg;
  return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:string'}, $value];
}

sub as_boolean {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:boolean'}, $value ? 'true' : 'false'];
}

sub as_ordered_hash {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, $attr, 
    [map{$self->encode_object(@{$value}[2*$_+1,2*$_])} 0..$#$value/2], 
    $self->multiref_object($value)
  ];
}

sub as_xml {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return [$name, {'_' => 'xml'}, $value];
}

sub typecast {
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return if ref $value; # skip complex object, caller knows how to do with it
  return if $self->autotype && !defined $type; # we don't know, autotype knows
  return [$name, {%{$attr || {}}, defined $type && $type gt '' ? ('xsi:type' => qualify('xsd'=>$type)) : ()}, $value];
}

# ----------------------------------------------------------------------

sub tag {
  my $self = shift;
  my($tag, $attrs, @values) = @_;
  my $value = join '', @values;
  my $indent = $self->readable ? "\n" . ' ' x $self->indent : '';
  # check for special attribute
  return "$indent$value" if exists $attrs->{_} && delete $attrs->{_} eq 'xml'; 
  my $tagattrs = join(' ', '', map { sprintf '%s="%s"', $_, $attrs->{$_} } 
                              grep { defined $attrs->{$_} } keys %$attrs);
  $value gt '' 
    ? sprintf("$indent<%s%s$indent>%s</%s>", $tag, $tagattrs, $value, $tag) 
    : $tagattrs ? sprintf("$indent<%s%s/>", $tag, $tagattrs) : '';
}

sub xmlize {
  my $self = shift;
  my($name, $attrs, $values, $id) = @{+shift}; $attrs ||= {};

  delete $attrs->{$SOAP::Constants::CNS}; # drop internal attribute

# qualify element and attributes with default namespace
  $name =~ s/^~V/$self->namespace/e;
  foreach (grep {/~[VC]/} keys %$attrs) {
    (my $key = $_) =~ s/~([VC])/($1 eq 'V' ? $self->namespace() 
                                           : $self->encodingspace())/e;
    $attrs->{$key} = delete $attrs->{$_};
  }
  if (exists $attrs->{'xmlns:~'}) {
    my $ns = ($name =~ s/^($SOAP::Constants::NSMASK):// ? $1 : gen_ns);
    $attrs->{"xmlns:$ns"} = delete $attrs->{'xmlns:~'}; # ? add 'urn:'
    $name = "$ns:$name";
  }

  local $self->{_current_namespace} = $1 if $name =~ /^($SOAP::Constants::NSMASK):/;
  local $self->{_in_header} = 1 if $name =~ /(^|:)Header$/;

# always qualify namespace for attributes in header
  if ($self->{_in_header}) {
    foreach (grep {$_ && !/:/} keys %$attrs) {
      $attrs->{join ':', $self->namespace(), $_} = delete $attrs->{$_};
    }
  }

# always qualify types 
  foreach (grep {/(^|:)type$/ && $attrs->{$_}} keys %$attrs) {
    $attrs->{$_} = join ':', $self->{_current_namespace} || '~V', $attrs->{$_} if $attrs->{$_} !~ /:/;
    $attrs->{$_} =~ s/~([VC])/($1 eq 'V' ? $self->namespace() 
                                         : $self->encodingspace())/e;
  }

  $name =~ s/^~/$self->{_current_namespace}/e;

  unless ($name =~ /^$SOAP::Constants::NSMASK$/) {
    require MIME::Base64; $name = MIME::Base64::encode_base64($name,'');
    $name =~ s/(=+)$/'-' x length($1)/e; # '=' doesn't allowed in elements
    $attrs->{__} = 'encoded';
  }

  local $self->{_indent} = $self->{_indent} + 2;
  return $self->tag($name, $attrs) unless defined $values;
  return $self->tag($name, $attrs, $values) unless UNIVERSAL::isa($values => 'ARRAY');
  return $self->tag($name, {%$attrs, href => '#' . $self->multiref_anchor($id)}) if $self->is_href($id);
  return $self->tag($name, {%$attrs, id => $self->multiref_anchor($id)}, map {$self->xmlize($_)} @$values);
}

sub serialize { SOAP::Trace::trace('()');
  my $self = shift->new;
  my @encoded = map { $self->encode_object($_) } @_;
  return join '', map { $self->xmlize($_) } 
           @encoded, map { $self->encode_object($_) } $self->get_multirefs;
}

sub envelope { SOAP::Trace::trace('()');
  my $self = shift->new;
  my $type = shift;

  my(@parameters, @header);
  for (@_) { 
    defined $_ && ref $_ && UNIVERSAL::isa($_ => 'SOAP::Header') 
      ? push(@header, $_) : push(@parameters, $_);
  }
  my $header = SOAP::Data->set_value(@header);
  my($body,$parameters);
  if ($type eq 'method') {
    SOAP::Trace::method(@parameters);
    my $method = shift(@parameters) or die "Unspecified method for SOAP call\n";
    $parameters = SOAP::Data->set_value(@parameters);
    $body = SOAP::Data
      -> name($method)
      -> attr({'xmlns:~' => $self->uri})
      -> value(\$parameters);
  } elsif ($type eq 'fault') {
    SOAP::Trace::fault(@parameters);
    $body = SOAP::Data
      -> name('~V:Fault')
      -> attr({'xmlns' => $SOAP::Constants::NS_ENV})
      -> value(\SOAP::Data->set_value(
        SOAP::Data->name(faultcode => qualify($self->namespace => shift(@parameters))),
        SOAP::Data->name(faultstring => shift(@parameters)),
        SOAP::Data->name(detail => do{my $detail = shift(@parameters); ref $detail ? \$detail : $detail}),
      ));
  } elsif ($type eq 'freeform') {
    SOAP::Trace::freeform(@parameters);
    $body = SOAP::Data->set_value(@parameters);
  } else {
    die "Wrong type of envelope ($type) for SOAP call\n";
  }

  $self->seen({}); # reinitialize multiref table
  my($encoded) = $self->encode_object(
    SOAP::Data->name('~V:Envelope' => \SOAP::Data->value(
      SOAP::Data->name('~V:Header' => \$header),
      SOAP::Data->name('~V:Body'   => \$body)
    ))->attr($self->attr)
  );
  $self->signature($parameters->signature) if ref $parameters;
  # add multireferences right after Body if any (add after Header?)
  push(@{$encoded->[2]->[1]->[2]}, map { $self->encode_object($_) } $self->get_multirefs);
  return join '', qq!<?xml version="1.0" encoding="@{[$self->encoding]}" ?>!,
                  $self->xmlize($encoded);
}

# ======================================================================

package SOAP::Parser;

use XML::Parser;

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  SOAP::Trace::objects('()');
  return bless {} => $class;
}

sub decode { SOAP::Trace::trace('()');
  my $self = shift; 
  my $string = shift;

  XML::Parser->new(
    Handlers => {
      Init  => sub { $self->init(@_) },
      Final => sub { shift; $self->final(@_) } 
    }
  )->parse($string);
}

sub init {
  my $self = shift;

  shift->setHandlers(
    Start => sub { shift; $self->start(@_) },
    End   => sub { shift; $self->end(@_)   },
    Char  => sub { shift; $self->char(@_)  } 
  );
}

sub start { push @{shift->{_values}}, [shift, {@_}] }

sub char  { shift->{_values}->[-1]->[3] .= shift }

sub final { shift->{_done} }

sub end {
  my $self = shift; 
  my $done = pop @{$self->{_values}};
  @{$self->{_values}} ? (push @{$self->{_values}->[-1]->[2]}, $done)
                      : ($self->{_done} = $done);
}

# ======================================================================

package SOAP::SOM;

sub BEGIN {
  no strict 'refs';
  my %path = (
    envelope    => '/Envelope',
    body        => '/Envelope/Body',
    header      => '/Envelope/Header',
    headers     => '/Envelope/Header/[>0]',
    fault       => '/Envelope/Body/Fault',
    faultcode   => '/Envelope/Body/Fault/faultcode',
    faultstring => '/Envelope/Body/Fault/faultstring',
    faultactor  => '/Envelope/Body/Fault/faultactor',
    faultdetail => '/Envelope/Body/Fault/detail',
  );
  for my $method (keys %path) {
    *$method = sub { 
      my $self = shift;
      ref $self or return $path{$method};
      $self->valueof($path{$method});
    };
  }
  my %results = (
    method    => '/Envelope/Body/[1]',
    result    => '/Envelope/Body/[1]/[1]',
    paramsin  => '/Envelope/Body/[1]/[>0]',
    paramsout => '/Envelope/Body/[1]/[>1]',
  );
  for my $method (keys %results) {
    *$method = sub { 
      my $self = shift;
      ref $self or return $results{$method};
      defined $self->fault ? undef : $self->valueof($results{$method});
    };
  }
}

# use object in boolean context return true/false on last match
# Ex.: $som->match('//Fault') ? 'failure' : 'success';
use overload fallback => 1, 'bool'  => sub { @{shift->{_current}} > 0 };

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  my $content = shift;
  SOAP::Trace::objects('()');
  return bless { _content => $content, _current => [$content] } => $class;
}

sub valueof {
  my $self = shift;
  local $self->{_current} = $self->{_current}; 
  $self->match(shift) if @_;
  return wantarray ? map {$_->[4]} @{$self->{_current}} 
                   : @{$self->{_current}} ? $self->{_current}->[0]->[4] : undef;
}

sub headerof { # SOAP::Header is the same as SOAP::Data, so just rebless it
  wantarray 
    ? map { bless $_ => 'SOAP::Header' } shift->dataof(@_) 
    : bless shift->dataof(@_) => 'SOAP::Header';
}

sub dataof {
  my $self = shift;
  local $self->{_current} = $self->{_current}; 
  $self->match(shift) if @_;
  return wantarray ? map {$self->_as_data($_)} @{$self->{_current}} 
                   : @{$self->{_current}} ? $self->_as_data($self->{_current}->[0]) : undef;
}

sub namespaceuriof {
  my $self = shift;
  $self->match(shift) if @_;
  return wantarray ? map {$_->[1]->{$SOAP::Constants::CNS}} @{$self->{_current}} 
                   : @{$self->{_current}} ? $self->{_current}->[0]->[1]->{$SOAP::Constants::CNS} : undef;
}

sub _as_data {
  my $self = shift;
  my $pointer = shift;
  SOAP::Data
    -> new(name => $pointer->[0], attr => $pointer->[1])
    -> set_value($pointer->[4]);
}

sub match { 
  my $self = shift;
  my $path = shift;
  $self->{_current} = [
    $path =~ s!^/!! || !@{$self->{_current}}
      ? $self->_traverse($self->{_content}, 1 => split '/' => $path)
      : map {$self->_traverse_tree($_->[2], split '/' => $path)} @{$self->{_current}}
  ];
  return $self;
}

sub _traverse {
  my $self = shift;
  my($pointer, $itself, $path, @path) = @_;
  my($op, $num) = $path =~ /^\[(<=|<|>=|>|=|!=?)?(\d+)\]$/ if defined $path;

  return $pointer unless defined $path;

  $op = '==' unless $op; $op .= '=' if $op eq '=' || $op eq '!';
  my $numok = defined $num && eval "$itself $op $num";
  my $nameok = ($pointer->[0] || '') =~ /(^|:)$path$/ if defined $path; # name can be with namespace

  my $anynode = $path eq '';
  unless ($anynode) {
    if (@path) {
      return if defined $num && !$numok || !defined $num && !$nameok;
    } else {
      return $pointer if defined $num && $numok || !defined $num && $nameok;
      return;
    }
  }

  my @walk;
  push @walk, $self->_traverse_tree([$pointer], @path) if $anynode;
  push @walk, $self->_traverse_tree($pointer->[2], $anynode ? ($path, @path) : @path);
  return @walk;
}

sub _traverse_tree {
  my $self = shift;
  my($pointer, @path) = @_;
  my $itself = 1;
  grep {defined} map {$self->_traverse($_, $itself++, @path)} @$pointer;
}

# ======================================================================

package SOAP::Deserializer;

sub DESTROY { SOAP::Trace::objects('()') }

sub BEGIN {
  no strict 'refs';
  for my $method (qw(ids hrefs)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
}

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;
  return $self if ref $self;

  SOAP::Trace::objects('()');
  return bless {
    _ids => {}, 
    _hrefs => {},
    _parser => SOAP::Parser->new,
  } => $class;
}

sub parser { shift->{_parser} }

sub deserialize { SOAP::Trace::trace('()');
  my $self = shift->new;

  $self->ids({})->hrefs({}); # initialize 

  # TBD: find better way to signal parsing errors
  my $parsed = $self->parser->decode(shift);
  $self->traverse_ids($parsed);
  $self->decode_object($parsed);
  return SOAP::SOM->new($parsed);
}

sub traverse_ids {
  my $self = shift;
  my $ref = shift;
  my($undef, $attrs, $childs) = @$ref;
  #  ^^^^^^ to fix nasty error on Mac platform (Carl K. Cunningham)

  $self->ids->{$attrs->{id}} = $ref if exists $attrs->{id};
  map {$self->traverse_ids($_)} @$childs if ref $childs;
}

sub decode_object {
  my $self = shift;              
  my $ref = shift;

  # decode element name if special attribute is present
  if (exists $ref->[1]->{__} && delete $ref->[1]->{__} eq 'encoded') {
    $ref->[0] =~ s/(-+)$/'=' x length($1)/e;
    require MIME::Base64; $ref->[0] = MIME::Base64::decode_base64($ref->[0]); 
  }
  my($name, $attrs, $childs, $value) = @$ref;

  use vars qw($ns %uris); # drop namespace from name
  local $ns = ($name =~ s/^($SOAP::Constants::NSMASK):// ? $1 : '');
  local %uris = (%uris, map {$_ => $attrs->{$_}} grep {/^xmlns(:|$)/} keys %$attrs);
  $ref->[1]->{$SOAP::Constants::CNS} = $uris{$ns ? "xmlns:$ns" : "xmlns"} || '';

  return $name => undef if grep {/^xsi:null$/ && $self->as_boolean($attrs->{$_})} keys %$attrs;

  my $id = delete $attrs->{id};
  my $object = $self->decode_value($ref);
  if (defined $id && exists $self->hrefs->{$id}) {
    my $href = $self->hrefs->{$id};
    %$href = %$object if UNIVERSAL::isa($href => 'HASH');
    @$href = @$object if UNIVERSAL::isa($href => 'ARRAY');
    $$href = $$object if UNIVERSAL::isa($href => 'SCALAR');
  }
  return $name => ($ref->[4] = $object);
}

sub decode_value {
  my $self = shift;
  my $ref = shift;
  my($name, $attrs, $childs, $value) = @$ref;

  my($type) = map {$attrs->{$_}} grep {/^xsi:type$/} keys %$attrs;
  my @classes = map {s/^$SOAP::Constants::NSMASK://; $_} 
                grep {defined} $type, $name;
  no strict 'refs'; 
  my($class) = grep { s/__/::/g; $_ } @classes;
  my $method = 'as_' . ($type || '-'); # dummy type if not defined
  if (exists $attrs->{href}) {
    (my $id = delete $attrs->{href}) =~ s/^#//;
    if (exists $self->ids->{$id}) {
      my $obj = $self->decode_value(delete $self->ids->{$id});
      return $self->hrefs->{$id} = ref $obj ? $obj : \$obj;
    } elsif (exists $self->hrefs->{$id}) {
      return $self->hrefs->{$id};
    } else {
      die "Wrong href ($id) in element '$name'\n";
    }
  } elsif ($name =~ /:Array$/ || grep {/:arrayType$/} keys %$attrs) {
    my $res = [map {($self->decode_object($_))[1]} @{$childs || []}];
    return defined $class ? bless($res => $class) : $res;
  } elsif ($name =~ /:Struct$/ || !$self->can($method) && (ref $childs || defined $class && !defined $value)) { 
    my $res = {map {$self->decode_object($_)} @{$childs || []}};
    return defined $class ? bless($res => $class) : $res;
  } else {
    return $self->can($method) && $self->$method($value)
        || $self->typecast($value)
        || $value;
  }
}

# ----------------------------------------------------------------------

sub as_base64  { shift; require MIME::Base64; MIME::Base64::decode_base64(shift) }

sub as_boolean { shift; my $value = shift; $value eq 'true' || $value eq 1 }

sub as_hex     { shift; my $value = shift; $value =~ s/([a-zA-Z0-9]{2})/chr oct '0x'.$1/ge; $value }

sub typecast   { shift; shift }

BEGIN {
  no strict 'refs';
  for my $method (qw(
string float double decimal timeInstant timePeriod month year century 
recurringDate recurringDay timeDuration recurringDuration uriReference
language integer nonPositiveInteger negativeInteger long int short byte
nonNegativeInteger unsignedLong unsignedInt unsignedShort unsignedByte
positiveInteger date time
  )) { my $name = 'as_' . $method; *$name = sub { shift; shift }; }
}

# ======================================================================

package SOAP::Client;

sub BEGIN {
  no strict 'refs';
  for my $method (qw(endpoint code message is_success status)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
}

# ======================================================================

package SOAP::Server::Object;

my %alive;
my %objects;

sub gen_id { shift =~ /\((0x\w+)\)/; $1 }

sub reference {
  my $self = shift;
  my $stamp = time;
  my $object = shift; 
  my $id = $stamp . gen_id($object);

  # this is code for garbage collection
  my $time = time;
  my $type = ref $object;
  my @objects = grep { $objects{$_}->[1] eq $type } keys %objects;
  for (grep { $alive{$type}->(scalar @objects, $time, @{$objects{$_}}) } @objects) { 
    delete $objects{$_}; 
  } 

  $objects{$id} = [$object, $type, $stamp];
  bless { id => $id } => ref $object;
}

sub objects_by_reference { 
  shift; 
  while (@_) { @alive{shift()} = ref $_[0] ? shift : sub { $_[1]-$_[$_[5] ? 5 : 4] > 600 } } 
  keys %alive;
}

sub object {
  my $self = shift;
  my $class = ref($self) || $self;
  my $object = shift;
  return $object unless ref($object) && $alive{ref $object} && exists $object->{id};
  my $reference = $objects{$object->{id}};
  die "Object with specified id couldn't be found\n" unless ref $reference->[0];
  $reference->[3] = time; # last access time
  return $reference->[0]; # reference to actual object
}

sub objects {
  my $self = shift;
  map { ref($_) && exists $alive{ref $_} && exists $_->{id} ? $self->object($_) : $_ } @_;
}

sub references {
  my $self = shift;
  map { ref($_) && exists $alive{ref $_} ? $self->reference($_) : $_ } @_;
}

# ======================================================================

package SOAP::Server::Parameters;

# ======================================================================

package SOAP::Server;

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    my(@params, @methods);
    while (@_) { $class->can($_[0]) ? push(@methods, shift() => shift) : push(@params, shift) }
    $self = bless {
      _dispatch_to => [], 
      _dispatched => [],
      _on_action => sub {},
      _action => '',
    } => $class;
    while (@methods) { my($method, $params) = splice(@methods,0,2);
      $self->$method(ref $params eq 'ARRAY' ? @$params : $params) 
    }
    SOAP::Trace::objects('()');
  }

  if (@_) { my %parameters = @_;
    foreach (keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }

  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(on_action action)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
  for my $method (qw(dispatch_to)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = [@_], return $self) 
         : return @{$self->{$field}};
    }
  }
}

sub objects_by_reference { 
  my $self = shift->new;
  @_ ? (SOAP::Server::Object->objects_by_reference(@_), return $self) : SOAP::Server::Object->objects_by_reference; 
}

sub dispatched {
  my $self = shift->new;
  @_ ? (push(@{$self->{_dispatched}}, @_), return $self) : return @{$self->{_dispatched}};
}

sub handle { SOAP::Trace::trace('()'); 
  my $self = shift;

  my $request = eval { local $SIG{'__DIE__'}; SOAP::Deserializer->deserialize(shift) };
  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Data' => "Application failed during request deserialization")
    if $@;

  return $self->make_fault($SOAP::Constants::FAULT_VERSION_MISMATCH, 'Bad version' => "Expected $SOAP::Constants::NS_ENV")
    if $request->namespaceuriof(SOAP::SOM::envelope) ne $SOAP::Constants::NS_ENV;

  my($method_uri, $method_name) = ($request->namespaceuriof(SOAP::SOM::method), $request->dataof(SOAP::SOM::method)->name);
  $method_name =~ s/^$SOAP::Constants::NSMASK://; # ignore namespace

  eval { local $SIG{'__DIE__'}; $self->on_action->($self->action, $method_uri, $method_name); };
  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad SOAPAction' => $@)
    if $@;

  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad URI' => "URI path shall map to class")
    unless defined (my $class = URI->new($method_uri)->path);

  for ($class) { s!^/!!; s!/!::!g; s/^$/main/; } 
  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Class Name' => "Failed to access class ($class)")
    unless $class =~ /^([\w:]+)$/;

  my $fullname = "$class\::$method_name";
  my $static = grep { 
    $class =~ /^$_$/ ||                          # MODULE
    $fullname =~ /^$_$/ ||                       # MODULE::method
    $method_name =~ /^$_$/ && ($class eq 'main') # method (main assumed)
  } grep {!m![/\\.]!} $self->dispatch_to;        # filter PATH

  no strict 'refs';
  unless (defined %{"${class}::"} && UNIVERSAL::can($class => $method_name)) {   
    # allow all for static and only specified path for dynamic bindings
    local @INC = (($static ? @INC : ()), grep {m![/\\.]!} $self->dispatch_to); # '\' to keep windows guys happy
    eval 'local $SIG{"__DIE__"}; local $^W; ' . "require $class";
    return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Class Name' => "Failed to access class ($class)")
      if $@;
    $self->dispatched($class) unless $static;
  } 

  return $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Class::method Call' => "Denied access to method ($method_name) in class ($class)")
    unless $static || grep {/^$class$/} $self->dispatched;

  my @results = eval { local $SIG{'__DIE__'}; local $^W;
    my($object, @parameters) = $request->paramsin;

    SOAP::Trace::dispatch($fullname);
    SOAP::Trace::parameters(@parameters);

    push @parameters, $request if $class->isa('SOAP::Server::Parameters');
    SOAP::Server::Object->references(
      defined $object && ref $object && UNIVERSAL::isa($object => $class) 
        ? (SOAP::Server::Object->object($object)->$method_name(
             SOAP::Server::Object->objects(@parameters)), 
           $request->headerof(SOAP::SOM::method.'/[1]')->value($object))
        : ($class->$method_name(SOAP::Server::Object->objects($object, @parameters)))
    );
  };

  SOAP::Trace::result(@results);

  return unless defined wantarray; # nothing to do in void context

  # let application errors pass through with 'Server' code
  return ($@ =~ s/ at .*\n//, $@ =~ /^Can't locate object method/) 
    ? $self->make_fault($SOAP::Constants::FAULT_CLIENT, 'Bad Method Call' => "Failed to locate method ($method_name) in class ($class)")
    : $self->make_fault($SOAP::Constants::FAULT_SERVER, 'Application error' => ref $@ ? $@ : "Application failed: $@")
    if $@;

  return SOAP::Serializer
    -> prefix('s')      # distinguish element names from client and server
    -> uri($method_uri) 
    -> envelope(method => $method_name . 'Response', @results);
}

sub make_fault { shift; SOAP::Serializer->fault(@_) }

# ======================================================================

package SOAP::Trace;

use Carp;

my @list = qw(transport dispatch result parameters headers objects method fault freeform trace debug);
{ no strict 'refs'; for (@list) { *$_ = sub {} } }

sub defaultlog { 
  my $caller = (caller(1))[3];
  $caller = (caller(2))[3] if $caller =~ /eval/;
  chomp(my $msg = join ' ', @_); 
  printf STDERR "%s: %s\n", $caller, $msg;
} 

sub import { no strict 'refs';
  my $pack = shift;
  my(@notrace, @symbols);
  for (@_) {
    if (ref eq 'CODE') {
      my $call = $_;
      foreach (@symbols) { *$_ = sub { $call->(@_) } }
      @symbols = ();
    } else {
      local $_ = $_;
      my $minus = s/^-//;
      my $all = $_ eq 'all';
      carp "Illegal symbol for tracing ($_)" unless $all || $pack->can($_);
      $minus ? push(@notrace, $all ? @list : $_) : push(@symbols, $all ? @list : $_);
    }
  }
  foreach (@symbols) { *$_ = \&defaultlog }
  foreach (@notrace) { *$_ = sub {} }
}

# ======================================================================

package SOAP::Lite;

use vars qw($AUTOLOAD);
use Carp;
use URI;

my $soap;

sub autodispatched { $soap } # access to autodispatched object

sub import {
  my $pkg = shift;
  while (@_) {
    my $command = shift;
    my @parameters = UNIVERSAL::isa($_[0], 'ARRAY') ? @{shift()} : splice(@_); 
    if ($command eq 'autodispatch') { 
      $soap = ($soap||$pkg)->new(@parameters);
      no strict 'refs'; local $^W; # no AUTOLOAD redefined warnings
      *UNIVERSAL::AUTOLOAD = sub {
        my($package, $method) = $AUTOLOAD =~ m/(?:(.+)::)([^:]+)$/;
        return if $method eq 'DESTROY';
  
        my $uri = URI->new($soap->uri);
        my $currenturi = $uri->path;
        $package = 
          $package eq 'SOAP' ? ref $_[0] || ($_[0] eq 'SOAP' 
            ? $currenturi || croak "URI is not specified for SOAP call" : $_[0]) :
          $package eq 'main' ? $currenturi || $package  
                             : $package;
        # drop first parameter if it's a class name
        shift @_ if !ref $_[0] && $_[0] eq $package;
  
        $uri->path($package);
  
        my $som = $soap->uri($uri->as_string)->call($method => @_);
        ref $som ? $som->result : $som;
      };
    } elsif ($command eq 'debug' || $command eq 'trace') { 
      SOAP::Trace->import(@parameters ? @parameters : 'all');
    } else {
      $soap = $pkg->new($command, @parameters);
    }
  }
}

sub DESTROY { SOAP::Trace::objects('()') }

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = bless {
      _transport => SOAP::Transport->new,
      _serializer => SOAP::Serializer->new,
      _deserializer => SOAP::Deserializer->new,
      _default => \$soap,
    } => $class;
   
    $self->on_action($self->on_action || sub { sprintf '"%s#%s"', @_ });
    $self->on_fault($self->on_fault || sub {ref $_[1] ? return $_[1] : croak "SOAP call failed: ", $_[0]->transport->status});
    $self->on_nonserialized($self->on_nonserialized || $self->serializer->on_nonserialized);
    SOAP::Trace::objects('()');
  }

  if (@_) { my %parameters = @_;
    foreach (keys %parameters) {
      $self->$_($parameters{$_}) if $self->can($_);
    }
  }

  return $self;
}

sub BEGIN {
  no strict 'refs';
  for my $method (qw(endpoint transport serializer deserializer outputxml)) {
    my $field = '_' . $method;
    *$method = sub {
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->{$field};
    }
  }
  for my $method (qw(proxy)) {
    *$method = sub { 
      my $self = shift->new;
      @_ ? ($self->transport->$method(@_), return $self) : return $self->transport->$method();
    }
  }                                                
  for my $method (qw(autotype readable namespace encodingspace multirefinplace encoding typelookup uri header)) {
    *$method = sub { 
      my $self = shift->new;
      @_ ? ($self->serializer->$method(@_), return $self) : return $self->serializer->$method();
    }
  }                                                
  for my $method (qw(on_action on_fault on_nonserialized)) {
    my $field = '_' . $method;
    *$method = sub { 
      my $self = shift->new;
      @_ ? ($self->{$field} = shift, return $self) : return $self->default($field);
    }
  }
}

sub on_debug { 
  my $self = shift; 
  carp "'SOAP::Lite->on_debug' method is deprecated. Instead use 'SOAP::Lite +debug ...'" if $^W;
  SOAP::Trace->import(debug => shift);
  $self;
}

sub default {
  my $self = shift;
  my $key = shift;
  local $^W;
  return $self->{$key} 
      || (ref ${$self->{_default}} && ${$self->{_default}}->{$key}) 
      || undef;
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  no strict 'refs';
  *$AUTOLOAD = sub { shift->call($method => @_) };
  goto &$AUTOLOAD;
}

sub call { SOAP::Trace::trace('()');
  my $self = shift;
  (my $method = $_[0]) =~ s/^$SOAP::Constants::NSMASK://; # drop namespace from method
  $self->serializer->on_nonserialized($self->on_nonserialized);
  my $respond = $self->transport->send_receive(
    endpoint    => $self->endpoint, 
    action      => $self->on_action->($self->uri, $method),
    envelope    => $self->serializer->envelope(method => shift, @_), 
  );

  return $respond if $self->outputxml;

  unless ($self->transport->is_success) {
    my $result = eval { $self->deserializer->deserialize($respond) } if $respond;
    return $self->on_fault->($self, $@ ? $respond : $result) || $result;
  }

  return unless $respond; # nothing to do for one-ways
  my $result = $self->deserializer->deserialize($respond);

  # little bit tricky part that binds in/out parameters
  if (($result->paramsout || $result->headers) && $self->serializer->signature) {
    my $num = 0;
    my %signatures = map {s/(^|$;)$SOAP::Constants::NSMASK:/$1/; $_ => $num++} @{$self->serializer->signature};
    for ($result->dataof(SOAP::SOM::paramsout), $result->dataof(SOAP::SOM::headers)) {
      my $signature = join $;, map {s/^$SOAP::Constants::NSMASK://; $_} $_->name, $_->type;
      if (exists $signatures{$signature}) {
        my $param = $signatures{$signature};
        my($value) = $_->value; # take first value
        UNIVERSAL::isa($_[$param] => 'SOAP::Data') ? $_[$param]->value($value) :
        UNIVERSAL::isa($_[$param] => 'ARRAY')      ? (@{$_[$param]} = @$value) :
        UNIVERSAL::isa($_[$param] => 'HASH')       ? (%{$_[$param]} = %$value) :
        UNIVERSAL::isa($_[$param] => 'SCALAR')     ? (${$_[$param]} = $$value) :
                                                     ($_[$param] = $value)
      }
    }
  }
  return $result;
}

# ======================================================================

1;

__END__

=head1 NAME

SOAP::Lite - Client and server side SOAP implementation

=head1 SYNOPSIS

  use SOAP::Lite;
  print SOAP::Lite 
    -> uri('http://simon.fell.com/calc')
    -> proxy('http://www.razorsoft.net/ssss4c/soap.asp')
    -> doubler([10,20,30,50,100])
    -> result ->[1];
 

  The same code with autodispatch: 

  use SOAP::Lite +autodispatch => 
    uri => 'http://simon.fell.com/calc',
    proxy => 'http://www.razorsoft.net/ssss4c/soap.asp'
  ;

  print doubler([10,20,30,50,100])->[2];                             


  Code for SOAP server (CGI) looks like: 

  use SOAP::Transport::HTTP;
  SOAP::Transport::HTTP::CGI
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'Module::Name', 'Module::method') 
    -> handle;

=head1 DESCRIPTION

SOAP::Lite for Perl is a collection of Perl modules which provides a 
simple and lightweight interface to the Simple Object Access Protocol 
(SOAP) both on client and server side.

To learn more about SOAP, visit the FAQ at http://www.develop.com/soap/soapfaq.htm.

This version of SOAP::Lite supports the SOAP 1.1 specification.
See http://www.w3.org/TR/SOAP for details.

The main features of the library are:

=over 3

=item *

Supports SOAP 1.1 spec.

=item *

Provides full namespace support for SOAP 1.1.

=item *

Contains various reusable components (modules) that can be
used separately or together, like SOAP::Serializer and SOAP::Deserializer.

=item *

Provides an object oriented interface for serializing/deserializing and
sending/receiving SOAP packets.

=item *

Supports serialization/deserialization of sophisticated object graphs
which may have cycles (a circular queue would serialize just fine,
as well as $a=\$a. See test.pl and documentation for more examples).

=item *

Has more than 40 tests that access public test servers with different 
implementations: Apache SOAP, Frontier, Perl, XSLT, COM and VB6.

=item *

Support for extensibility of the serialization/deserialization
architecture has been included; see SOAP::Data for details.

=item *

Supports blessed object references.

=item *

Supports arrays (both serialization and deserialization with autotyping).

=item *

Supports ordered hashes (as working example of user-defined data types).

=item *

Custom/user-defined types (see SOAP::Data::as_ordered_hash for example).

=item *

Customizable auto type definitions.

=item *

Supports Base64 encoding.

=item *

Supports XML entity encoding.

=item *

Supports header attributes.

=item *

Supports out parameters binding.

=item *

Supports transparent SOAP calls with autodispatch feature.

=item *

Supports dynamic/static class/method binding.

=item *

Provides CGI/daemon server implementation

=item *

Supports HTTPS protocol

=item *

Supports SMTP protocol

=item *

Provides POP3 server implementation

=item *

Supports Basic/Digest server authentication

=item *

Provides shell for interactive SOAP sessions. See SOAPsh.pl.

=item *

Easy services deployment. Just put module in specified directory and 
it'll be accessible.

=back

=head2 WHERE TO FIND EXAMPLES

See test.pl, examples/*.pl and module documentation for a client-side 
examples that show the serialization of a SOAP request, sending it over 
HTTP and receiving a response, and the deserialization of the response.
See examples/soap.cgi, examples/soap.daemon and examples/My/Apache.pm
for server implementations.

=head1 OVERVIEW OF CLASSES AND PACKAGES

This table should give you a quick overview of the classes provided by the
library.

 SOAP::Lite.pm
 -- SOAP::Lite         -- Main class provides all logic
 -- SOAP::Transport    -- Supports transport architecture
 -- SOAP::Data         -- Provides extensions for serialization architecture
 -- SOAP::Header       -- Provides extensions for Header serialization
 -- SOAP::Parser       -- Parse XML file into object tree
 -- SOAP::Serializer   -- Serializes data structures to SOAP package
 -- SOAP::Deserializer -- Deserializes result of SOAP::Parser into objects
 -- SOAP::SOM          -- Provides access to deserialized object tree
 -- SOAP::Constants    -- Provides access to common constants
 -- SOAP::Trace        -- Provides tracing facilities
 -- SOAP::Server::Object -- Internal class implements objects-by-reference 

 SOAP::Transport::HTTP.pm
 -- SOAP::Transport::HTTP::Client  -- Client interface to HTTP transport
 -- SOAP::Transport::HTTP::Server  -- Server interface to HTTP transport
 -- SOAP::Transport::HTTP::CGI     -- CGI implementation of server interface
 -- SOAP::Transport::HTTP::Daemon  -- Daemon implementation of server interface
 -- SOAP::Transport::HTTP::Apache  -- mod_perl implementation of server interface

 SOAP::Transport::POP3.pm
 -- SOAP::Transport::POP3::Server  -- Server interface to POP3 protocol

 SOAP::Transport::MAILTO.pm
 -- SOAP::Transport::MAILTO::Client -- Client interface to SMTP/sendmail

 SOAP::Transport::LOCAL.pm
 -- SOAP::Transport::LOCAL::Client -- Client interface to local transport

 SOAP::Transport::TCP.pm
 -- SOAP::Transport::TCP::Server -- Server interface to TCP protocol
 -- SOAP::Transport::TCP::Client -- Client interface to TCP protocol

=head2 SOAP::Lite

All methods that SOAP::Lite gives you access to can be used for both
setting and retrieving values. If you provide no parameters, you'll
get current value, and if you'll provide parameter(s), new value
will be assigned and method will return object (if not stated something
else). This is suitable for stacking these calls like:

  $lite = SOAP::Lite
    -> uri('http://simon.fell.com/calc')
    -> proxy('http://www.razorsoft.net/ssss4c/soap.asp')
  ;

Order is insignificant and you may call new() method first. If you
don't do it, SOAP::Lite will do it for you. However, new() method
gives you additional syntax:

  $lite = new SOAP::Lite
    uri => 'http://simon.fell.com/calc',
    proxy => 'http://www.razorsoft.net/ssss4c/soap.asp'
  ;

new() accepts hash with method names and values, and will call 
appropriate method with passed value.

Since new() is optional it won't be mentioned anymore.

Other available methods are:

=over 4

=item transport()

Provides access to SOAP::Transport object. Object will be created for 
you. You can reassign it (but generally you should not).

=item serializer()

Provides access to L</SOAP::Serialization> object. Object will be created for 
you. You can reassign it (but generally you should not).

=item proxy()

Shortcut for C<transport-E<gt>proxy()>. Lets you specify endpoint and 
load required module at the same time. Required for dispatching SOAP 
calls. Name of the module will be defined depending on protocol 
specified for endpoint. Prefix SOAP::Transport will be appended,
module loaded and object of class (with appended ::Client) will
be created. 
For example, for 'http://localhost/' class for create object will 
look like SOAP::Transport:HTTP::Client;

=item endpoint()

Lets you specify endpoint B<without> changing/loading protocol module. 
Usable for changing endpoints without changing protocols. You should 
call proxy() first. No checks for protocol equality will be made.

=item outputxml()

Lets you specify output from all methods call. If C<true>, all methods
will return unprocessed raw xml. You can parsed it with XML::Parser,
SOAP::Deserializer or any other module that will work for you.

=item autotype()

Shortcut for C<serializer-E<gt>autotype()>. Lets you specify will serializer
try to make autotyping for you or not. Default setting is C<true>.

=item readable()

Shortcut for C<serializer-E<gt>readable()>. Lets you specify format for
generated xml code. Carriage returns and indentation will be added for
readability. Usable when you want to see generated code in debugger. 
By default there are no additional characters in generated xml code. 

=item namespace()

Shortcut for C<serializer-E<gt>namespace()>. Lets you specify default
namespace for generated envelope. 'SOAP-ENV' by default.

=item encodingspace()

Shortcut for C<serializer-E<gt>encodingspace()>. Lets you specify default
encoding namespace for generated envelope. 'SOAP-ENC' by default.

=item encoding()

Shortcut for C<serializer-E<gt>encoding()>. Lets you specify encoding 
for generated envelope. For now it won't actually change envelope
encoding, it'll just modify xml header. 'UTF-8' by default.

=item typelookup()

Shortcut for C<serializer-E<gt>typelookup()>. Gives you access to 
typelookup table that used for autotyping. For more information
see L</SOAP::Serializer>.

=item uri()

Shortcut for C<serializer-E<gt>uri()>. Lets you specify uri for SOAP method.
Default value is provided, however you call will definitely fail if
you don't specify required uri. 

=item multirefinplace()

Shortcut for C<serializer-E<gt>multirefinplace()>. If true, serializer will
put value for multireferences in the first occurence of the reference. 
Otherwise it will be encoded as top intependent element, right after Body.
Default value is 'false'. 

=item header() 

DEPRECATED. Use SOAP::Header instead. 

Shortcut for C<serializer-E<gt>header()>. Lets you specify header for 
generated envelope. You can specify C<root>, C<mustUnderstand> or any
other header using SOAP::Data class:

  $serializer = SOAP::Serializer->envelope('method' => 'mymethod', 1,
    SOAP::Header->name(t1 => 5)->attr({'~V:mustUnderstand' => 1}),
    SOAP::Header->name(t2 => 7)->mustUnderstand(2),
  );

will be serialized into:

  <SOAP-ENV:Envelope ...attributes skipped>
    <SOAP-ENV:Header>
      <t1 xsi:type="xsd:int" SOAP-ENV:mustUnderstand="1">5</t1>
      <t2 xsi:type="xsd:int" SOAP-ENV:mustUnderstand="1">7</t2>
    </SOAP-ENV:Header>
    <SOAP-ENV:Body>
      <namesp1:mymethod xmlns:namesp1="urn:SOAP__Serializer">
        <c-gensym6 xsi:type="xsd:int">1</c-gensym6>
      </namesp1:mymethod>
    </SOAP-ENV:Body>
  </SOAP-ENV:Envelope>

You can mix SOAP::Header parameters with other parameters and also you can
return SOAP::Header parameters as result of remote call and they will be 
placed in header. See My::Parameters::addheader as example.

=item on_action()

Lets you specify handler for on_action event. Triggered for creating
SOAPAction. Default handler will make SOAPAction as C<"uri#method">.
You can change this behavior globally (see L</DEFAULT HANDLERS>) or
locally, for particular object.

=item on_fault()

Lets you specify handler for on_fault event. Default behavior is die 
on transport error and does nothing on others. You can change this 
behavior globally (see L</DEFAULT HANDLERS>) or locally, for particular 
object.

=item on_debug()

Lets you specify handler for on_debug event. Default behavior is do 
nothing. Use +trace/+debug option for SOAP::Lite instead.

=item on_nonserialized()

Lets you specify handler for on_nonserialized event. Default behavior 
is produce warning if warnings are on for everything that cannot be
properly serialized (like CODE references or GLOBs).

=back

=head2 SOAP::Data

You can use this class if you want to specify value, name, type, uri or 
attributes for SOAP elements (use C<value()>, C<name()>, C<type()>, 
C<uri()> and C<attr()> methods correspondingly). 
For example, C<SOAP::Data-E<gt>name('abc')-E<gt>value(123)> will be serialized
to '<abc>123</abc>', as well as SOAP::Data->name(abc => 123).
Each of them except 'value' method can have value as second parameter. 
All methods return current value if you call them without parameters and
return object otherwise, so you can stack them. See test.pl for more 
examples. You can import these methods with: 
   
  SOAP::Data->import('name'); 

or 

  import SOAP::Data 'name'; 

and then use C<name(abc =E<gt> 123)> for brevity. 

Interface for specific attributes is also provided. You can use C<actor()>,
C<mustUnderstand()>, C<encodingStyle()> and C<root()> methods to set/get
values of correspondent attributes.

  SOAP::Data
    ->name(c => 3)
    ->encodingStyle('http://xml.apache.org/xml-soap/literalxml')

should be serialized into:

  <c SOAP-ENV:encodingStyle="http://xml.apache.org/xml-soap/literalxml"
     xsi:type="xsd:int">3</c>

=head2 SOAP::Serializer

Usually you don't need to interact directly with this module. The only 
case when you need it, it's autotyping. This feature lets you specify 
types for your data according to your needs as well as introduce new
data types (like ordered hash for example). 

You can specify type with C<SOAP::Data-E<gt>type(float => 123)> and during
serialization stage module will try to serialize you data with as_float 
method, then call typecast method (you can override it or inherit your 
own class from SOAP::Data) and only then will try to serialize it as 
usual data structure. For example:

  SOAP::Data->type('ordered_hash' => [a => 1, b => 2]) 

will be serialized as ordered hash, using as_ordered_hash method.

If you do not specify type directly serialization module will try
to autodefine type for you according to typelookup hash. It contains 
type name as key and following 3-element array as value:

  priority, 
  check_function (CODE reference), 
  typecast function (METHOD name or CODE reference)

For example, if you want to add uriReference to autodefined types,
you should add something like this:

  $s->typelookup({
    %{$s->typelookup},
    uriReference => [11, sub { shift =~ m!^http://! }, 'as_uriReference']
  });

and add C<as_uriReference> method to L</SOAP::Serializer> class:

  sub SOAP::Serializer::as_uriReference {
    my $self = shift;
    my($value, $name, $type, $attr) = @_;
    return [$name, {%{$attr || {}}, 'xsi:type' => 'xsd:uriReference'}, $value];
  }

Specified methods will work for both autotyping and direct typing, so you
can use either C<SOAP::Data-E<gt>type(uriReference =E<gt> 'http://yahoo.com')> or
just C<'http://yahoo.com'> and it'll be serialized into the same type.

For more examples see as_* methods in SOAP::Serializer.

SOAP::Serializer provides you with autotype(), readable(), namespace(),
encodingspace(), encoding(), typelookup(), uri(), multirefinplace() and 
envelope() methods. All methods except envelope() are described in 
L</SOAP::Lite> section.

=over 4

=item envelope()

Allows you build three kind of envelopes depenfing on the first parameter:

=over 4

=item method

  envelope(method => 'methodname', @parameters);

Lets you build request/response envelope.

=item fault

  envelope(fault => 'faultcode', 'faultstring', 'details');

Lets you build fault envelope. 

=item freeform

  envelope(freeform => 'something that I want to serialize');

Reserved for nonRPC calls. Lets you build you own payload inside SOAP 
envelope. All specification rules are applied, except method specific.

=back

For more examples see test.pl and SOAP::Transport::HTTP.pm

=back

=head2 SOAP::SOM

SOM gives you access to deserialized enveloped with several methods.
All methods accepts node path (similar to XPath notations). 
SOM understands '/' as a root node, '//' as relative location path
('//Body' will find all bodies in document, as well as 
'/Envelope//nums' will find all 'nums' nodes under Envelope node),
'[num]' as node number and '[opnum]' that can be operation ('<', 
'>', '<=', '>=', '!', '=') followed by node number. 
All nodes in nodeset will be returned in document order.

=over 4

=item match()

Accepts path to node and return true/false in boolean context and
SOM object otherwise. valueof() and dataof() can be used to get value(s)
of matched node(s).

=item valueof()

Returns value of (previously) matched node. Can accept node path. 
In that case return value of matched node, but do not change current
node. Suitable when you want to match node and then navigate through
node childs:

  $som->match('/Envelope/Body/[1]'); # match method
  $som->valueof('[1]');              # result
  $som->valueof('[2]');              # first out parameter (if present)

Return value depends on context. In scalar context will return first
element from matched nodeset.

=item dataof()

Same as valueof(), but returns SOAP::Data object, so you can get access
to name, type and attributes of element.

=item headerof()

Same as dataof(), but returns SOAP::Header object, so you can get access
to name, type and attributes of element.  

=item namespaceuriof()

Returns uri associated with matched element. This uri can be inherited.

=back

SOAP::SOM also provides you methods for quick access to Envelope, Body, 
method and parameters (both in and out). All these methods return real
values (in most cases it'll be reference to hash), if called as object
method. Return value also depends on context: in array context it'll 
return you array of values and in scalar context it'll return first
element. So if you want to access first output parameter, you can call
C<$param = $som-E<gt>paramsout>; 
and you'll get it disregarding real number of output parameters. 
If you call it as class function (for example, SOAP::SOM::method)
it returns Xpath string that match current element ('/Envelope/Body/[1]'
in case of 'method'). Method will return undef if not present OR if you
try to access element that has xsi:null="1" attribute. To distinguish
between these two cases you can first access match method that'll 
return true/false in boolean context and then get the real value:

  if ($som->match('//myparameter')) {
    $value = $som->valueof; # can be undef too
  } else {
    # doesn't exist
  }

=over 4

=item envelope()

Returns hash with deserialized envelope. Keys in this hash will be 
'Header' (if present) and 'Body'. Values will be deserialized header
and body correspondingly.
If called as function (SOAP::SOM::envelope) will return Xpath string 
that match envelope content. Usable when you want just match it and 
then iterate content by yourself. Example:

  if ($som->match(SOAP::SOM::envelope)) {
    $som->valueof('Header'); # should give access to header if present
    $som->valueof('Body');   # should give access to body
  } else {
    # hm, are we doing SOAP or what?
  }

=item header()

Returns hash with deserialized header. If you want to get access to 
all attributes in header use:

  # get element as SOAP::Data object 
  $transaction = $som->match(join '/', SOAP::SOM::header, 'transaction')->dataof;
  # then you can access all attributes of 'transaction' element
  $transaction->attr; 

=item headers()

Returns nodeset of deserialized headers. Difference between header() and 
headers() methods is that former gives you access to the whole header and 
later to the headers inside 'Header' tag:

  $som->headerof(join '/', SOAP::SOM::header, '[1]');
  # gives you first header as SOAP::Header object

  ($som->headers)[0];
  # gives you value of the first header, same as
  $som->valueof(join '/', SOAP::SOM::header, '[1]');

  $som->header->{name_of_your_header_here}
  # gives you value of name_of_your_header_here

=item body()

Returns hash with deserialized body. 

=item fault()

Returns value (hash) of Fault elements: faultcode, faultstring and
detail. If Fault element is present, result, paramsin, paramsout and methods 
will return undef value.

=item faultcode()

Returns value of faultcode element if present and undef otherwise.

=item faultstring()

Returns value of faultstring element if present and undef otherwise.

=item faultactor()

Returns value of faultactor element if present and undef otherwise.

=item faultdetail()

Returns value of detail element if present and undef otherwise.

=item method()

Returns value of method element (all input parameters if you call it on 
desetialized request envelope, and result/output parameters if you call it
on deserialized response envelope). Return undef if Fault element is 
present.

=item result()

Returns value of result from method call. In fact, it'll return first
child element (in document order) of method element.

=item paramsin()

Return value(s) of all passed parameters.

=item paramsout()

Return value(s) of output parameters. See following section for details 
and examples.

=back

=head2 SOAP::Trace

SOAP::Trace provides you trace/debug facilities for SOAP::Lite library.
To access it you need to specify list of traceable events:

  use SOAP::Lite +trace =>
    qw(list of available traces here);

Available events are:

 transport  -- (client) access to request/response for transport layer
 dispatch   -- (server) shows full name of dispatched call 
 result     -- (server) result of method call
 parameters -- (server) parameters for method call
 headers    -- (server) headers of received message
 objects    -- (both) new/DESTROY calls
 method     -- (both) parameters for '->envelope(method =>' call
 fault      -- (both) parameters for '->envelope(fault =>' call
 freeform   -- (both) parameters for '->envelope(freeform =>' call
 trace      -- (both) trace enters into some important functions
 debug      -- (both) details about transport 

For example:

  use SOAP::Lite +trace =>
    qw(method fault);

lets you output parameters for all your fault/normal envelopes on STDERR. 
If you want to log it you can either redirect STDERR to some other file, 
for example:

  BEGIN { open(STDERR, '>>....'); }

or (what's better) define your own function for particular event:

  use SOAP::Lite +trace =>
    method => sub {'log messages here'}, fault => \&log_faults;

You can share same function for several events:

  use SOAP::Lite +trace =>
    method, fault => \&log_methods_and_faults;

Also you can use 'all' to get all available tracing and use '-' before to 
disable particular logging:

  use SOAP::Lite +trace =>
    all, -transport; # to get all logging without transport messages

Finally,

  use SOAP::Lite +trace; 

will switch all debugging on.

You can use 'debug' instead of 'trace'. I'd like 'trace', others 'debug'. 
Also on_debug is available for compatibility with old versions, like:

  use SOAP::Lite;

  my $s = SOAP::Lite 
    -> uri('http://tempuri.org/')
    -> proxy('http://beta.search.microsoft.com/search/MSComSearchService.asmx')
    -> on_debug(sub{print@_}) # show you request/response with headers
  ;
  print $s->GetVocabulary(SOAP::Data->name('~:Query' => 'something'))
          ->valueof('//FOUND');

or switch it individually, with

  use SOAP::Lite +trace => debug;

or
  
  use SOAP::Lite +trace => debug => sub {'do_what_I_want_here'};

Compare with: 

  use SOAP::Lite +trace => transport;
 
that gives you access to REAL request/response objects, so you can even 
set/read cookies or do whatever you want there.

Difference between debug and transport is that transport will get 
HTTP::Request/HTTP::Response object and debug will get stringified request 
(NOT OBJECT!) and also can be called in other places too. 

=head2 IN/OUT, OUT PARAMETERS AND AUTOBINDING

SOAP::Lite gives you access to all parameters (both in/out and out) and
also does some additional work for you. Lets consider following example:

  <mehodResponse>
    <res1>name1</res1>
    <res2>name2</res2>
    <res3>name3</res3>
  </mehodResponse>

In that case:

  $result = $r->result; # gives you 'name1'
  $paramout1 = $r->paramsout;      # gives you 'name2', because of scalar context
  $paramout1 = ($r->paramsout)[0]; # gives you 'name2' also
  $paramout2 = ($r->paramsout)[1]; # gives you 'name3'

or

  @paramsout = $r->paramsout; # gives you ARRAY of out parameters
  $paramout1 = $paramsout[0]; # gives you 'res2', same as ($r->paramsout)[0]
  $paramout2 = $paramsout[1]; # gives you 'res3', same as ($r->paramsout)[1]

Generally, if server returns C<return (1,2,3)> you'll get C<1> as result 
and C<2> and C<3> as out parameters.

If server returns C<return [1,2,3]> you'll get ARRAY from result() and 
C<undef> from paramsout() .
Result can be arbitrary complex: it can be array of something, it can
be object, it can be anything and it still be in result() . If only
one parameter is returned paramsout() will return C<undef>.

But there is more.
If you have in your output parameters parameter with the same
signature (name+type) as in input parameters it'll be mapped
automatically. Example:

B<server>:

  sub mymethod {
    shift; # object/class reference
    my $param1 = shift;
    my $param2 = SOAP::Data->name('myparam' => shift() * 2);
    return $param1, $param2;
  }

B<client>:

  $a = 10;
  $b = SOAP::Data->name('myparam' => 12);
  $result = $soap->mymethod($a, $b);

After that, C<$result == 10 and $b-E<gt>value == 24>! Magic? Kind of. 
Autobinding gives it to you. That'll work with objects also with 
one difference: you don't need to worry about name and type of
object parameter. Consider PingPong example (My/PingPong.pm and
examples/pingpong.pl):

B<server>:

  package My::PingPong;

  sub new { 
    my $self = shift;
    my $class = ref($self) || $self;
    bless {_num=>shift} => $class;
  }

  sub next {
    my $self = shift;
    $self->{_num}++;
  }

B<client>:

  use SOAP::Lite +autodispatch 
    => (uri => 'urn:', proxy => 'http://localhost/');

  my $p = My::PingPong->new(10); # $p->{_num} is 10 now, real object returned 
  print $p->next, "\n";          # $p->{_num} is 11 now!, object autobinded

=head2 AUTODISPATCHING

SOAP::Lite provides autodispatching feature that lets you create 
code that looks similar for local and remote access.

For example:

  use SOAP::Lite +autodispatch 
    => (uri => 'urn:/My/Examples', proxy => 'http://localhost/');

tells autodispatch all calls to 'http://localhost/' endpoint with
'urn:/My/Examples' uri. All consequent call can look like:

  print getStateName(1), "\n";
  print getStateNames(12,24,26,13), "\n";
  print getStateList([11,12,13,42])->[0], "\n";
  print getStateStruct({item1 => 10, item2 => 4})->{item2}, "\n";

As you can see, there is no SOAP specific coding at all.

The same logic will work for objects also:

  print "Session iterator\n";
  my $p = My::SessionIterator->new(10);     
  print $p->next, "\n";  
  print $p->next, "\n";   

will access remote My::SessionIterator module, get object, and then call
remote method again. Object will be transferred there, method executed
and result (and modified object!) will be transferred back.

Autodispatch will work B<only> if you don't have the same method in your
code. For example, if you have C<use My::SessionIterator> somewhere in your
code for previous example all methods will be resolved locally with no
SOAP calls. If you want to get access to remote objects/methods even
in that case, use C<SOAP::> prefix to your methods, like:

  print $p->SOAP::next, "\n";  

See pingpong.pl for example of script, that work with the same object
locally and remotely.

You can mix autodispatch and usual SOAP calls in the same code if
you need it. Keep in mind, call with SOAP:: prefix should always be a
method call, so if you want to call function, use C<SOAP->myfunction()>
instead of C<SOAP::myfunction()>.

=head2 ACCESSING HEADERS AND ENVELOPE ON SERVER SIDE

SOAP::Lite gives you easy access to all headers and whole envelope on 
server side. Consider following code from My::Parameters.pm:

  sub byname { 
    my($a, $b, $c) = @{pop->method}{qw(a b c)};
    return "a=$a, b=$b, c=$c";
  }

You'll get this functionality ONLY if you inherit your class from 
SOAP::Server::Parameters class. It should keep old code working and
provide this feature only when you need it.

Every method on server side will be called as class/object method, so it'll
get B<object reference> or B<class name> as the first parameter, then method
parameters, and then envelope as SOAP::SOM object. Shortly:

  $self [, @parameters] , $envelope

If you have fixed number of parameters, you can simple do:

  my $self = shift;
  my($param1, $param2) = @_;

and ignore envelope. If you need access to envelope you can do:

  my $envelope = pop; 

since envelope is always last element in parameters list.
In mentioned byname() method C<pop-E<gt>method> will return hash with
parameter names as keys and values as values. So:

  my($a, $b, $c) = @{pop->method}{qw(a b c)};

gives you by-name access to your parameters.

=head2 SERVICE DEPLOYMENT. STATIC AND DYNAMIC

Let us scrutinize deployment process. Designing your SOAP server you 
can consider two kind of deployment: B<static> and B<dynamic>.
For both static and dynamic deployment you should specify C<MODULE>, 
C<MODULE::method>, C<method> or C<PATH/>. Difference between static and dynamic
deployment is that if module is not present it'll be loaded on
demand. See L</SECURITY> section for detailed description.

Example for B<static> deployment:

  use SOAP::Transport::HTTP;
  use My::Examples;           # module is preloaded 

  SOAP::Transport::HTTP::CGI
    # deployed module should be present here or client will get 'access denied'
    -> dispatch_to('My::Examples') 
    -> handle;

Example for B<dynamic> deployment:

  use SOAP::Transport::HTTP;
  # name is unknown, module will be loaded on demand

  SOAP::Transport::HTTP::CGI
    # deployed module should be present here or client will get 'access denied'
    -> dispatch_to('/Your/Path/To/Deployed/Modules', 'My::Examples') 
    -> handle;

For static deployment you should specify MODULE name directly. 
For dynamic deployment you can specify name either directly (in that 
case it'll be required with no restriction) or indirectly, with PATH
(in that case ONLY path that'll be available will be PATH from 
dispatch_to() parameters). For information how to handle this situation
see L</SECURITY> section.

You should also use statis binding when you have several different classes 
in one file and want to make them available for SOAP calls.

=head2 SECURITY

Due to security reasons if you choose dynamic deployment and specified 
C<PATH/>, current path for perl modules (C<@INC>) will be disabled. 
If you want to access other modules in your included package you have 
several options:

=over 4

=item 1

Switch to static linking:

   use MODULE;
   $server->dispatch_to('MODULE');

It can be usable also when you want to import something specific
from deployed modules: 

   use MODULE qw(import_list);

=item 2

Change C<use> to C<require>. Path is unavailable only during 
initialization part, and it's available again during execution. 
So, if you do C<require> somewhere in your package it'll work.

=item 3

Same thing, but you can do: 

   eval 'use MODULE qw(import_list)'; die if $@;

=item 4

Assign C<@INC> directory in your package and then make C<use>.
Don't forget to put C<@INC> in C<BEGIN{}> block or it won't work:

   BEGIN { @INC = qw(my_directory); use MODULE }

Personally I don't like this method, better options are available.

=back

=head2 OBJECTS-BY-REFERENCE

SOAP::Lite implements experimental (yet fully functional) support for
objects-by-reference. You shouldn't see any differences on client side.
On server side you should specify name of the class you want to return
by reference (instead of by value) in C<objects_by_reference()> method for
your server implementation (see soap.pop3, soap.daemon and Apache.pm).
Garbage collection is done on server side (no early than after 600 
seconds of inactivity time), and you can overload default behavior with 
specific function for any particular class. Binding doesn't have any 
special syntax and implemented on server side (see difference between 
My::SessionIterator and My::PersistentIterator). On client side object 
will have same type as before (C<My::SessionIterator-E<gt>new()> will return 
object of My::SessionIterator type), however this object is just a stub 
with object ID inside.

=head2 DEFAULT HANDLERS

C<use SOAP::Lite> syntax also lets you specify default event handlers 
for your code. Imagine you have different SOAP objects and want to 
share same on_action() (or on_fault() ) handler. You can specify 
on_action() during initialization for every object, but also you can 
do:

  use SOAP::Lite on_action => sub {sprintf '%s#%s', @_};

and this handler will be default handler for all your SOAP objects. 
You can override it if you specify handler for particular object.

See test.pl as example of on_fault() handler.

=head1 BUGS AND LIMITATIONS

=over 4

=item *

No support for multidimensional, partially transmitted and sparse arrays 
(however arrays of arrays are supported, as well as any other data
structures, and you can add your own implementation with L</SOAP::Data>).

=item *

No support for xsd schemas.

=back

=head1 PLATFORMS

=over 4

=item MacOS

Information about XML::Parser for MacPerl could be found here:
http://bumppo.net/lists/macperl-modules/1999/07/msg00047.html

Compiled XML::Parser for MacOS could be found here:
http://www.perl.com/CPAN-local/authors/id/A/AS/ASANDSTRM/XML-Parser-2.27-bin-1-MacOS.tgz

=back

=head1 AVAILABILITY

You can download the latest version SOAP::Lite for Unix or SOAP::Lite for Win32 ( http://geocities.com/paulclinger/soap.html ).
SOAP::Lite is available also from CPAN ( http://search.cpan.org/search?dist=SOAP-Lite ).  
You are very welcome to write mail to author (paulclinger@yahoo.com) 
with your comments, suggestions, bug reports and complains.

=head1 SEE ALSO

You can get SOAP/Perl library from Keith Brown 
( http://www.develop.com/soap/ ) or directly from CPAN. 
I tried introduced as little interactions as possible and hopefully 
you'll be able to use both libraries simultaneously. Let me know if I 
did something wrong and you cannot use them at the same time.

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
