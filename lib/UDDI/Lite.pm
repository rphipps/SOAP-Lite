# ======================================================================
#
# Copyright (C) 2000 Paul Kulchenko (paulclinger@yahoo.com)
# SOAP::Lite is free software; you can redistribute it
# and/or modify it under the same terms as Perl itself.
#
# $Id: UDDI::Lite.pm,v 0.42 2000/11/14 23:14:18 $ 
#
# ======================================================================

package UDDI::Lite;

use 5.004;
use strict;
use vars qw($VERSION);
$VERSION = '0.42';

use SOAP::Lite;

# ======================================================================

package UDDI::SOM;

use vars qw(@ISA);
@ISA = qw(SOAP::SOM);

sub result { # result should point to immediate child of Body
  my $self = shift;
  my $result = '/Envelope/Body/[1]'; 
  ref $self or return $result;
  defined $self->fault ? undef : $self->valueof($result);
};

# ======================================================================

package UDDI::Data;

use vars qw(@ISA $AUTOLOAD);
@ISA = qw(SOAP::Data);

use overload '""' => sub { shift->value };

sub new {
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = $class->SUPER::new(@_, type => 'uddi');
  }
  return $self;
}

sub AUTOLOAD {
  my($method) = $AUTOLOAD =~ m/([^:]+)$/;
  return if $method eq 'DESTROY';

  # 'name' is already in use, so provide 'Name' as synonym and all others for 
  # consistency. understand [Bb]usinessKey as businessKey
  $method = lcfirst($method);

  no strict 'refs';
  *$AUTOLOAD = sub { 
      my $self = shift;
      return $self->value if $method eq '_';
      my @elements = grep {UNIVERSAL::isa($_ => 'UDDI::Data') && $_->name eq $method} $self->value;
      if (@elements) {
        wantarray ? @elements : $elements[0];
      } elsif (exists $self->attr->{$method}) {
        $self->attr->{$method};
      } else {
        return;
      }
    };
  goto &$AUTOLOAD;
}

# ======================================================================

package UDDI::Serializer;

use vars qw(@ISA);
@ISA = qw(SOAP::Serializer);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = $class->SUPER::new(@_);
    $self->attr({'xmlns:~V' => $SOAP::Constants::NS_ENV});
    $self->autotype(0);
  }
  return $self;
}

sub as_uddi { 
  my $self = shift;
  my($value, $name, $type, $attr) = @_;
  return $self->encode_array($value, $name) if ref $value eq 'ARRAY';
  return $self->encode_hash($value, $name) if ref $value eq 'HASH';
  [$name, {%{$attr || {}}}, ref $value ? [$self->encode_object($value)] : $value];
}                                                                                          

sub encode_array {
  my $encoded = shift->SUPER::encode_array(@_);
  delete $encoded->[1]->{'~C:arrayType'};
  return $encoded;
}

# ======================================================================

package UDDI::Deserializer;

use vars qw(@ISA);
@ISA = qw(SOAP::Deserializer);

sub decode_value {
  my $self = shift;
  my $ref = shift;
  my($name, $attrs, $childs, $value) = @$ref;

  # base class knows what to do with elements in SOAP namespace
  return $self->SUPER::decode_value($ref) 
    if exists $attrs->{href} || $attrs->{'xmlns:~'} eq $SOAP::Constants::NS_ENV;

  $name =~ s/^$SOAP::Constants::NSMASK://; 

  UDDI::Data
    -> new(name => $name, attr => $attrs)
    -> set_value(ref $childs && @$childs ? map(($self->decode_object($_))[1], @$childs) : $value);
}

sub deserialize {
  bless shift->SUPER::deserialize(@_) => 'UDDI::SOM';
}

# ======================================================================

package UDDI::Lite;

use vars qw(@ISA $AUTOLOAD);
@ISA = qw(SOAP::Lite);

sub new { 
  my $self = shift;
  my $class = ref($self) || $self;

  unless (ref $self) {
    $self = $class->SUPER::new(@_);
    $self->on_action(sub{'""'});
    $self->serializer(UDDI::Serializer->new);     # register UDDI Serializer
    $self->deserializer(UDDI::Deserializer->new); # and Deserializer
  }
  return $self;
}

sub call { SOAP::Trace::trace('()'); 
  my $self = shift;
  my $method = shift;
  my @parameters;
  my $attr = ref $_[0] eq 'HASH' ? shift() : {};
  while (@_) {
    push(@parameters, UNIVERSAL::isa($_[0] => 'UDDI::Data') 
      ? shift : UDDI::Data->name(shift, shift));
  }
  my $message = SOAP::Data
    -> name($method => \SOAP::Data->value(@parameters))
    -> attr({xmlns=>'urn:uddi-org:api', generic => '1.0', %$attr});
  $self->serializer->on_nonserialized($self->on_nonserialized);

  my $respond = $self->transport->send_receive(
    endpoint    => $self->endpoint, 
    action      => $self->on_action->($self->uri),
    envelope    => $self->serializer->envelope(freeform => $message), 
  );

  return $respond if $self->outputxml;

  unless ($self->transport->is_success) {
    my $result = eval { $self->deserializer->deserialize($respond) } if $respond;
    return $self->on_fault->($self, $@ ? $respond : $result) || $result;
  }

  return unless $respond; # nothing to do for one-ways
  return $self->deserializer->deserialize($respond);
}

# ======================================================================

1;

__END__

=head1 NAME

UDDI::Lite - Library for UDDI clients in Perl

=head1 SYNOPSIS

  use UDDI::Lite;
  print UDDI::Lite
    -> proxy('http://test.uddi.microsoft.com/inquire')
    -> find_business(name => 'old')
    -> result
    -> businessInfos->businessInfo->serviceInfos->serviceInfo->Name;

  The same code with autodispatch: 

  use UDDI::Lite +autodispatch => 
    proxy => 'http://test.uddi.microsoft.com/inquire'
  ;

  print find_business(name => 'old')
    -> businessInfos->businessInfo->serviceInfos->serviceInfo->Name;                         

=head1 DESCRIPTION

UDDI::Lite for Perl is a collection of Perl modules which provides a 
simple and lightweight interface to the Universal Description, Discovery
and Integration (UDDI) server.

To learn more about UDDI, visit http://www.uddi.org/.

The main features of the library are:

=over 3

=item *

Supports both inquiry and publishing API 

=item *

Builded on top of SOAP::Lite module, hence inherited syntax and features

=item *

Supports HTTPS protocol

=item *

Supports SMTP protocol

=item *

Supports Basic/Digest server authentication

=back

=head1 OVERVIEW OF CLASSES AND PACKAGES

This table should give you a quick overview of the classes provided by the
library.

 UDDI::Lite.pm
 -- UDDI::Lite         -- Main class provides all logic
 -- UDDI::Data         -- Provides extensions for serialization architecture
 -- UDDI::Serializer   -- Serializes data structures to UDDI/SOAP package
 -- UDDI::Deserializer -- Deserializes result into objects
 -- UDDI::SOM          -- Provides access to deserialized object tree

=head2 UDDI::Lite

All methods that UDDI::Lite gives you access to can be used for both
setting and retrieving values. If you provide no parameters, you'll
get current value, and if you'll provide parameter(s), new value
will be assigned and method will return object (if not stated something
else). This is suitable for stacking these calls like:

  $uddi = UDDI::Lite
    -> on_debug(sub{print@_})
    -> proxy('http://test.uddi.microsoft.com/inquire')
  ;

Order is insignificant and you may call new() method first. If you
don't do it, UDDI::Lite will do it for you. However, new() method
gives you additional syntax:

  $uddi = new UDDI::Lite
    on_debug => sub {print@_},
    proxy => 'http://test.uddi.microsoft.com/inquire'
  ;

new() accepts hash with method names and values, and will call 
appropriate method with passed value.

Since new() is optional it won't be mentioned anymore.

Other available methods inherited from SOAP::Lite and most usable are:

=over 4

=item proxy()

Shortcut for C<transport-E<gt>proxy()>. Lets you specify endpoint and 
load required module at the same time. Required for dispatching UDDI/SOAP 
calls. Name of the module will be defined depending on protocol 
specified for endpoint. SOAP::Lite will do the rest work.

=item namespace()

Shortcut for C<serializer-E<gt>namespace()>. Lets you specify default
namespace for generated envelope. 'SOAP-ENV' by default.

=item on_fault()

Lets you specify handler for on_fault event. Default behavior is die 
on transport error and does nothing on others. You can change this 
behavior globally (see L</DEFAULT HANDLERS>) or locally, for particular 
object.

=item on_debug()

Lets you specify handler for on_debug event. Default behavior is do 
nothing. Use +trace/+debug option for UDDI::Lite instead.

=back

=head2 UDDI::Data

You can use this class if you want to specify value and name for UDDI 
elements. 
For example, C<UDDI::Data-E<gt>name('businessInfo')-E<gt>value(123)> will 
be serialized to C<E<lt>businessInfoE<gt>123E<lt>/businessInfoE<gt>>, as 
well as C<UDDI::Data->name(businessInfo =E<gt> 123)>.

If you want to provide names for your parameters you can either specify

  find_business(name => 'old')

or do it with UDDI::Data:

  find_business(UDDI::Data->name(name => 'old'))

Later has some advantages: it'll work on any level, so you can do:

  find_business(UDDI::Data->name(name => UDDI::Data->name(subname => 'old')))

and also you can create arrays with this syntax:

  find_business(UDDI::Data->name(name => 
    [UDDI::Data->name(subname1 => 'name1'), 
     UDDI::Data->name(subname2 => 'name2')]))

will be serialized into:

  <find_business xmlns="urn:uddi-org:api" generic="1.0">
    <name>
      <subname1>name1</subname1>
      <subname2>name2</subname2>
    </name>
  </find_business>

As special case you can pass hash as the first parameter of method
call and values of this hash will be added as attributes to top element:

  find_business({maxRows => 10}, UDDI::Data->name(name => old))

gives you

  <find_business xmlns="urn:uddi-org:api" generic="1.0" maxRows="10">
    ....
  </find_business>

You can also pass back parameters exactly as you get it from method call
(like you probably want to do with authInfo).

You can get access to attributes and elements through the same interface:

  my $list = find_business(name => old);
  my $bis = $list->businessInfos;
  for ($bis->businessInfo) {
    my $s = $_->serviceInfos->serviceInfo;
    print $s->Name,        # element
          $s->BusinessKey, # attribute
          "\n";
  }

Unfortunately name() method is used internally, so for UDDI-specific name()
element provided Name() alias and for all other methods provided aliases
with first letter capitalized (for consistency).

  print $s->businessKey; # gives the same result as
  print $s->BusinessKey; # in last example

=head2 AUTODISPATCHING

UDDI::Lite provides autodispatching feature that lets you create 
code that looks similar for local and remote access.

For example:

  use UDDI::Lite +autodispatch => 
    proxy => 'http://test.uddi.microsoft.com/inquire';

tells autodispatch all UDDI calls to 
'http://test.uddi.microsoft.com/inquire'. All consequent calls can look 
like:

  find_business(name => 'old');
  find_business(UDDI::Data->name(name => 'old'));

=head1 BUGS AND LIMITATIONS

=over 4

=item *

Interface is still subject to change.

=item *

Publishing API is not tested and though HTTPS/SLL is supported you
should specify it yourself for publishing API calls.

=back

=head1 AVAILABILITY

For now UDDI::Lite is distributed as part of SOAP::Lite package that 
you can download from ( http://geocities.com/paulclinger/soap.html ) 
or from CPAN ( http://search.cpan.org/search?dist=SOAP-Lite ).  

=head1 SEE ALSO

SOAP::Lite

=head1 COPYRIGHT

Copyright (C) 2000 Paul Kulchenko. All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 AUTHOR

Paul Kulchenko (paulclinger@yahoo.com)

=cut
