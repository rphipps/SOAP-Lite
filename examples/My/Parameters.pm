package My::Parameters;

use vars qw(@ISA);
@ISA = qw(SOAP::Server::Parameters);

sub echo {
  my $self = shift;
  pop; # last parameter is envelope (SOAP::SOM object)
       # don't want to echo it
  @_;
}

sub autobind {
  my $self = shift;
  my $param1 = shift;
  my $param2 = SOAP::Data->name('myparam' => shift() * 2);
  return $param1, $param2;
}

sub addheader { 
  my $self = shift;
  my $param1 = shift;
  my $header = pop->headerof(SOAP::SOM::headers);
  return $param1, $header->value($header->value x 2);
}

sub byorder {
  my $self = shift;
  my($a, $b, $c) = @_;
  return "1=$a, 2=$b, 3=$c";
}

sub byname { # input parameter(s), envelope (SOAP::SOM object)
  # pop() will return SOAP::SOM object
  # SOM->method will return structure with parameters {name => value, ...}
  my($a, $b, $c) = @{pop->method}{qw(a b c)};
  return "a=$a, b=$b, c=$c";
}

sub die_simply {
  die 'Something bad happened in our method';
}

sub die_with_object {
  die SOAP::Data->name(something => 'value')->uri('http://www.soaplite.com/');
}

1;