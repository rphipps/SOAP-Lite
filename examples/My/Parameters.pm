package My::Parameters;

sub echo {
  my $self = shift;
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
  my $header = shift;
  return $param1, $header->value($header->value x 2);
}

sub byorder {
  my $self = shift;
  my($a, $b, $c) = @_;
  return "1=$a, 2=$b, 3=$c";
}

sub byname { # input parameter(s), [header(s), ] envelope (SOAP::SOM object)
  # pop() will return SOAP::SOM object
  # SOM->method will return structure with parameters {name => value, ...}
  # use 'grep {ref !~ /^SOAP::/}' if you want to filter specific parameters
  my($a, $b, $c) = @{pop->method}{qw(a b c)};
  return "a=$a, b=$b, c=$c";
}

1;