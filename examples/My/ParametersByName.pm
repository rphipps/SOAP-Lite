package My::ParametersByName;

sub echo {
  my $self = shift;
  @_;
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