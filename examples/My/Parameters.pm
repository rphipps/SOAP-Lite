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

1;