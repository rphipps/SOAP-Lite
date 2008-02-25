use Test::More;
my @perl_from = ('/opt/perl5.6.2/bin/perl',
	'/opt/perl-5.8.7/bin/perl',
	'/opt/perl5.10/bin/perl',
	'/usr/bin/perl');
plan tests => scalar @perl_from;
for my $perl (@perl_from) {
	if (system("make clean > /dev/null 2>&1; \\
		$perl Makefile.PL --noprompt > /dev/null 2>&1 \\
		&& make > /dev/null 2>&1 \\
		&& make test > /dev/null 2>&1")
	) {
		fail $perl;
	} 
	else {
		pass $perl;
	}
}
