use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	   => 'SOAP::Lite',
    'VERSION_FROM' => 'lib/SOAP/Lite.pm', # finds $VERSION
    'PREREQ_PM'    => { XML::Parser => 2.26 }
);
