use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	   => 'SOAP::Lite',
    'VERSION_FROM' => 'lib/SOAP/Lite.pm', # finds $VERSION
    'PREREQ_PM'    => { 
      XML::Parser  => 2.23,
      MIME::Base64 => 0,
      URI          => 0,
      HTTP::Daemon => 0,
      Apache       => 0,
      Net::POP3    => 0, 
      MIME::Parser => 0,
      MIME::Lite   => 0, 
      LWP::UserAgent => 0,
      Crypt::SSLeay => 0,
    },
);
