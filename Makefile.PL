use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

require 5.004;

my $prerequisites = <<EOL;

Module                        Prerequisites     Bundle                  Default
----------------------------- ----------------- ----------------------- -------
Client (SOAP::Transport::HTTP::Client)                                   [yes]
                              LWP::UserAgent
----------------------------- ----------------- ----------------------- -------
Client HTTPS/SSL support (SOAP::Transport::HTTP::Client, require OpenSSL) [no]
                              Crypt::SSLeay
----------------------------- ----------------- ----------------------- -------
Client SMTP/sendmail support (SOAP::Transport::MAILTO::Client)           [yes]
                              MIME::Lite
----------------------------- ----------------- ----------------------- -------
Client FTP support (SOAP::Transport::FTP::Client)                        [yes]
                              Net::FTP          [libnet]
                              IO::File          [IO]
----------------------------- ----------------- ----------------------- -------
Standalone HTTP server (SOAP::Transport::HTTP::Daemon)                   [yes]
                              HTTP::Daemon
----------------------------- ----------------- ----------------------- -------
Apache/mod_perl server (SOAP::Transport::HTTP::Apache, require Apache)    [no]
                              Apache            [mod_perl]
----------------------------- ----------------- ----------------------- -------
FastCGI server (SOAP::Transport::HTTP::FCGI, require FastCGI)             [no]
                              FCGI
----------------------------- ----------------- ----------------------- -------
POP3 server (SOAP::Transport::POP3::Server)                              [yes]
                              Net::POP3         [libnet]
                              MIME::Parser 5.22 [MIME Tools]
----------------------------- ----------------- ----------------------- -------
IO server (SOAP::Transport::IO::Server)                                  [yes]
                              IO::File          [IO]
----------------------------- ----------------- ----------------------- -------
MQ transport support (SOAP::Transport::MQ)                                [no]
                              MQSeries
----------------------------- ----------------- ----------------------- -------
JABBER transport support (SOAP::Transport::JABBER)                        [no]
                              Net::Jabber 1.0021
----------------------------- ----------------- ----------------------- -------
MIME messages [required for POP3, optional for HTTP] (SOAP::MIMEParser)   [no]
                              MIME::Parser 5.22 [MIME Tools]
----------------------------- ----------------- ----------------------- -------
SSL support for TCP transport (SOAP::Transport::TCP)                      [no]
                              IO::Socket::SSL
----------------------------- ----------------- ----------------------- -------
Compression support for HTTP transport (SOAP::Transport::HTTP)            [no]
                              Compress::Zlib    
----------------------------- ----------------- ----------------------- -------
Core package (SOAP::Lite)                                                 yes 
                              XML::Parser 2.23
                              MIME::Base64
                              URI
----------------------------- ----------------- ----------------------- -------
EOL

print <<EOI, map {"$_\n"} grep {/\[(?:yes|no)\]/} split "\n", $prerequisites;

We are about to install SOAP::Lite and for your convenience will provide 
you with list of modules and prerequisites, so you'll be able to choose 
only modules you need for your configuration.

XMLRPC::Lite, UDDI::Lite, and XML::Parser::Lite are included by default.
Installed transports can be used for both SOAP::Lite and XMLRPC::Lite.

EOI

my %prerequisites;
my $auto = ExtUtils::MakeMaker::prompt("\nDo you want to proceed with this configuration?" => 'yes') =~ /^\s*y/i;

foreach (split /(^[-\s]+$)/m, $prerequisites) {
  print unless $auto;
  if (/SOAP::/m &&
      (/\s+(yes)\s*$/m ||
        /\[(yes|no)\]/ && 
          ($auto || ExtUtils::MakeMaker::prompt("\nDo you plan to use this module?", $1) =~ /^\s*(y)/i))) {
    %prerequisites = (%prerequisites, map {/\s+/? split : ($_ => 0)} /^\s{20,}([\w:]+(?:\s+[\d.]+)?)/mg) if $1 =~ /^y/;
  } 
}

my $noncoretests = ExtUtils::MakeMaker::prompt('
During "make test" phase we may run tests with several SOAP servers 
that may take long and may fail due to server/connectivity problems. 

Do you want to perform these tests in addition to core tests?', 'no') =~ /^\s*(y)/i;

my $tests = join ' ', glob ($noncoretests ? 't/*.t' : 't/0*.t');

WriteMakefile(
    'NAME'	   => 'SOAP::Lite',
    'VERSION_FROM' => 'lib/SOAP/Lite.pm', 
    'PREREQ_PM'    => \%prerequisites,
    'EXE_FILES'    => ['bin/SOAPsh.pl', 'bin/XMLRPCsh.pl', 'bin/stubmaker.pl'],
    test           => {TESTS => $tests},
);
