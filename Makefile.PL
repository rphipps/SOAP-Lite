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
Standalone server (SOAP::Transport::HTTP::Daemon)                        [yes]
                              HTTP::Daemon
----------------------------- ----------------- ----------------------- -------
Apache/mod_perl server (SOAP::Transport::HTTP::Apache, require Apache)    [no]
                              Apache            [mod_perl]
----------------------------- ----------------- ----------------------- -------
POP3 server (SOAP::Transport::POP3::Server)                              [yes]
                              Net::POP3         [libnet]
----------------------------- ----------------- ----------------------- -------
IO server (SOAP::Transport::IO::Server)                                  [yes]
                              IO::File          [IO]
----------------------------- ----------------- ----------------------- -------
MIME messages, required for POP3, optional for HTTP (SOAP::MIMEParser)    [no]
                              MIME::Parser 5.22 [MIME Tools]
----------------------------- ----------------- ----------------------- -------
Core SOAP package (SOAP::Lite)                                            yes 
                              XML::Parser 2.23
                              MIME::Base64
                              URI
----------------------------- ----------------- ----------------------- -------
EOL

print <<EOI, map {"$_\n"} grep {/\[(?:yes|no)\]/} split "\n", $prerequisites;

We are about to install SOAP::Lite and for your convinience will provide 
you with list of modules and prerequisites, so you'll be able to choose 
only modules you need for your configuration.

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

WriteMakefile(
    'NAME'	   => 'SOAP::Lite',
    'VERSION_FROM' => 'lib/SOAP/Lite.pm', # finds $VERSION
    'PREREQ_PM'    => \%prerequisites,
    'EXE_FILES'     => [ "bin/SOAPsh.pl", "bin/stubmaker.pl" ],
);
