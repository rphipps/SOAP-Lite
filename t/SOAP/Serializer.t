use strict;
use warnings;
use Test::More tests => 12; #qw(no_plan);

use_ok qw(SOAP::Lite);

my $serializer = SOAP::Serializer->new();

is $serializer->find_prefix('http://schemas.xmlsoap.org/soap/envelope/'), 'soap';

ok my $tag = $serializer->tag('fooxml', {}, undef), 'serialize <fooxml/>';
ok $tag = $serializer->tag('_xml', {}, undef), 'serialize <_xml/>';
eval {
    $tag = $serializer->tag('xml:lang', {}, undef);;
};
like $@, qr{^Element \s 'xml:lang' \s can't \s be \s allowed}x, 'error on <xml:lang/>';
undef $@;
eval {
    $tag = $serializer->tag('xmlfoo', {}, undef);
};
like $@, qr{^Element \s 'xmlfoo' \s can't \s be \s allowed}x, 'error on <xmlfoo/>';


my $xml = $serializer->envelope('fault', faultstring => '>>> foo <<<');
like $xml, qr{\&gt;\&gt;\&gt;}x, 'fault escaped OK';
unlike $xml, qr{\&amp;gt;}x, 'fault escaped OK';

$xml = $serializer->envelope('response', foo => '>>> bar <<<');
like $xml, qr{\&gt;\&gt;\&gt;}x, 'response escaped OK';
unlike $xml, qr{\&amp;gt;}x, 'response escaped OK';

$xml = $serializer->envelope('method', foo => '>>> bar <<<');
like $xml, qr{\&gt;\&gt;\&gt;}x, 'response escaped OK';
unlike $xml, qr{\&amp;gt;}x, 'response escaped OK';
