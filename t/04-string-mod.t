# perl-test
# $Id: 04-string-mod.t,v 1.1 2004/03/15 08:23:15 knok Exp $

use strict;
use Test;

BEGIN { plan tests => 2 };

use File::MMagic;

my $dir = -d 't' ? 't/' : '';


my $m1 = File::MMagic->new($dir . 'test-magic');
my $ret = $m1->checktype_filename($dir . 'test.html');
ok($ret eq 'text/html');
my $m2 = new File::MMagic;
$ret = $m2->checktype_filename($dir . 'test.html');
ok($ret eq 'text/html');
