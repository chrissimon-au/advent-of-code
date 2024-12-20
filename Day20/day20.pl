package My::Day20;

use strict;
use warnings;
use builtin qw(true false trim);
use feature qw(say);
use Test::Simple tests => 3;


sub count_cheats_saving_at_least {
    my $maze = $_[0];
    my $limit = $_[1];
    say $maze;
    return 0;
}


ok (count_cheats_saving_at_least(trim('
####
#SE#
####
'),0) eq 0);

ok (count_cheats_saving_at_least(trim('
#####
#S#E#
#...#
#####
'),5) eq 0);

ok (count_cheats_saving_at_least(trim('
#####
#S#E#
#...#
#####
'),2) eq 1);