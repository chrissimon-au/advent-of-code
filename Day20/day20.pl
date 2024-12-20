package My::Day20;

use strict;
use warnings;
use builtin qw(true false trim);
use feature qw(say);
use Test::Simple tests => 3;

my $moves_in_cheat = 2;

sub parse {

}


sub count_cheats_saving_at_least {
    my $maze = $_[0];
    my $threshold = $_[1];
    my $full_moves = ($maze =~ tr/[S\.E]//)-1;
    if ($full_moves-$moves_in_cheat >= $threshold) {
        return 1;
    }
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
