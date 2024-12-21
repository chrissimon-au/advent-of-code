package My::Day20;

use strict;
use warnings;
use warnings FATAL => qw[uninitialized];
use builtin qw(true false trim);
use feature qw(say);
use Test::Simple tests => 9;
use File::Slurp;

{
    package Maze;
    use List::Util qw(min max);
    use JSON qw(to_json);

    sub position {
        my ($col, $row) = @_;
        return { col => $col, row => $row };
    }

    sub move {
        my ($position, $col, $row) = @_;
        return { col => $position->{col} + $col, row => $position->{row} + $row};
    }

    sub posToString {
        my ($position) = @_;
        return "$position->{col},$position->{row}";
    }

    sub posEqual {
        my ($posL, $posR) = @_;
        return $posL->{col} eq $posR->{col} && $posL->{row} eq $posR->{row};
    }


    sub new {
        my $class = shift;
        my $input = shift;
        my $self = {
            _distances => {}
        };
        bless $self, $class;
        $self->parse($input);
        $self->computeDistances();
        return $self;
    }

    sub parse {
        my ($self, $mazeInput) = @_;
        my @mazeRows = split "\n", $mazeInput;
        my @grid;
        for my $rowIdx (0 .. $#mazeRows) {
            my $rowInput = $mazeRows[$rowIdx];
            my @row;
            for my $colIdx (0 .. length($rowInput)-1) {
                my $cell = substr($rowInput,$colIdx,1);
                $row[$colIdx]=$cell;
                if ($cell eq "S") {
                    $self->setStart(position($colIdx,$rowIdx));
                }
                if ($cell eq "E") {
                    $self->setEnd(position($colIdx,$rowIdx));
                }
            }
            $grid[$rowIdx]=\@row;
        }
        $self->setGrid(@grid);
    }

    sub setDistance {
        my ($self, $position, $distance) = @_;
        $self->{_distances}->{posToString($position)} = $distance;
    }

    sub nextInPath {
        my ($self, $position, $lastPos) = @_;
        my $east = move($position,1,0);
        my $south = move($position,0,1);
        my $west = move($position,-1,0);
        my $north = move($position,0,-1);
        my $newPos;
        if ($self->isFree($west) && not posEqual($west, $lastPos)) {
            $newPos = $west;
        }
        if ($self->isFree($east) && not posEqual($east, $lastPos)) {
            $newPos = $east;
        }            
        if ($self->isFree($south) && not posEqual($south, $lastPos)) {
            $newPos = $south;
        }
        if ($self->isFree($north) && not posEqual($north, $lastPos)) {
            $newPos = $north;
        }
        return $newPos;
    }


    sub computeDistances {
        my ($self) = @_;
        my $currentPos = $self->getStart();
        my $end = $self->getEnd();
        my $distance = 0;
        my $lastPos = position(-1,-1);
        while (not posEqual($currentPos,$end)) {
            # say ((posToString($currentPos)) . ": " . $distance);
            # $b = <>;
            $self->setDistance($currentPos, $distance);
            my $origPos = $currentPos;
            $currentPos = $self->nextInPath($currentPos,$lastPos);
            $lastPos = $origPos;
            $distance = $distance + 1;            
        };
        $self->setDistance($currentPos, $distance);
        # say to_json($self->{_distances});
    }

    sub setStart {
        my ($self, $pos) = @_;
        $self->{_start} = $pos;
    }
    sub getStart {
        my ($self) = @_;
        $self->{_start};
    }

    sub setEnd {
        my ($self, $pos) = @_;
        $self->{_end} = $pos;
    }
    sub getEnd {
        my ($self) = @_;
        $self->{_end};
    }

    sub setGrid {
        my ($self, @grid) = @_;
        $self->{_grid} = \@grid;
        my @row = @{$grid[0]};
        $self->{_width} = $#row+1;
        $self->{_height} = $#grid+1;
        # say $self->{_width} . "," . $self->{_height};
    }
    sub getGrid {
        my ($self) = @_;
        return $self->{_grid};
    }
    sub width {
        my ($self) = @_;
        return $self->{_width};
    }
    sub height {
        my ($self) = @_;
        return $self->{_height};
    }

    sub at {
        my ($self, $position) = @_;
        my $row = max(min($position->{row}, $self->{_height}-1),0);
        my $col = max(min($position->{col}, $self->{_width}-1),0);
        return $self->{_grid}[$row][$col];
    }

    sub isWall {
        my ($self, $position) = @_;
        ($self->at($position)) eq "#";
    }

    sub isFree {
        my ($self, $position) = @_;
        my $cell = $self->at($position);
        $cell eq "." || 
        $cell eq "S" ||
        $cell eq "E";
    }

    sub cheatSaver {
        my ($self, $position, ($dc,$dr), $minSaved) = @_;
        my $cheatTarget = move($position,$dc,$dr);
        my $neighbourDeltaCol = $dc eq 0 ? 0 : $dc / abs($dc);
        my $neighbourDeltaRow = $dr eq 0 ? 0 : $dr / abs($dr);
        my $neighbour = move($position,$neighbourDeltaCol,$neighbourDeltaRow);
        my $cheatTargetNeighbour = move($cheatTarget,-$neighbourDeltaCol,-$neighbourDeltaRow);
        my $cheatDistance = abs($dc) + abs($dr);
        # print(to_json($position), to_json($cheatTarget));
        # $a=<>;
        if ($self->isFree($cheatTarget) && $self->isWall($neighbour) && $self->isWall($cheatTargetNeighbour)) {
            my $startDistance = $self->{_distances}->{posToString($position)};
            my $endDistance = $self->{_distances}->{posToString($cheatTarget)};
            return ($endDistance - $startDistance - $cheatDistance) >= $minSaved;
        }
        return false;
    }

    sub countCheatsSavingAtLeast {
        my ($self, $minSaved, $picoSecondLimit) = @_;

        my %cheats;

        my $currentPos = $self->getStart();
        my $end = $self->getEnd();
        my $lastPos = position(-1,-1);

        while (not posEqual($currentPos,$end)) {

            foreach my $cheatDeltaCol (-$picoSecondLimit..$picoSecondLimit) {

                my $rowLimit = $picoSecondLimit-abs($cheatDeltaCol); 

                foreach my $cheatDeltaRow (-$rowLimit..$rowLimit) {
                
                    #say "Testing $cheatDeltaCol,$cheatDeltaRow";
                
                    if ($self->cheatSaver($currentPos,($cheatDeltaCol,$cheatDeltaRow),$minSaved)) {
                        my $cheatId = posToString($currentPos) . "|" . $cheatDeltaCol . "," . $cheatDeltaRow;
                        #say "... is a cheat $cheatId";
                        @cheats{$cheatId} = ()
                    }
                }
            }

            my $origPos = $currentPos;
            $currentPos = $self->nextInPath($currentPos,$lastPos);
            $lastPos = $origPos;
        };
        return scalar keys %cheats;
    }
}

sub count_cheats_saving_at_least {
    my $input = shift;
    my $minSaved = shift;
    my $picoSecondLimit = shift || 2;
    my $maze = Maze->new($input);

    return $maze->countCheatsSavingAtLeast($minSaved, $picoSecondLimit);
}

sub assert_equal {
    my $actual = shift;
    my $expected = shift;
    if ($actual ne $expected) {
        say "Expected, $expected, got $actual";
        return false;
    }
    return true;
}

ok(assert_equal(count_cheats_saving_at_least(trim('
####
#SE#
####
'),0), 0), "no movement");

ok (assert_equal(count_cheats_saving_at_least(trim('
#####
#S#E#
#...#
#####
'),5), 0), "1 cheat, only saves 2");

ok (assert_equal(count_cheats_saving_at_least(trim('
#####
#S#E#
#...#
#####
'),2), 1), "1 cheat, saves 2, is detected");

ok(assert_equal(count_cheats_saving_at_least(trim('
#######
#S#...#
#...#E#
#######
'),2), 2), "2 cheats");

my $sampleData = read_file("sampledata.txt");
ok (assert_equal(count_cheats_saving_at_least($sampleData,0), 44), "AoC Sample, all possible cheats");

ok (assert_equal(count_cheats_saving_at_least($sampleData,64), 1), "AoC Sample - only cheast that save 64 or more");

my $testdata = read_file('testdata.txt');
my $part1Answer = read_file('testdata.answer.txt');
my $count = count_cheats_saving_at_least($testdata, 100);
ok (assert_equal($count, $part1Answer), "AoC Test data part 1");

ok(assert_equal(count_cheats_saving_at_least(trim('
########
#S##...#
#....#E#
########
'),0,3), 2), "1 part 1 cheat and 1 that is only a cheat with extra time in the cheat");

ok(assert_equal(count_cheats_saving_at_least($sampleData,76,20), 3), "AoC Part 2 Sample for saving more than 76");