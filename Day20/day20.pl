package My::Day20;

use strict;
use warnings;
use warnings FATAL => qw[uninitialized];
use builtin qw(true false trim);
use feature qw(say);
use Test::Simple tests => 7;
use File::Slurp;

my $moves_in_cheat = 2;

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
        my ($self, $position, $neighbour, $cheatTarget) = @_;
        if ($self->isWall($neighbour) && $self->isFree($cheatTarget)) {
            my $startDistance = $self->{_distances}->{posToString($position)};
            my $endDistance = $self->{_distances}->{posToString($cheatTarget)};
            return $endDistance - $startDistance - 2;
        }
        return -1;
    }

    sub countCheatsSavingAtLeast {
        my ($self, $minSaved) = @_;

        my $currentPos = $self->getStart();
        my $end = $self->getEnd();
        my $lastPos = position(-1,-1);

        my $cheatCounter = 0;
        while (not posEqual($currentPos,$end)) {
            

            if ($self->cheatSaver($currentPos,move($currentPos,1,0),move($currentPos,2,0)) >= $minSaved) {
                $cheatCounter = $cheatCounter + 1;
            }

            if ($self->cheatSaver($currentPos,move($currentPos,0,1),move($currentPos,0,2)) >= $minSaved) {
                $cheatCounter = $cheatCounter + 1;
            }

            if ($self->cheatSaver($currentPos,move($currentPos,-1,0),move($currentPos,-2,0)) >= $minSaved) {
                $cheatCounter = $cheatCounter + 1;
            }

            if ($self->cheatSaver($currentPos,move($currentPos,0,-1),move($currentPos,0,-2)) >= $minSaved) {
                $cheatCounter = $cheatCounter + 1;
            }

            my $origPos = $currentPos;
            $currentPos = $self->nextInPath($currentPos,$lastPos);
            $lastPos = $origPos;
        };
        return $cheatCounter;
    }
}

sub count_cheats_saving_at_least {
    my $input = shift;
    my $minSaved = shift;
    my $maze = Maze->new($input);

    return $maze->countCheatsSavingAtLeast($minSaved);
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

ok (count_cheats_saving_at_least(trim('
#######
#S#...#
#...#E#
#######
'),2) eq 2);

ok (count_cheats_saving_at_least(trim('
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
'),0) eq 14+14+2+4+2+3+1+1+1+1+1);

ok (count_cheats_saving_at_least(trim('
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
'),64) eq 1);

my $testdata = read_file('testdata.txt');
my $answer = read_file('testdata.answer.txt');
my $count = count_cheats_saving_at_least($testdata ,100);
ok ($count eq $answer);