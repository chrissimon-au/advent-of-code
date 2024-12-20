package My::Day20;

use strict;
use warnings;
use builtin qw(true false trim);
use feature qw(say);
use Test::Simple tests => 4;

my $moves_in_cheat = 2;

{
    package Maze;
    use List::Util qw(min max);

    sub position {
        my ($col, $row) = @_;
        return { col => $col, row => $row };
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

    sub new {
        my $class = shift;
        my $input = shift;
        my $self = {};
        bless $self, $class;
        $self->parse($input);
        say $self->isWall(position(-5,-5));
        return $self;
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
        (at @_) eq "#";
    }

    sub isFree {
        my $cell = at @_;
        $cell eq "." || 
        $cell eq "S" ||
        $cell eq "E";
    }
}

sub count_cheats_saving_at_least {
    my $maze = Maze->new(shift);

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

ok (count_cheats_saving_at_least(trim('
#######
#S#...#
#...#E#
#######
'),2) eq 2);

