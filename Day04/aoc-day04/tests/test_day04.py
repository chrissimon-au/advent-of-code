from aoc_day04.day04 import *
import pytest


@pytest.mark.parametrize("test_input,expected", [
    ("", 0),
    ("XMAS", 1),
    ("ASDFXMAS.. asdfXMAS234", 2),
    ("ASDFXMAS.. asdfXMAS234\n"
     "BLERG..XMAS...........", 3),
    ("ASDFSAMX..", 1),
    ("ASDFSAMXMAS..XMAS...SAMX", 4),
    ("X\n"
     "M\n"
     "A\n"
     "S",1),
    ("XMAS\n"
     "M  A\n"
     "A  M\n"
     "S  X",3),
     ("X   \n"
      " M  \n"
      "  A \n"
      "   S",1),
    ("XX  \n"
     " M  \n"
     " AA \n"
     " S S",2),
    ("   X\n"
     "  M \n"
     " A  \n"
     "S   ",1),
])
def test_count_xmas(test_input: str, expected: int):
    assert count_xmas(test_input) == expected


@pytest.mark.parametrize("test_input,expected", [
    ("AB\n"
     "CD",
     "AC\n"
     "BD")
])
def test_rotate_90(test_input: str, expected: str):
    assert rotate_90(test_input) == expected

@pytest.mark.parametrize("test_input,expected", [
    ("AB\n"
     "CD",
     "B\n"
     "AD\n"
     "C"),
    ("X   \n"
     " M  \n"
     "  A \n"
     "   S",
     " \n"
     "  \n"
     "   \n"
     "XMAS\n"
     "   \n"
     "  \n"
     " "),
])
def test_rotate_45(test_input: str, expected: str):
    assert rotate_45(test_input) == expected