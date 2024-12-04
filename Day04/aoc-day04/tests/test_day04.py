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
     "S",1)
])
def test_count_xmas(test_input: str, expected: int):
    assert count_xmas(test_input) == expected


@pytest.mark.parametrize("test_input,expected", [
    ("AB\n"
     "CD",
     "AC\n"
     "BD")
])
def test_rotate(test_input: str, expected: str):
    assert rotate_90(test_input) == expected