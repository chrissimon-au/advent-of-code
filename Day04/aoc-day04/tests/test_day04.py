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

@pytest.mark.parametrize("file_prefix", [
    "sample",
    "test"
])
def test_count_from_file(file_prefix: str):
    with open(file_prefix + 'data.txt', 'r') as file:
        test_input = file.read()
    with open(file_prefix + 'data.answer.txt', 'r') as file:
        expected = int(file.read())
    assert count_xmas(test_input) == expected



@pytest.mark.parametrize("test_input,expected", [
    ("", 0),
    ("M M\n"
     " A \n"
     "S S",1),
    ("M.M\n"
     ",A2\n"
     "S1S",1),
])
def test_count_x_mas(test_input: str, expected: int):
    assert count_x_mas(test_input) == expected


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

@pytest.mark.parametrize("test_input,expected", [
    ("AB\n"
     "CD",
     "A\n"
     "BC\n"
     "D"),
])
def test_rotate_135(test_input: str, expected: str):
    assert rotate_135(test_input) == expected