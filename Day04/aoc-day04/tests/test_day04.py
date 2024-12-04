from aoc_day04.day04 import *
import pytest


@pytest.mark.parametrize("test_input,expected", [
    ("", 0),
    ("XMAS", 1),
])
def test_count_xmas(test_input: str, expected: str):
    assert count_xmas(test_input) == expected