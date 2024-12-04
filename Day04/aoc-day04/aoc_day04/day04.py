XMAS = "XMAS"
REVERSED_XMAS = XMAS[::-1]

def count_xmas(input: str):
    return input.count(XMAS) + input.count(REVERSED_XMAS)