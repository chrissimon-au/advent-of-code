import os
XMAS = "XMAS"
REVERSED_XMAS = XMAS[::-1]

def rotate_90(input: str):
    lines = input.split(os.linesep)
    output = []
    indexes = range(0,len(lines))
    for i, c in enumerate(lines[0]):
        new_line = [lines[r][i] for r in indexes]
        output.append("".join(new_line))
    return os.linesep.join(output)

def count_xmas(input: str):
    all_lines = [
        input,
        rotate_90(input),
    ]
    all = os.linesep.join(all_lines)
    return all.count(XMAS) + all.count(REVERSED_XMAS)