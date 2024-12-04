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

def rotate_45(input: str):
    lines = input.split(os.linesep)
    if (len(lines) != len(lines[0])):
        return ""
    length = len(lines)
    output = []

    for i in range(length-1,-1,-1):
        new_line = []
        for r in range(0,length-i):
            c = i+r
            new_line.append(lines[r][c])
        output.append("".join(new_line))
    
    for i in range(1,length):
        new_line = []
        for c in range(0,length-i):
            r = i+c
            new_line.append(lines[r][c])
        output.append("".join(new_line))

    return os.linesep.join(output)

def rotate_135(input: str):
    lines = input.split(os.linesep)
    if (len(lines) != len(lines[0])):
        return ""
    length = len(lines)
    output = []

    for i in range(0,length):
        new_line = []
        for r in range(0,i+1):
            c = i-r
            new_line.append(lines[r][c])
        output.append("".join(new_line))
    
    for i in range(length-2,-1,-1):
        new_line = []
        for c in range(length-1,i,-1):
            r = i-c
            new_line.append(lines[r][c])
        output.append("".join(new_line))

    return os.linesep.join(output)

def count_xmas(input: str):
    all_lines = [
        input,
        rotate_90(input),
        rotate_45(input),
        rotate_135(input),
    ]
    all = os.linesep.join(all_lines)
    return all.count(XMAS) + all.count(REVERSED_XMAS)

def get_X(input: str):
    return "".join([input[i] for i in range(0,len(input),2)])

def get_linearised_block(input: str):
    linearised = input.replace(os.linesep,"")
    return get_X(linearised)
    
def get_linearised_blocks(input: str):
    lines = input.split(os.linesep)
    blocks = []
    for r in range(0,len(lines)-2):
        for c in range(0,len(lines)-2):
            block = "".join(
                [
                    lines[r][c:c+3],
                    lines[r+1][c:c+3],
                    lines[r+2][c:c+3],
                ])
            blocks.append(get_linearised_block(block))
    return blocks


def count_x_mas(input: str):
    linearised_blocks = \
        get_linearised_blocks(input) + \
        get_linearised_blocks(rotate_90(input))
    print(linearised_blocks)
    return linearised_blocks.count("MMASS")