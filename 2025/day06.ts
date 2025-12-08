const operators = {
    '+': (a: number, b: number) => a + b,
    '*': (a: number, b: number) => a * b,
}

function compute(problem: string[]) {
    let operands = problem.slice(0, problem.length - 1).map(x => parseInt(x));
    let operator = problem[problem.length - 1];
    return operands.reduce((acc, operand) => operators[operator](acc, operand));
}

function getGrid(input: string) {
    const lines = input.split('\n');
    return lines.map(l => l.trim().split(/\s+/));
}

export function part1(input: string) {
    const grid = getGrid(input);
    const problems: string[][] = [];
    for (var pIdx = 0; pIdx < grid[0].length; pIdx++) {
        const problem = [];
        problems.push(problem)
        for (var opIdx = 0; opIdx < grid.length; opIdx++) {
            problem.push(grid[opIdx][pIdx])
        }
    }
    //console.log(problems);
    return problems.reduce((acc, p) => acc + compute(p), 0)
}

enum RowState {
    BeforeOperand,
    InOperand,
    AfterOperand
}

export function part2(input: string) {
    const lines = input.split('\n');
    const problems: string[][] = [];
    const initBreakFound = () => {
        const breakFound = Array(lines.length - 1);
        for (var i: number = 0; i < breakFound.length - 1; i++) {
            breakFound[i] = RowState.BeforeOperand;
        }
        return breakFound;
    }
    var currentBreaksFound = initBreakFound();
    var currentOperands: string[][] = []
    var currentOperator = "";
    var opIdx = 0;
    for (var colIdx = 0; colIdx < lines[0].length; colIdx++) {
        currentOperands.push([]);

        for (var lineIdx = 0; lineIdx < lines.length - 1; lineIdx++) {
            const ch = lines[lineIdx][colIdx];
            if (ch != ' ') {
                currentOperands[opIdx].push(ch);
                currentBreaksFound[lineIdx] = RowState.InOperand;
            } else {
                if (currentBreaksFound[lineIdx] == RowState.InOperand) {
                    currentBreaksFound[lineIdx] = RowState.AfterOperand;
                }
            }

        }        
        const ch = lines[lines.length-1][colIdx];        
        if (ch && ch != ' ') {
            currentOperator = ch;
        }

        if (currentBreaksFound.every(b => b == RowState.AfterOperand) || colIdx == lines[0].length-1) {
            const operands = currentOperands.map(digits => digits.join('')).filter(x => x != '');
            const problem = operands.concat([currentOperator]);

            problems.push(problem);
            currentOperands = [];
            opIdx = 0;            
            currentOperator = "";
            currentBreaksFound = initBreakFound()
        } else {
            opIdx++;
        }
    }
    //console.log(problems);
    return problems.reduce((acc, p) => acc + compute(p), 0)
}
