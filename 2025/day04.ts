function isPaperRoll(grid: string[][], x: number, y: number) {
    if (x < 0 || x >= grid[0].length || y < 0 || y >= grid.length) {
        return 0;
    }
    return grid[y][x] == '@' ? 1 : 0;
}

function removeRolls(grid: string[][]): [number, string[][]] {
    var acc = 0;
    const newGrid: string[][] = [[]];
    //console.log(grid);
    for (var y = 0; y < grid[0].length; y++) {
        newGrid[y] = []
        for (var x = 0; x < grid.length; x++) {
            //console.log("checking", x,y);
            if (isPaperRoll(grid, x, y)) {
                //console.log("  ", "is roll, so:")
                const numRolls =
                    isPaperRoll(grid, x - 1, y - 1)
                    + isPaperRoll(grid, x, y - 1)
                    + isPaperRoll(grid, x + 1, y - 1)
                    + isPaperRoll(grid, x - 1, y)
                    + isPaperRoll(grid, x + 1, y)
                    + isPaperRoll(grid, x - 1, y + 1)
                    + isPaperRoll(grid, x, y + 1)
                    + isPaperRoll(grid, x + 1, y + 1);
                if (numRolls < 4) {
                    acc++;
                    newGrid[y][x] = '.';
                }
                else {
                    newGrid[y][x] = '@';
                }
            }
            else {
                newGrid[y][x] = '.';
            }
        }
    }
    return [acc, newGrid]
}

function getGrid(input: string) {
    const rows = input.trim().split('\n');
    return rows.filter(r => r.length > 0).map(r => r.trim().split(''));
}

export function part1(input: string) {
    const grid = getGrid(input);
    const [numRolls, _] = removeRolls(grid)
    return numRolls;
}

export function part2(input: string) {
    var grid = getGrid(input);
    var acc = 0;
    var numRolls = 0;
    do {
        //console.log(grid);
        [numRolls, grid] = removeRolls(grid);
        //console.log("removed", numRolls);
        acc += numRolls;
    } while (numRolls > 0);
    return acc
}