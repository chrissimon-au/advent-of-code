function isPaperRoll(grid: string[][], x: number, y: number) {
    if (x < 0 || x >= grid[0].length || y < 0 || y >= grid.length) {
        return 0;
    }
    return grid[y][x] == '@' ? 1 : 0;
}

export function part1(input: string) {
    const rows = input.trim().split('\n');
    const grid = rows.filter(r => r.length > 0).map(r => r.trim().split(''));
    var acc = 0;
    //console.log(grid);
    for (var x = 0; x < grid[0].length; x++) {
        for (var y = 0; y < grid.length; y++) {
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
                }
            }
        }
    }
    return acc;
}

export function part2(input: string) {

}