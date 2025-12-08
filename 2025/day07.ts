function isSplitter(line: string, pos: number): boolean {
    return line[pos] == '^';
}

export function part1(input: string) {
    const lines = input.split('\n');
    const manifold = lines.filter((_, i) => i % 2 == 0);
    var tachyons = [manifold[0].indexOf('S')]
    var totalSplits = 0;    
    for (var y = 1; y < manifold.length; y++) {
        const manifoldRow = manifold[y];
        const numSplits = tachyons.reduce((acc, tachyonPos) => acc + (isSplitter(manifoldRow, tachyonPos) ? 1 : 0), 0)
        tachyons = Array.from(new Set(tachyons.flatMap(tachyonPos => isSplitter(manifoldRow, tachyonPos) ? [tachyonPos - 1, tachyonPos + 1] : [tachyonPos])));
        totalSplits += numSplits;
    }
    return totalSplits;
}

export function part2(input: string) {

}