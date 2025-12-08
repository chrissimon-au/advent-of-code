function isSplitter(line: string, pos: number): boolean {
    return line[pos] == '^';
}

function getManifold(input: string) {
    const lines = input.split('\n');
    return lines.filter((_, i) => i % 2 == 0);
}

export function part1(input: string) {
    const manifold = getManifold(input);
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
    const manifold = getManifold(input);
    var tachyons: number[] = []
    tachyons[manifold[0].indexOf('S')] = 1;
    console.log(tachyons);
    var totalTimelines = 0;
    for (var y = 1; y < manifold.length; y++) {
        const manifoldRow = manifold[y];
        const newTachyons = [];
        tachyons.forEach((count, tachyonPos) => {
            if (isSplitter(manifoldRow, tachyonPos)) {
                newTachyons[tachyonPos - 1] = (newTachyons[tachyonPos - 1] ?? 0) + count;
                newTachyons[tachyonPos + 1] = (newTachyons[tachyonPos + 1] ?? 0) + count;
            } else {
                newTachyons[tachyonPos] = (newTachyons[tachyonPos] ?? 0) + count;
            }
        });
        tachyons = newTachyons;
        // console.log(manifoldRow);
        // let tachyonRow = Array(manifoldRow.length)
        // tachyons.forEach((c, i) => tachyonRow[i] = c.toString());
        // tachyonRow = [...tachyonRow].map((c, i) => c ? c : ".");
        // console.log(tachyonRow.join(''))
        //console.log("new tachyons", tachyons);
        //console.log("num timelines", tachyons.reduce((acc, i) => acc + i, 0));
    }
    return tachyons.reduce((acc, i) => acc + i, 0);
}