interface Range {
    low: number
    high: number
}

function getRanges(rangeInput: string): Range[] {
    const ranges = rangeInput.split('\n');
    return ranges.map(r => {
        const [low, high] = r.split('-').map(x => parseInt(x));
        return {
            low,
            high
        }
    });
}


export function part1(input: string) {
    const [rangeInput, idInput] = input.split('\n\n');
    const rangeNums = getRanges(rangeInput);
    //console.log(rangeNums);
    const ids = idInput.split('\n').map(x => parseInt(x));
    //console.log(ids);
    return ids.reduce((acc, id) => acc + (rangeNums.some(r => r.low <= id && r.high >= id) ? 1 : 0), 0);
}

function collapseRange(range: Range[]) : [Range[], number] {
    const collapsedRanges: Range[] = []
    var numCollapse = 0;
    for (var r of range) {
        const overlappingRangeIdx = collapsedRanges.findIndex(cr => ((cr.low <= r.high && r.low <= cr.high) || (cr.high >= r.low && r.high >= cr.low)));
        if (overlappingRangeIdx >= 0) {
            const overlappingRange = collapsedRanges[overlappingRangeIdx];
            const newRange = {
                low: Math.min(r.low, overlappingRange.low),
                high: Math.max(r.high, overlappingRange.high)
            }
            collapsedRanges[overlappingRangeIdx] = newRange;
            numCollapse++;
        } else {
            collapsedRanges.push(r);
        }
        console.log(collapsedRanges);
    }
    return [collapsedRanges, numCollapse];
}


export function part2(input: string) {
    const [rangeInput] = input.split('\n\n');
    var rangeNums = getRanges(rangeInput);
    var numCollapsed = 0;
    do {
        [rangeNums, numCollapsed] = collapseRange(rangeNums);
    } while (numCollapsed > 0)
    return rangeNums.reduce((acc, r) => acc + (r.high - r.low + 1), 0);
}