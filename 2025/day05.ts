

export function part1(input: string) {
    const [rangeInput, idInput] = input.split('\n\n');
    const ranges = rangeInput.split('\n');
    const rangeNums = ranges.map(r => {
        const [low, high] = r.split('-').map(x => parseInt(x));
        return {
            low,
            high
        }
    });
    console.log(rangeNums);
    const ids = idInput.split('\n').map(x => parseInt(x));
    console.log(ids);
    return ids.reduce((acc, id) => acc + (rangeNums.some(r => r.low <= id && r.high >= id) ? 1 : 0), 0);
}

export function part2(input: string) {

}