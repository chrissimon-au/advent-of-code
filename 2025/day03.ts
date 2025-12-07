function nextDigit(batteries: number[], numBatteries: number) {
    //console.log("nextDigit in", batteries, "with", numBatteries, "remaining");
    const candidateBatteries = batteries.slice(0, batteries.length-numBatteries+1);
    //console.log(candidateBatteries);
    const digit = Math.max(...candidateBatteries);
    const pos = candidateBatteries.indexOf(digit);
    //console.log("  found", digit, pos);
    return [digit, pos];
}

function maxBatteryInBank(bank: string, numBatteries: number) {
    var batteries = bank.trim().split('').map(x => parseInt(x));
    var acc = 0;
    for (var num = numBatteries; num > 0; num--) {
        const [digit, pos] = nextDigit(batteries, num);
        var batteries = batteries.slice(pos+1);
        const contribution = (digit * (Math.pow(10, num-1)));
        //console.log("   contribution", contribution);
        acc += contribution;
    }
    return acc;
}

export function part1(input: string) {
    const banks = input.split('\n');
    return banks
        .map(b => maxBatteryInBank(b,2))
        .reduce((partialSum, a) => partialSum + a, 0);
}

export function part2(input: string) {
    const banks = input.split('\n');
    return banks
        .map(b => maxBatteryInBank(b,12))
        .reduce((partialSum, a) => partialSum + a, 0);
}