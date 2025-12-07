function maxBatteryInBank(bank: string) {
    const batteries = bank.trim().split('').map(x => parseInt(x));
    const firstDigit = Math.max(...batteries);
    const firstDigitPos = batteries.indexOf(firstDigit);
    if (firstDigitPos == batteries.length - 1) {
        const secondDigit = firstDigit;
        const newFirstDigit = Math.max(...(batteries.slice(0, firstDigitPos)));
        return newFirstDigit * 10 + secondDigit;
    } else {
        const secondDigit = Math.max(...(batteries.slice(firstDigitPos + 1)));
        return firstDigit * 10 + secondDigit;
    }
}

export function part1(input: string) {
    const banks = input.split('\n');
    return banks
        .map(maxBatteryInBank)
        .reduce((partialSum, a) => partialSum + a, 0);
}

export function part2(input: string) {
    return "";
}