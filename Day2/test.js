const { parse, numSafeReports } = require('./index');

describe('parse', () => {

    test.each([
            [
                '1',
                [[1]]
            ],
            [
                '1 2',
                [[1, 2]]
            ],
            [
                `1 2
3 4`,
                [[1, 2], [3, 4]]
            ],
        ])('Parses %s to %o', (input, output) => {
        let inputData = parse(input);
    
        expect(inputData).toEqual(output);
    });

});

describe('Report Safety', () => {

    test.each([
        [
            [[1]],
            1            
        ],
        [
            [[1],[1]],
            2
        ]
    ])('Safety of %o is %i', (reports, expectedSafetyCount) => {
        expect(numSafeReports(reports)).toEqual(expectedSafetyCount);
    });

});