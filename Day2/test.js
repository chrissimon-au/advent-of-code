const { parse, numSafeReports } = require('./index');
const fs = require('fs');

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
        ],
        [
            [[1,2],[1,5]],
            1
        ],
        [
            [[1,1],[1,4]],
            1
        ],
        [
            [[2,1]],
            1
        ],
        [
            [[2,1],[1,2],[5,1]],
            2
        ],
        [
            [ [ 1, 2, 7] ],
            0
        ],
        [
            parse(fs.readFileSync('./sampledata.txt', {encoding: 'utf-8'})),
            parseInt(fs.readFileSync('./sampledata.answer.txt', {encoding: 'utf-8'}))
        ]
    ])('Safety of %o is %i', (reports, expectedSafetyCount) => {
        expect(numSafeReports(reports)).toEqual(expectedSafetyCount);
    });

});