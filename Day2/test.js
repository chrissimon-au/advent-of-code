const { parse, numSafeReports, numSafeWithProblemDampenerReports } = require('./index');
const fs = require('fs');

const getData = (filePrefix, answerSuffix="") =>
    [
        parse(fs.readFileSync(`./${filePrefix}.txt`, {encoding: 'utf-8'})),
        parseInt(fs.readFileSync(`./${filePrefix}.answer${answerSuffix}.txt`, {encoding: 'utf-8'}))
    ]

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
        getData('sampledata'),
        getData('testdata'),
    ])('Safety of %o is %i', (reports, expectedSafetyCount) => {
        expect(numSafeReports(reports)).toEqual(expectedSafetyCount);
    });

});

describe('Report Safety With Problem Dampener', () => {

    test.each([
        [
            [[1]],
            1            
        ],
        [
            [[1, 3, 2]],
            1            
        ],
        [
            [[1, 2, 7, 8, 9]],
            0
        ],
        [
            [[40, 42, 44, 47, 49, 50, 48]],
            1
        ],
        getData("sampledata", 2),
    ])('Safety of %o is %i', (reports, expectedSafetyCount) => {
        expect(numSafeWithProblemDampenerReports(reports)).toEqual(expectedSafetyCount);
    });

});