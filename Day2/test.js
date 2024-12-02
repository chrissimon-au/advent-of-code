const { parse } = require('./index');

describe('parse', () => {

    test.each([
            [
                '1',
                [[1]]
            ],
            [
                '1 2',
                [[1, 2]]
            ]
        ])('Parses %s to %o', (input, output) => {
        let inputData = parse(input);
    
        expect(inputData).toEqual(output);
    });

});
