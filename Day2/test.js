const { parse } = require('./index');

describe('parse', () => {

    test.each([
            [
                '1',
                [[1]]
            ]
        ])('Parses text to list of lists', (input, output) => {
        let inputData = parse(input);
    
        expect(inputData).toEqual(output);
    });

});
