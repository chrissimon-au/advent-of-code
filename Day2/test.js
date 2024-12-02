const { parse } = require('./index');

test('Parses text to list of lists', () => {
    let input = `1`;
    let inputData = parse(input);

    expect(inputData).toEqual([[1]]);
});