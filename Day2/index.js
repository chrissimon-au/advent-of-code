function parse(input) {    
    return [input.split(/\s/).map(a => parseInt(a))];
}

module.exports = {
    parse
} 