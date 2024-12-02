const { EOL } = require("os");

function parse(input) {    
    return input
        .split(EOL)
        .map(row => 
            row
            .split(/\s/)
            .map(a => parseInt(a)
        )
    );
}

function numSafeReports(reports) {
    return 1;
}

module.exports = {
    parse,
    numSafeReports
} 