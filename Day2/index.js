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

}

module.exports = {
    parse,
    numSafeReports
} 