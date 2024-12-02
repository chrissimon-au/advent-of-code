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

function adjacentDifferences(report) {
    if (report.length < 2) {
        return [];
    }
    return report.splice(1).map((value, index) => value - report[index]);
}

function isReportSafe(report) {
    const diffs = adjacentDifferences(report);
    return diffs.filter(diff => diff > 3 || diff < 1).length == 0;
}

function numSafeReports(reports) {
    return reports.filter(isReportSafe).length;
}

module.exports = {
    parse,
    numSafeReports
} 