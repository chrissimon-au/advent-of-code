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

    let numDecreasing = 0;
    const diffs = report.slice(1).map((value, idx) => {
        const diff = value - report[idx];
        if (diff < 0) {
            numDecreasing++;
        }
        return diff;
    });
    if (numDecreasing >= report.length / 2) {
        return diffs.map (diff => diff * -1);
    }
    return diffs;
}

const isDiffUnSafe = diff => diff > 3 || diff < 1;

function isReportSafe(report) {
    const diffs = adjacentDifferences(report);

    return diffs.filter(isDiffUnSafe).length == 0;
}

function isReportSafeWithProblemDampener(report) {
    const diffs = adjacentDifferences(report);
    const firstProblemIdx = diffs.findIndex(isDiffUnSafe);

    return isReportSafe(report.toSpliced(firstProblemIdx, 1))
        || isReportSafe(report.toSpliced(firstProblemIdx+1, 1));
}

function numSafeReports(reports) {
    return reports.filter(isReportSafe).length;
}

function numSafeWithProblemDampenerReports(reports) {
    return reports.filter(isReportSafeWithProblemDampener).length;
}

module.exports = {
    parse,
    numSafeReports,
    numSafeWithProblemDampenerReports
} 