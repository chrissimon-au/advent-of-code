package au.chrissimon

fun totalValidCalibrationResult(equations: String): Int {
    val equationParts = equations.split(":")
    val total = equationParts[0].toInt()
    val operands = equationParts[1].trim().split(" ").map {it.toInt()}
    val calculatedTotal = operands.sumOf {it}
    return if (total == calculatedTotal) { total } else { 0 }
}
