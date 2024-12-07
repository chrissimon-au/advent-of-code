package au.chrissimon

fun totalValidCalibrationResult(equations: String): Int {
    val equationParts = equations.split(":")
    val total = equationParts[0].toInt()
    val equation = equationParts[1].trim().toInt();
    return if (total == equation) { total } else { 0 }
}
