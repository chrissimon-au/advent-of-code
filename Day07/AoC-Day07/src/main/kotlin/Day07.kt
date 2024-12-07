package au.chrissimon

class Day07(val equations: String) {

    fun totalValidCalibrationResult(): Int {
        val equationParts = equations.split(":")
        val total = equationParts[0].toInt()
        val equation = equationParts[1].trim().toInt();
        return if (total == equation) { total } else { 0 }
        }
}