package au.chrissimon

typealias Operation = (Int, Int) -> Int

val operations: List<Operation> = listOf(
    { a, b -> a + b },
    { a, b -> a * b }
)

fun totalValidCalibrationResult(equations: String): Int {
    val equationParts = equations.split(":")
    val total = equationParts[0].toInt()
    val operands = equationParts[1].trim().split(" ").map {it.toInt()}
    val calculatedTotals = operations.map { operands.reduce(it) }
    return if (calculatedTotals.any { it == total }) { total } else { 0 }
}
