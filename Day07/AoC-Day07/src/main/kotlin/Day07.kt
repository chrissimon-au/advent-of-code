package au.chrissimon

import kotlin.math.pow

typealias Operation = (Long, Long) -> Long

val operations: List<Operation> = listOf(
    { a, b -> a + b },
    { a, b -> a * b }
)

infix fun Int.pow(exponent: Int): Int = toDouble().pow(exponent).toInt()

private fun getOperationSets(numOps: Int): List<List<Operation>> =
    (0..<(2 pow numOps)).map { i ->
        (0..numOps).map {
            operations[((i and (2 pow it)) > 0).compareTo(false)]
        }
    }

fun totalValidCalibrationForEquation(equation: String): Long {
    val equationParts = equation.split(":")
    val total = equationParts[0].toLong()
    val operands = equationParts[1].trim().split(" ").map {it.toLong()}
    val operationSets = getOperationSets(operands.size - 1)
    val calculatedTotals = if (operationSets.isNotEmpty()) {
        operationSets.map { opSet ->
            operands.reduceIndexed { index, l, r ->
                opSet[index-1](l, r)
            }
        }
    } else {
        listOf(operands[0])
    }
    return if (calculatedTotals.any { it == total }) {
        total
    } else {
        0
    }
}

fun totalValidCalibrationResult(equations: String): Long =
    equations.split(System.lineSeparator()).sumOf { totalValidCalibrationForEquation(it) }
