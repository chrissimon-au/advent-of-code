package au.chrissimon

import kotlin.math.pow

typealias Operation = (Int, Int) -> Int

val operations: List<Operation> = listOf(
    { a, b -> a + b },
    { a, b -> a * b }
)

infix fun Int.pow(exponent: Int): Int = toDouble().pow(exponent).toInt()

private fun getOperationSets(numOps: Int): List<List<Operation>> =
    (1..(2 pow numOps)).map { i ->
        (0..numOps).map {
            operations[((i and (2 pow it)) > 0).compareTo(false)]
        }
    }

fun totalValidCalibrationForEquation(equation: String): Int {
    val equationParts = equation.split(":")
    val total = equationParts[0].toInt()
    val operands = equationParts[1].trim().split(" ").map {it.toInt()}
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

fun totalValidCalibrationResult(equations: String): Int =
    equations.split(System.lineSeparator()).sumOf { totalValidCalibrationForEquation(it) }
