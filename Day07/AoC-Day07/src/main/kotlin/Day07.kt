package au.chrissimon

import kotlin.math.pow

typealias Operation = (Long, Long) -> Long

val baseOperations: List<Operation> = listOf(
    { a, b -> a + b },
    { a, b -> a * b },
)

val allOperations: List<Operation> = baseOperations + listOf { a, b -> (a.toString() + b.toString()).toLong() }


private fun asBase(input: Int, base: Int): Long {
    var num: Int = input
    var ret: Long = 0
    var factor: Long = 1
    while (num > 0) {
        ret += num % base * factor
        num /= base
        factor *= 10
    }
    return ret
}

private fun getDigit(numString: String, digit: Int): Int =
    if (numString.length <= digit) {
        0
    } else {
        numString[numString.length - digit - 1].toString().toInt()
    }


infix fun Int.pow(exponent: Int): Int = toDouble().pow(exponent).toInt()

private fun getOperationSets(numOps: Int, operations: List<Operation>): Sequence<List<Operation>> =
    (0..<(operations.size pow numOps)).asSequence().map { i ->
        val digits = asBase(i, operations.size).toString()
        (0..<numOps).map {
            operations[getDigit(digits, it)]
        }
    }

fun totalValidCalibrationForEquation(equation: String, operations: List<Operation>): Long {
    val equationParts = equation.split(":")
    val total = equationParts[0].toLong()
    val operands = equationParts[1].trim().split(" ").map {it.toLong()}
    val operationSets = getOperationSets(operands.size - 1, operations)
    val calculatedTotals = operationSets.map { opSet ->
        operands.reduceIndexed { index, l, r ->
            opSet[index-1](l, r)
        }
    }
    return calculatedTotals.firstOrNull { it == total } ?: 0
}

fun totalValidCalibrationResult(equations: String, operations: List<Operation>): Long =
    equations.split(System.lineSeparator()).sumOf { totalValidCalibrationForEquation(it, operations) }
