package au.chrissimon

import kotlin.math.pow

typealias Operation = (Int, Int) -> Int

val operations: List<Operation> = listOf(
    { a, b -> a + b },
    { a, b -> a * b }
)

private fun getOperationSets(numOps: Int): List<List<Operation>> {
    val numCombos = 2.0.pow(numOps).toInt()-1
    return (1..numCombos+1).map { i ->
        (0..numOps).map { j ->
            val bitPos = 2.0.pow(j).toInt()
            if ((i and bitPos) > 0) {
                operations[1]
            } else {
                operations[0]
            }
        }
    }
}

fun totalValidCalibrationResult(equations: String): Int {
    val equationParts = equations.split(":")
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
    return if (calculatedTotals.any { it == total }) { total } else { 0 }
}
