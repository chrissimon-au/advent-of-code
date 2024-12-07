import au.chrissimon.Day07
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource


class Day07Test {

    @ParameterizedTest
    @CsvSource("1: 1, 1", "2: 2, 2")
    fun testCheck(equations: String, expectedResult: Int) {
        val day07 = Day07(equations)
        assertEquals(expectedResult, day07.totalValidCalibrationResult())
    }
}