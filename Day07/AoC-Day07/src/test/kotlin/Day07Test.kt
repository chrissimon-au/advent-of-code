import au.chrissimon.totalValidCalibrationResult
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.CsvSource


class Day07Test {

    @ParameterizedTest
    @CsvSource("1: 1, 1", "2: 2, 2", "3: 1, 0", "2: 1 1, 2")
    fun testCheck(equations: String, expectedResult: Int) {
        assertEquals(expectedResult, totalValidCalibrationResult(equations))
    }
}