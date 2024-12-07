import au.chrissimon.totalValidCalibrationResult
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;


class Day07Test {
    companion object {
        @JvmStatic
        fun testEquations(): List<Arguments> {
            return listOf(
                Arguments.of("1: 1", 1),
                Arguments.of("2: 2", 2),
                Arguments.of("3: 1", 0),
                Arguments.of("2: 1 1", 2),
                Arguments.of("6: 3 2", 6),
                Arguments.of("17: 5 3 2", 17)
            )
        }
    }

    @ParameterizedTest()
    @MethodSource("testEquations")
    fun testCheck(equations: String, expectedResult: Int) {
        assertEquals(expectedResult, totalValidCalibrationResult(equations))
    }
}