import au.chrissimon.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.CsvSource
import org.junit.jupiter.params.provider.MethodSource
import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path


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
                Arguments.of("17: 5 3 2", 17),
                Arguments.of("""
                    17: 5 3 2
                    15: 3 2 3
                    10: 1 2 3
                """.trimIndent(), 32),
                Arguments.of("156: 15 6", 156)
            )
        }
    }

    @ParameterizedTest()
    @MethodSource("testEquations")
    fun testCheck(equations: String, expectedResult: Long) {
        assertEquals(expectedResult, totalValidCalibrationResult(equations, allOperations))
    }

    @ParameterizedTest
    @CsvSource("sampledata.txt,sampledata.answer.txt","testdata.txt,testdata.answer.txt")
    @Throws(
        IOException::class
    )
    fun testAoCTest(dataFile: String, answerFile: String) {
        val equations = Files.readString(Path.of("src/test/kotlin/$dataFile"), Charset.defaultCharset())
        val expectedTotal =
            Files.readString(Path.of("src/test/kotlin/$answerFile"), Charset.defaultCharset()).toLong()
        assertEquals(expectedTotal, totalValidCalibrationResult(equations, baseOperations))
    }

    @ParameterizedTest
    @CsvSource("sampledata.txt,sampledata.answer2.txt","testdata.txt,testdata.answer2.txt")
    @Throws(
        IOException::class
    )
    fun testAoCTestPart2(dataFile: String, answerFile: String) {
        val equations = Files.readString(Path.of("src/test/kotlin/$dataFile"), Charset.defaultCharset())
        val expectedTotal =
            Files.readString(Path.of("src/test/kotlin/$answerFile"), Charset.defaultCharset()).toLong()
        assertEquals(expectedTotal, totalValidCalibrationResult(equations, allOperations))
    }
}