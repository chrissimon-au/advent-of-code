import au.chrissimon.Day07
import org.junit.jupiter.api.Assertions.*
import kotlin.test.Test

class Day07Test {
    @Test
    fun testCheck() {
        val day07 = Day07("1: 1")
        assertEquals(1, day07.totalValidCalibrationResult())
    }
}