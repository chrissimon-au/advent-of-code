package au.chrissimon;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.*;

class Day06Test {

    @ParameterizedTest()
    @CsvSource({"^,1"})
    public void test_GuardRoute(String map, int expectedVisitedLocationCount) {
        Day06 day06 = new Day06();
        assertEquals(expectedVisitedLocationCount, day06.getVisitedLocationCount(map));
    }

}