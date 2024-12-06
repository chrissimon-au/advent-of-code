package au.chrissimon;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.*;

class Day06Test {

    @ParameterizedTest()
    @CsvSource({"^,1", ">.,2", ">.#,2"})
    public void test_GuardRoute(String map, int expectedVisitedLocationCount) {
        assertEquals(expectedVisitedLocationCount, Day06.getVisitedLocationCount(map));
    }

}