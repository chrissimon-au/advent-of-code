package au.chrissimon;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class Day06Test {

    static final String multiLineMap1 = """
        .#..
        .>.#
        ....
        ..#.
        """;

    private static Stream<Arguments> testMaps() {
        return Stream.of(
                Arguments.of("^", 1),
                Arguments.of(">.", 2),
                Arguments.of(">.#", 2),
                Arguments.of(multiLineMap1, 5)
        );
    }

    @ParameterizedTest()
    @MethodSource("testMaps")
    public void test_GuardRoute(String map, int expectedVisitedLocationCount) {
        assertEquals(expectedVisitedLocationCount, Day06.getVisitedLocationCount(map));
    }

}