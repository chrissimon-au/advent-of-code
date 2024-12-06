package au.chrissimon;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class Day06Test {

    static final String multiLineMap1 = """
        .#..
        .>.#
        ....
        ..#.
        """;

    private static Stream<Arguments> testMapsForExiting() {
        return Stream.of(
                Arguments.of("^", 1),
                Arguments.of(">.", 2),
                Arguments.of(">.#", 2),
                Arguments.of(multiLineMap1, 5)
        );
    }

    @ParameterizedTest()
    @MethodSource("testMapsForExiting")
    public void test_GuardRoute(String map, int expectedVisitedLocationCount) {
        assertEquals(expectedVisitedLocationCount, Day06.getVisitedLocationCount(map));
    }

    @ParameterizedTest
    @CsvSource({"sampledata.txt,sampledata.answer.txt", "testdata.txt,testdata.answer.txt"})
    public void test_TestGuardRoute(String dataFile, String answerFile) throws IOException {
        String map = Files.readString(Path.of("src/test/java/au/chrissimon/" + dataFile), Charset.defaultCharset());
        int expectedVisitedLocationCount =  Integer.parseInt(Files.readString(Path.of("src/test/java/au/chrissimon/" + answerFile), Charset.defaultCharset()));
        assertEquals(expectedVisitedLocationCount, Day06.getVisitedLocationCount(map));
    }

    private static Stream<Arguments> testMapsForLooping() {
        return Stream.of(
                Arguments.of("""
                                        ###
                                        .^#
                                        ###
                                        """, 1),
                Arguments.of("""
                                        ....
                                        ...#
                                        ...#
                                        #^..
                                        ..#.
                                        """, 2)
        );
    }

    @ParameterizedTest()
    @MethodSource("testMapsForLooping")
    public void test_GuardRouteCreateLoopInterventionPoints(String map, int expectedVisitedLocationCount) {
        assertEquals(expectedVisitedLocationCount, Day06.getNumberOfLoopInterventionPoints(map));
    }

    @ParameterizedTest
    @CsvSource({"sampledata.txt,sampledata.answer2.txt", "testdata.txt,testdata.answer2.txt"})
    public void test_TestGuardRouteCreateLoopInterventionPoints(String dataFile, String answerFile) throws IOException {
        String map = Files.readString(Path.of("src/test/java/au/chrissimon/" + dataFile), Charset.defaultCharset());
        int expectedVisitedLocationCount =  Integer.parseInt(Files.readString(Path.of("src/test/java/au/chrissimon/" + answerFile), Charset.defaultCharset()));
        assertEquals(expectedVisitedLocationCount, Day06.getNumberOfLoopInterventionPoints(map));
    }
}