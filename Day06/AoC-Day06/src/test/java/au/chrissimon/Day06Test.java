package au.chrissimon;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class Day06Test {

    @Test
    public void test_check() {
        Day06 day06 = new Day06();
        assertTrue(day06.check());
    }

}