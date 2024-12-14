#include <catch2/catch_test_macros.hpp>
#include <bits/stdc++.h>
#include <string>

struct Coordinates {
    int x;
    int y;
    Coordinates(int x, int y) : x(x), y(y) {}
};
std::ostream & operator << (std::ostream & outs, const Coordinates & coords) {
    return outs << "(" << coords.x << ", " << coords.y << ")";
}

int get_safety_score(std::string input, Coordinates size) {
    return 0;
}


TEST_CASE( "Single Robot, Single Dimension" ) {
    std::string input = "p=0,4 v=1,0";
    Coordinates size = Coordinates(7,5);
    std::cout << size << std::endl;
    REQUIRE( get_safety_score(input, size) == 0 );
}