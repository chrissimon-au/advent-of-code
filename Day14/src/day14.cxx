#include <catch2/catch_test_macros.hpp>
#include <bits/stdc++.h>
#include <string>

class Coordinates {
private:
    int x_;
    int y_;
public:
    Coordinates(int x, int y) : x_(x), y_(y) {}
    const int x() const { return x_; }
    const int y() const { return y_; }
};

struct Position : Coordinates {
    Position(int x, int y) : Coordinates(x,y) {}
};

std::ostream & operator << (std::ostream & outs, const Coordinates & coords) {
    return outs << "(" << coords.x() << ", " << coords.y() << ")";
}

int get_safety_score(std::string input, Position size) {
    return 0;
}


TEST_CASE( "Single Robot, Single Dimension" ) {
    SECTION( "From Input" ) {
        std::string input = "p=0,4 v=1,0";
        Position size = Position(7,5);
        std::cout << size << std::endl;
        REQUIRE( get_safety_score(input, size) == 0 );
    }
}