#include <catch2/catch_test_macros.hpp>
#include <bits/stdc++.h>
#include <string>

class Coordinates {
protected:
    int x_;
    int y_;
public:
    Coordinates(int x, int y) : x_(x), y_(y) {}
    const int x() const { return x_; }
    const int y() const { return y_; }
};

std::ostream & operator << (std::ostream & outs, const Coordinates & coords) {
    return outs << "(" << coords.x() << ", " << coords.y() << ")";
}

class Velocity : public Coordinates {
public:
    Velocity(int x, int y) : Coordinates(x,y) {}
    Velocity operator*(int multiplier) {
        return Velocity(x_ * multiplier, y_ * multiplier);
    }
};

class Position : public Coordinates {
public:
    Position(int x, int y) : Coordinates(x,y) {}
    void move_by(Velocity velocity, Position wrap_at) {
        x_ = (x_ + velocity.x()) % wrap_at.x();
        y_ = (y_ + velocity.y()) % wrap_at.y();
    }
};

class Robot {
private:
    Position position_;
    Velocity velocity_;
public:
    Robot(Position position, Velocity velocity) : position_(position), velocity_(velocity) {}
    const Position position() const { return position_; }

    void move_seconds(int seconds, Position map_size) { 
        position_.move_by(velocity_ * seconds, map_size);
    }
};

int get_safety_score(std::string input, Position size) {
    return 0;
}

TEST_CASE( "Single Robot, Single Dimension" ) {
    SECTION( "From Input" ) {
        std::string input = "p=0,4 v=1,0";
        Position map_size = Position(7,5);
        std::cout << map_size << std::endl;
        REQUIRE( get_safety_score(input, map_size) == 0 );
    }

    SECTION( "Direct Robot" ) {

        SECTION( "moving a single time unit" ) {
            Position map_size = Position(7,5);
            Position position = Position(0,4);
            Velocity velocity = Velocity(1,0);
            Robot r = Robot(position, velocity);

            r.move_seconds(1, map_size);

            REQUIRE( r.position().x() == 1 );
            REQUIRE( r.position().y() == 4 );
        }

        SECTION( "moving multiple time units" ) {
            Position map_size = Position(7,5);
            Position position = Position(0,4);
            Velocity velocity = Velocity(1,0);
            Robot r = Robot(position, velocity);

            r.move_seconds(4, map_size);

            REQUIRE( r.position().x() == 4 );
            REQUIRE( r.position().y() == 4 );
        }

        SECTION( "moving multiple time units and wrapping" ) {
            Position map_size = Position(7,5);
            Position position = Position(0,4);
            Velocity velocity = Velocity(1,0);
            Robot r = Robot(position, velocity);

            r.move_seconds(8, map_size);

            REQUIRE( r.position().x() == 1 );
            REQUIRE( r.position().y() == 4 );
        }

        SECTION( "moving in both directions and wrapping" ) {
            Position map_size = Position(7,5);
            Position position = Position(0,4);
            Velocity velocity = Velocity(1,3);
            Robot r = Robot(position, velocity);

            r.move_seconds(8, map_size);

            REQUIRE( r.position().x() == 1 );
            REQUIRE( r.position().y() == 3 );
        }

    }
}