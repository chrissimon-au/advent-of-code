#include <iostream>
#include <catch2/catch_test_macros.hpp>

bool check() {
    return true;
}

TEST_CASE( "Checks", "[check]" ) {
    REQUIRE( check() == true );
}