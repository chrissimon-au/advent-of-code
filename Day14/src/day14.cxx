#include <catch2/catch_test_macros.hpp>
#include <string>

int get_safety_score(std::string input) {
    return 0;
}

TEST_CASE( "Single Robot, Single Dimension" ) {
    std::string input = "p=0,4 v=1,0";    
    REQUIRE( get_safety_score(input) == 0 );
}