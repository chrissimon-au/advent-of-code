cmake_minimum_required(VERSION 3.30.5)

project(AoC-Day14)

find_package(Catch2 3 REQUIRED)

add_executable(day14 day14.cxx)

target_link_libraries(day14 PRIVATE Catch2::Catch2WithMain)

add_custom_command(
     TARGET day14
     COMMENT "Run tests"
     POST_BUILD 
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
     COMMAND day14
)