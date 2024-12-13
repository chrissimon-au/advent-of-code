cmake_minimum_required(VERSION 3.30.5)

project(AoC-Day14)

add_executable(AoC-Day14 day14.cxx)

target_include_directories(AoC-Day14 PUBLIC
                            ${PROJECT_BINARY_DIR})
