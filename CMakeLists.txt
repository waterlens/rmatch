cmake_minimum_required(VERSION 3.17)

project(rmatch)
set(CMAKE_CXX_STANDARD 20)

add_executable(rmatch rmatch.cpp rmatch.hpp)

add_subdirectory(fmt)
target_link_libraries(rmatch PRIVATE fmt::fmt-header-only)
include_directories(fmt/include)
include_directories(xbyak/xbyak)
