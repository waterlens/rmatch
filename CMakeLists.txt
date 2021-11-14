cmake_minimum_required(VERSION 3.17)

project(rmatch)
set(CMAKE_CXX_STANDARD 20)

add_executable(rmatch rmatch.cpp rmatch.hpp)

option(ZYDIS_BUILD_TOOLS "" OFF)
option(ZYDIS_BUILD_EXAMPLES "" OFF)
add_subdirectory(zydis)
add_subdirectory(fmt)

target_link_libraries(rmatch PRIVATE fmt::fmt-header-only Zydis)
include_directories(fmt/include)
include_directories(xbyak/xbyak)
include_directories(zydis/include)
