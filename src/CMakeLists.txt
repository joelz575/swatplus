cmake_minimum_required (VERSION 3.12)
project(hello CXX Fortran)
enable_testing()

set(CTEST_OUTPUT_ON_FAILURE ON)

add_compile_options(-Wall -Wextra -Wpedantic)

FILE(GLOB csrc *.c)
FILE(GLOB fsrc *.f90)

add_executable(swatplus ${csrc} ${fsrc} )
