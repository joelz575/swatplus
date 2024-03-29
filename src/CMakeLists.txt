cmake_minimum_required (VERSION 3.14.5)

enable_language(Fortran)
project(SwatPlus)

SET(FORTRAN_EXTRA_COMPILE_FLAGS "-g -O0 -ffixed-line-length-132 -fdefault-real-8 -fdefault-double-8 -fno-automatic -fbacktrace -ffpe-trap=invalid,zero,overflow,denormal -static-libgfortran -fbounds-check -fopenmp -ffast-math -funsafe-math-optimizations")

SET(CMAKE_Fortran_FLAGS  "${CMAKE_Fortran_FLAGS} ${FORTRAN_EXTRA_COMPILE_FLAGS}")

include(FetchContent)
FetchContent_Declare(json-c
  GIT_REPOSITORY    https://github.com/joelz575/json-c.git
)

FetchContent_MakeAvailable(json-c)


FILE(GLOB src *.f90 *.c)
add_executable(swatplus_exe ${src} )

if(WIN32)
	target_link_libraries(swatplus_exe ws2_32 json-c)
else()
	target_link_libraries(swatplus_exe json-c)
endif()

target_include_directories(swatplus_exe PUBLIC ${json-c_SOURCE_DIR})

set_target_properties(swatplus_exe PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

install(TARGETS swatplus_exe DESTINATION bin)