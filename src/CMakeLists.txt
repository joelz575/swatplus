cmake_minimum_required (VERSION 3.12)

project(SwatPlus)

FILE(GLOB src *.f90 *.c)
add_executable(swatplus_exe ${src} )

include(ExternalProject)
set(EXTERNAL_INSTALL_LOCATION ${CMAKE_BINARY_DIR}/external)
ExternalProject_Add(json-c
  GIT_REPOSITORY    https://github.com/json-c/json-c.git
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${EXTERNAL_INSTALL_LOCATION}
)
include_directories(swatplus_exe ${EXTERNAL_INSTALL_LOCATION}/include/json-c)
link_directories(swatplus_exe ${EXTERNAL_INSTALL_LOCATION}/lib)


