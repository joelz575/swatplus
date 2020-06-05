cmake_minimum_required (VERSION 3.14.5)

enable_language(Fortran)
project(SwatPlus)

include(FetchContent)
FetchContent_Declare(json-c
  GIT_REPOSITORY    https://github.com/json-c/json-c.git
)

FetchContent_MakeAvailable(json-c)


FILE(GLOB src *.f90 *.c)
add_executable(swatplus_exe ${src} )

if(WIN32)
	target_link_libraries(swatplus_exe json-c  ws2_32)
else()
	target_link_libraries(swatplus_exe json-c)
endif()

target_include_directories(swatplus_exe PUBLIC ${json-c_SOURCE_DIR})

install(TARGETS swatplus_exe DESTINATION bin)
