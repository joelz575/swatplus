WHERE GCC
WHERE FCC
mkdir build
cd build
cmake .
cmake   -G "MinGW Makefiles" -D CMAKE_C_COMPILER='C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/bin/gcc.exe' DCMAKE_FFLAGS=-m64 DCMAKE_SH="CMAKE_SH-NOTFOUND"  ../src
cmake --build .
#cmd /k this is a new comment to be able to push something