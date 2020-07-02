mkdir build
cd build
if [[ $TRAVIS_OS_NAME != windows ]]; then cmake ../src;
else cmake  -G "MinGW Makefiles" -A x64 DCMAKE_SH="CMAKE_SH-NOTFOUND" ../src; fi
cmake --build .
#-DCMAKE_GENERATOR_PLATFORM=x64