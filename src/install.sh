#$ cat install

#json-c BUILD instructions...

#if [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "darwin"* || "$OSTYPE" == "cygwin" ]]; then
    #sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
    #sudo apt-get update -y
   ##sudo apt-get install g++-7 -y
   ##sudo apt install git
   ##sudo apt install cmake
   ##git clone https://github.com/json-c/json-c.git
   ##mkdir json-c-build
   ##cd json-c-build
   ##cmake ../json-c
   ##make
   ##make test
   ##make USE_VALGRIND=0 test
   ##make install # always fails right here
   #./bootstrap-vcpkg.sh
   #./vcpkg integrate install
   ### sudo apt install libjson-c-dev
   # sudo apt update
   # sudo apt-get install libjson-glib-1.0-0 libjson-glib-1.0-0-dev
#else
#git clone https://github.com/Microsoft/vcpkg.git
#git clone https://github.com/json-c/json-c.git
cd vcpkg
bootstrap-vcpkg.bat -disableMetrics
vcpkg integrate install
vcpkg install json-c
#fi
#echo "vcpkg installed successfully"

#echo "json-c installed successfully"
#für APPVEYOR
#need .yml file
cd ..
make
#echo ".exe made successfully"




