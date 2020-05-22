#$ cat install

#json-c BUILD instructions...

if [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "darwin"* || "$OSTYPE" == "darwin"* || "$OSTYPE" == "cygwin" ]]; then
    sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
    sudo apt-get update -y
    sudo apt-get install g++-7 -y
    sudo apt install git
    sudo apt install cmake
    $ git clone https://github.com/json-c/json-c.git
    $ mkdir json-c-build
    $ cd json-c-build
    $ cmake ../json-c
    $ make
    $ make test
    $ make install
   #./bootstrap-vcpkg.sh
   #./vcpkg integrate install
else
   git clone https://github.com/Microsoft/vcpkg.git
   cd vcpkg
   git clone https://github.com/json-c/json-c.git
   ./bootstrap-vcpkg.sh
   ./vcpkg integrate install
   vcpkg install json-c
fi
#echo "vcpkg installed successfully"

#echo "json-c installed successfully"
#für APPVEYOR
#need .yml file
make
#echo ".exe made successfully"




