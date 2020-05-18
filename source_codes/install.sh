$ cat install
git clone https://github.com/json-c/json-c.git
#json-c BUILD instructions...
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
if [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "darwin"* || "$OSTYPE" == "darwin"* || "$OSTYPE" == "cygwin" ]]; then
   ./bootstrap-vcpkg.sh
   ./vcpkg integrate install
else

   .\bootstrap-vcpkg.sh
   .\vcpkg integrate install
fi
echo "vcpkg installed successfully"
vcpkg install json-c
echo "json-c installed successfully"
#für APPVEYOR
#need .yml file
make
echo ".exe made successfully"




