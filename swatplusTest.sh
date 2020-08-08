#if statement for operating systems
if [[ $TRAVIS_OS_NAME == windows ]]
then
python -m ensurepip
python -m pip install setuptools
python -m pip install --upgrade setuptools
python -m pip install git+https://github.com/joelz575/Tinamit
python test.py

elif [[ $TRAVIS_OS_NAME == osx ]]
then
python3 -m pip install --upgrade pip
python3 -m pip install --upgrade setuptools
python3 -m pip install git+https://github.com/joelz575/Tinamit
python3 test.py

else
python3.6 -m pip install --upgrade pip
python3.6 -m pip install --upgrade setuptools
python3.6 -m pip install git+https://github.com/joelz575/Tinamit
#from Tinamit import test.py
python3.6 test.py #has to be in swatplus not with Tinamit
#cd "/home/joelz/Documents/Prof Adamowski/Iximulew/SWAT+/TRIAL/Trial Robit/Trial Robit/Scenarios/Default/TxtInOut"
#sudo /home/joelz/PycharmProjects/swatplus/build/bin/swatplus_exe
fi