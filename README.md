# Dynamic SWAT+

A SWAT+ Model repository (almost) clone.

[![Build Status](https://travis-ci.com/joelz575/swatplus.svg?branch=master)](https://travis-ci.com/joelz575/swatplus)

This repository is (mostly) a clone of the official SWAT+ one available [here](https://bitbucket.org/blacklandgrasslandmodels/modular_swatplus/src/master/). A few major differences are:
* We only store source code in the git repository. This means that documentation, compiled executables, and example data are **not** included. It also means that you will not download a git repo over 1 GB in size when you clone it.
* Releases for MacOS, Windows and Linux are all automatically compiled and published in the releases tab (magic!)
* Added option for dynamic model linking (documentation to follow)

We will always try to keep this repostitory as up to date as possible compared to the main SWAT+ code. Pull requests are welcome!

## How to compile
If you feel like recompiling SWAT+ from source yourself (what could possibly go wrong?), you will need `CMake`. Check the
`.travis.yml` file for an example of how it works on Windows versus Linux/OSX.

## Dynamic linking
SWAT+ (as well as SWAT, and nearly all other hydrological models) do not support dynamic linking at runtime. 
In other words, if you wanted to connect a SWAT+ model with another model (e.g., an agriculture, landuse,
system dynamics, or groundwater model) *dynamically*, with output from one becoming the input for
the other, and vice versa, you had to stop the simulation after a year, rewrite input files, and restart it.
Unfortunately, SWAT+ has a lot of internal state variables (such as water balances) that are not saved
to output files, nor can be specified in input files...

That is why Dynamic SWAT+ was invented. Run normally, it behaves exactly like SWAT+ (we hope).
However, by specifying the `-d` option at runtime, you can now tell SWAT+ to pause after a specific
amount of time and exchange data with a different model according to the `Tinamït` data
exchange specification (disclaimer: in progress). When the model is given the signal
to continue, simulation will resume exactly where it left off.

For instance, when in the SWAT+ project directory, running

`$ SwatPlus -d 12345`

will execute SWAT+ in dynamic mode on port `12345`.

## License
The license provided here, naturally, only applies to our original work on DynamicSWAT+ (such as `src/CMakeLists.txt`, `src/tinamit_module.f90` and `src/socket.c`) and not to the rest of the original SWAT+ code. For information on SWAT+ code licensing, please contact the SWAT+ team directly.
