# FluoSpec systems control using a Raspberry Pi III.

The FluoSpec system uses two Ocean Optics spectrometers (HR-2000 and QE-Pro) to monitor the canopy reflectance and the solar-induced chlorophyll fluorescence (SIF).

This code is based on the Seabreeze library written in C language.

## Install Seabreeze Library
To start, one must install the seabreeze library on a Raspberry Pi III (RP3) which can be downloaded from http://oceanoptics.com/product/seabreeze/. (Version 3.0.11). One can follow the instructions on http://oceanoptics.com/api/seabreeze/.

To test whether the installation is successful, one may use the test functions in “SeaBreeze/test/apt-test.c”

## Make the control function
find the make fuction under the test directory. change the api-test.c to our filename

