# FluoSpec systems control using a Raspberry Pi III.

The FluoSpec system uses two Ocean Optics spectrometers (HR-2000+ and QE-Pro) to monitor the canopy reflectance and the solar-induced chlorophyll fluorescence (SIF).

This code is based on the Seabreeze library written in C language.

## Install Seabreeze Library
To start, one must install the seabreeze library on a Raspberry Pi III (RP3) which can be downloaded from http://oceanoptics.com/product/seabreeze/. (Version 3.0.11). One can follow the instructions on http://oceanoptics.com/api/seabreeze/.

if your raspberry Pi 3 runs a raspbian OS, you will probably face the problem of missing "usb.h", you can use the command <pre><code>
sudo apt-get install libusb-dev</code></pre>
The make process may take several minutes, please be patient.
To test whether the installation is successful, one may use the test functions in "SeaBreeze/test/apt-test"

## Make the control function
find the make fuction under the test directory. change the api-test.c to our filename "RP3_fluorescence_v1.00.c" and then run the make function. you can then test it using the command "./RP3_fluorescence_v1.00"

## use crontab to setup the runtine
one can set up a crontab command to run this fuction regualarly (recommand every 15 minutes from 6:00 am to 18:00 pm local time). and use another command to sycronize the data to an external USB drive during the midnight when the observation fuction is paused.

