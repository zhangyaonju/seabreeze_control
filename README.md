# FluoSpec systems control using a Raspberry Pi III.

The FluoSpec system uses two Ocean Optics spectrometers (HR-2000+ and QE-Pro) to monitor the canopy reflectance and the solar-induced chlorophyll fluorescence (SIF).

This code is based on the Seabreeze library written in C language.

## Install Seabreeze Library
To start, one must install the seabreeze library on a Raspberry Pi III (RP3) which can be downloaded from http://oceanoptics.com/product/seabreeze/. (Version 3.0.11). One can follow the instructions on http://oceanoptics.com/api/seabreeze/.
To install the Seabreeze, you need to first unzip the downloaded zip file. Change directory to Seabreeze where a readme file locates, and then use the command
<pre><code>make new</code></pre>
and then use the code to add the library to the system
<pre><code>export LD_LIBRARY_PATH="$PWD/lib"</code></pre>
if your raspberry Pi 3 runs a raspbian OS, you will probably face the problem of missing "usb.h", you can use the command 
<pre><code>sudo apt-get install libusb-dev</code></pre>
The make process may take several minutes, please be patient.
To test whether the installation is successful, one may use the test functions in "SeaBreeze/test/apt-test"

## Make the control function
find the make fuction under the test directory. change the api-test.c to our filename "RP3_fluorescence_v1.00.c" and then run the make function. you can then test it using the command "./RP3_fluorescence_v1.00"

## use crontab to setup the rountine
one can set up a crontab command to run this fuction regualarly (recommand every 15 minutes from 6:00 am to 18:00 pm local time). and use another command to sycronize the data to an external USB drive during the midnight when the observation fuction is paused.

## Configuration and test
RP3 have a HDMI port and can be connected to a monitor for indoor configuration. When setting up for outdoor use, a vnc viewer may allow you to use other laptops to remotely control the RP3. The latest Raspbian OS have intergrated this function, but you may need static IP address so that you can control it through the Ethernet.

## remote access through a 4g celluar module
The data obtained by this system can be uploaded to a server with static IP address. However, the 4g cellular module usually cannot provide a static IP that is required for remote access. A teamviewer software can be installed on RP3 and used for remote control. https://www.teamviewer.com/en/download/linux/#downloadAdditionalDownloads

## Notes
Since the Raspberry does not have a internal time clock. When power off, time will be reset and affect the crontab schedule. A real clock module (RTM) is needed, e.g. (https://www.amazon.com/battery-Raspberry-Arduino-Atomic-Market/dp/B01M105UFC/ref=sr_1_1?ie=UTF8&qid=1494884994&sr=8-1&keywords=real+clock+module), other RTM may also be used, but may need different configuration.
