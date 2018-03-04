/*
This program is used for the data gathering for every 5 minutes for HR2000+ and QE-pro
It is designed for the Raspberry Pi 3 B version under C language
For every 5 minutes, the crontab call this program to complete the following procedure
	1. start the spectrometers
	2. wait for 2 minutes for the QE-pro to cool down to an optimum temperature.
		a. will output the enclosure temperature first
		b. start the cooling fuction and record the temepurature before it is stable
	3. get the optimum integration time for both HR2000+ and QE-pro
	4. start 10 pairs of observation (both for irradiance and reflectance)
	5. do the dark correction and nonlinearity correction
	6. get the time info and output the spectrum to the destination folder (/home/pi/spectrum_data/).
	
   20170515 updates: 
   1. change TEC temperature control, heat up to 20 and then cool down to -10 degree C
   2. change allowed DN range for optimal integration time to 100000 ~ 140000 for QE-pro and 9000~12000 for HR-2000+
   20171129 updates:
   1. add two additional measurement with fixed integration time (Only for QE-pro 2000 ms for irradiance and 1000 ms for radiance)
   	additional dark measurements for the irradiance and radiance measurements
	will be saved in a separate file
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <string.h>
#include "api/seabreezeapi/SeaBreezeAPI.h"

void HR2000PLUS_operation(long deviceID, int Operation_ct);
void QE_PRO_operation(long deviceID, int Operation_ct);
void QE_PRO_fixed_fime_operation(long deviceID, int Operation_ct);
double max_array(double *arr, int length) //get maximun value of the spectrum
{
	double maxVal = arr[0];
	int i;
	for (i = 0; i < length; i++){
		if (maxVal < arr[i])
			maxVal = arr[i];
	}
	return maxVal;
}

/*
double dark_correct_HR(double *spectrum)	//dark_correction for HR2000+ e-dark pixels are [0~17]
{
	double sum_dark = 0;
	double average_dark = 0;
	
	for (int i = 0; i < 18; i++)
	{
		sum_dark = sum_dark + spectrum[i];
	}
	average_dark = sum_dark/18;
	
	for (int i = 0; i < 2048; i++){
		spectrum[i] = spectrum[i] - average_dark;
	}
	return average_dark;
}
double dark_correct_QE(double *spectrum,double *spectrum_dark)	//dark_correction for QE-pro using dark measurement
{	
	for (int i = 0; i < 1044; i++){
		spectrum[i] = spectrum[i] - spectrum_dark[i];
	}
}
*/

//get integration time for HR2000+
long get_opt_integration_time_HR2k(long deviceID, long spectrometerID)//calibrate integration time every time before the measurement
{
	int spec_length;
	long minAllowedInteTime;
	long maxAllowedInteTime = 10000000;
	long optInteTime;

	int maxiter = 15;
	int iter = 0;
	int error;
	int flag;
	int max = 0;

	double max_spec;
	double max_allowed_DN = 12000;
	double min_allowed_DN = 9000;
	double *spectrum = 0;
	double ratio;
	
	spec_length = sbapi_spectrometer_get_formatted_spectrum_length(deviceID,
                spectrometerID, &error);
	minAllowedInteTime = sbapi_spectrometer_get_minimum_integration_time_micros(
                deviceID, spectrometerID, &error);
	printf("\nMinimum and Maximum integration time are [%ldms] [%ldms]\n", 
		minAllowedInteTime/1000, maxAllowedInteTime/1000);
	
	spectrum = (double *)calloc((size_t)spec_length,sizeof(double));
	optInteTime = minAllowedInteTime;
	sbapi_spectrometer_set_integration_time_micros(
                deviceID, spectrometerID, &error, optInteTime);
			
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometerID, 
		&error, spectrum, spec_length);
	sleep(1);
	max_spec = max_array(spectrum, spec_length);
	printf("\n\toptInteTime [%d], %ld ms, max value: %lf, [%d]",iter, optInteTime/1000, max_spec, flag);
	
	while(((max_spec > max_allowed_DN) || (max_spec < min_allowed_DN))&& (iter < maxiter)&&(max < 2)){
		iter++;
		if (max_spec > 350){
			ratio = (min_allowed_DN+1000)/(max_spec-350);
		}else{
			ratio = 100;
		}
		if (max_spec > max_allowed_DN){
			ratio = 0.65;
			max = 0;
		}
			
		optInteTime = (long)optInteTime * ratio;

		if (optInteTime > maxAllowedInteTime){
			optInteTime = maxAllowedInteTime;
			max++;
		}
		if (optInteTime < minAllowedInteTime){
			optInteTime = minAllowedInteTime;
		}
		sbapi_spectrometer_set_integration_time_micros(
                	deviceID, spectrometerID, &error, optInteTime);
		sleep(1);
		printf("\n\toptInteTime [%d], %ld ms",iter, optInteTime/1000);
		
		flag = sbapi_spectrometer_get_formatted_spectrum(deviceID,
                	spectrometerID, &error, spectrum, spec_length);
		sleep(1);		
		max_spec = max_array(spectrum, spec_length);
		printf("\n\toptInteTime [%d], %ld ms, max value: %lf, [%d]",iter, optInteTime/1000, max_spec, flag);
		
	}
	free(spectrum);
	return optInteTime;
}

//get the integration time for QE-pro
long get_opt_integration_time_QEpro(long deviceID, long spectrometerID)//calibrate integration time every time before the measurement
{
	int spec_length;
	long minAllowedInteTime;
	long maxAllowedInteTime = 30000000;
	long optInteTime;
	long *buffer_ids = 0;

	int maxiter = 5;
	int iter = 0;
	int error;
	int flag;
	int max = 0;
	int number_of_buffer;
	
	double max_spec;
	double max_allowed_DN = 140000;
	double min_allowed_DN = 100000;
	double *spectrum = 0;
	double ratio;
	
	number_of_buffer = sbapi_get_number_of_data_buffer_features(deviceID, &error);
	buffer_ids = (long *)calloc(number_of_buffer,sizeof(long));
	number_of_buffer = sbapi_get_data_buffer_features(deviceID, &error,
		buffer_ids, number_of_buffer);
	spec_length = sbapi_spectrometer_get_formatted_spectrum_length(deviceID,
                spectrometerID, &error);
	minAllowedInteTime = sbapi_spectrometer_get_minimum_integration_time_micros(
                deviceID, spectrometerID, &error);
	printf("\nMinimum and Maximum integration time are [%ldms] [%ldms]\n", 
		minAllowedInteTime/1000, maxAllowedInteTime/1000);
	spectrum = (double *)calloc((size_t)spec_length,sizeof(double));
	optInteTime = minAllowedInteTime;
	sbapi_spectrometer_set_integration_time_micros(
                deviceID, spectrometerID, &error, optInteTime);
	sleep(2);

	sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID,
                spectrometerID, &error, spectrum, spec_length);

	max_spec = max_array(spectrum, spec_length);
	printf("\n\toptInteTime [%d], %ld ms, max value: %lf, [%d]",iter, optInteTime/1000, max_spec, flag);
	
	while(((max_spec > max_allowed_DN) || (max_spec < min_allowed_DN))&& (iter < maxiter)&&(max < 2)){
		iter++;
		if (max_spec > 1100){
			ratio = (min_allowed_DN+10000)/(max_spec-1100);
		}else{
			ratio = 100;
		}
		if (max_spec > max_allowed_DN){
			ratio = 0.65;
			max = 0;
		}
		optInteTime = (long)optInteTime * ratio;

		if (optInteTime > maxAllowedInteTime){
			optInteTime = maxAllowedInteTime;
			max++;
		}
		if (optInteTime < minAllowedInteTime){
			optInteTime = minAllowedInteTime;
		}
		sbapi_spectrometer_set_integration_time_micros(
                deviceID, spectrometerID, &error, optInteTime);
		sleep(2);
		printf("\n\toptInteTime [%d], %ld ms",iter, optInteTime/1000);

		sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
		flag = sbapi_spectrometer_get_formatted_spectrum(deviceID,
                	spectrometerID, &error, spectrum, spec_length);
		
		sleep(2);

		max_spec = max_array(spectrum, spec_length);
		printf("\n\toptInteTime [%d], %ld ms, max value: %lf, [%d]",iter, optInteTime/1000, max_spec, flag);

	}
	free(spectrum);
	return optInteTime;
}


//use usb communication to control the internal shutter of QE-pro
void set_internal_shutter_for_QE_pro(long deviceID, int onoff)
{
	int error_code;
	int length;
	int feature_numbers;
	long featureID;
	const unsigned char STS_REQUEST_ENDPOINT = 0x01;
	
	unsigned char set_GPIO4_as_output[64] = {0xc1, 0xc0, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 
		0x10, 0x01, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
		0x10, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0xc5, 0xc4, 0xc3, 0xc2 };
	unsigned char set_GPIO4_on[64] = {0xc1, 0xc0, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 
		0x10, 0x03, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
		0x10, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0xc5, 0xc4, 0xc3, 0xc2 };
	unsigned char set_GPIO4_off[64] = {0xc1, 0xc0, 0x00, 0x11, 0x00, 0x00, 0x00, 0x00, 
		0x10, 0x03, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08,
		0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0x00, 0x00, 0xc5, 0xc4, 0xc3, 0xc2 };
	
	feature_numbers = sbapi_get_number_of_raw_usb_bus_access_features(deviceID, &error_code);
	feature_numbers = sbapi_get_raw_usb_bus_access_features(deviceID, &error_code, &featureID, feature_numbers);	
	sleep(1);
	length = sbapi_raw_usb_bus_access_write(deviceID, featureID, &error_code, set_GPIO4_as_output, sizeof(set_GPIO4_as_output), STS_REQUEST_ENDPOINT);
	printf("\n %d byte of data send to device, set GPIO4 as output, altogether %d feature numbers\n",length,feature_numbers);
	if (onoff==0)
	{
		length = sbapi_raw_usb_bus_access_write(deviceID, featureID, &error_code, set_GPIO4_off, sizeof(set_GPIO4_off), STS_REQUEST_ENDPOINT);
		printf("\n %d byte of data send to device, featureID: %ld, turn on internal shutter [%d]\n",length, featureID, error_code);
	}else if(onoff==1)
	{
		length = sbapi_raw_usb_bus_access_write(deviceID, featureID, &error_code, set_GPIO4_on, sizeof(set_GPIO4_on), STS_REQUEST_ENDPOINT);
		printf("\n %d byte of data send to device, featureID: %ld, turn off the internal shutter [%d]\n",length, featureID, error_code);
	}
	sleep(1);
}



/*
void nonlinearity_correction(double *spectrum, int spec_length, double *nonlinear_coeff, double edark)//calibrate for nonlinearity
{
	double xpower;
	double y;
	
	for (int pixel = 0; pixel < spec_length; pixel++){
		xpower = spectrum[pixel];
		y = nonlinear_coeff[0];
		for (int i = 1; i < 8; i++){
			y +=xpower*nonlinear_coeff[i];
			xpower *= spectrum[pixel];
		}
		spectrum[pixel] /= y;
		spectrum[pixel] += edark;
	}
}
*/

void RemoveSpaces(char *source)
{
	char *temp = source;
	int cursor = 0;
	int n = strlen(source);
	while (cursor < n)
	{
		if (temp[cursor] == ' ')
			temp[cursor] = '_';
		if (temp[cursor] == '\n')
			temp[cursor] = '\0';
		if (temp[cursor] == ':')
			temp[cursor] = '_';
		cursor++;
	}
}


int main(){
	long *device_ids;
	int maxCount = 5; //maximum collection numbers for each operation
	int flag;
	int error = 0;
	int number_of_devices = 0;
	int i;
	char nameBuffer[80];
	//int DetectNonlinCorr;
	//long inteTimeMicrosec1;  //integration time for irradiance
	//long inteTimeMicrosec2;  //intergration time for radiance
	//int ElecDarkCorr;

	//int *wavelengths2;

	
	/* Give the driver a chance to initialize itself */
	sbapi_initialize();

	printf("Probing for devices...\n"); fflush(stdout);
	sbapi_probe_devices();
	
	printf("Getting device count...\n"); fflush(stdout);
	number_of_devices = sbapi_get_number_of_device_ids();
	printf("Device count is %d\n", number_of_devices);
	// get the numbers of the devices
	
	if (0 == number_of_devices)
		return 0;
	
	printf("Getting device IDs...\n");
	device_ids = (long *)calloc(number_of_devices, sizeof(long));
	number_of_devices = sbapi_get_device_ids(device_ids, number_of_devices);
	printf("Got %d device ID%s.\n", number_of_devices, number_of_devices == 1 ? "" : "s");

	for (i = 0; i < number_of_devices; i++){
		printf("\tGetting device type...\n");
		flag = sbapi_get_device_type(device_ids[i], &error, nameBuffer, 79);
		printf("\t\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
		if(flag > 0) {
			printf("\tDevice type: [%s]\n", nameBuffer);
		}
		if(strcmp(nameBuffer,"HR2000PLUS")==0){
			HR2000PLUS_operation(device_ids[i], maxCount);
		}else if(strcmp(nameBuffer,"QE-PRO")==0){
			QE_PRO_operation(device_ids[i], maxCount);
			QE_PRO_fixed_fime_operation(device_ids[i], maxCount);
		}else{
			printf("\n\nUnsupported Spectrometer: %s \n\n", nameBuffer);
			free(device_ids);
			sbapi_shutdown();
			return 0;
		}
	}
	free(device_ids);
	sbapi_shutdown();
	return 0;
}

void HR2000PLUS_operation(long deviceID, int Operation_ct){
	int error;
	int spec_length;
	int number_of_spectrometers = 0;
	int number_of_nonlinearity_coeff_features;
	int flag;
	int length_nonlinear_coeff;
	int number_of_lamps;

	long *lamp_ids = 0;
	long irrInteTime1;
	long irrInteTime2;
	long *spectrometer_ids;
	long *nonlinearity_coeff_feature_ids = 0;

	double *wavelengths;
	double *spectrum1 = 0;
	double *spectrum2 = 0;
	double nonlinear_coeff[8];
	double max_v;
	char fname[100];
	char *dateonly;		//date without day and replace space with "_"	

	char *timestring;
	time_t rawtime;
	struct tm *timeinfo;
	
	/* Open the device */
	printf("\tAttempting to open:\n");
	flag = sbapi_open_device(deviceID, &error);
	printf("\t\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
	
	if(flag != 0) {
		return;
	}
	
	//get system time for the output file name
	rawtime = time(0);
	timeinfo = localtime(&rawtime);
	
	timestring = asctime(timeinfo);
	RemoveSpaces(timestring);
	dateonly = &(timestring[4]);
	printf("\nToday is %s\n",dateonly);
	
	number_of_spectrometers = sbapi_get_number_of_spectrometer_features(deviceID, &error);
	spectrometer_ids = (long *)calloc(number_of_spectrometers, sizeof(long));
	number_of_spectrometers = sbapi_get_spectrometer_features(deviceID, &error,
		spectrometer_ids, number_of_spectrometers);
	
	sbapi_spectrometer_set_trigger_mode(deviceID,spectrometer_ids[0], &error, 0);
	
	//get the name of the output file;
	fname[0] = '\0';
 	strcat(fname,"/home/pi/spectrum_data/");
	strcat(fname,"HR2000PLUS");
	strcat(fname,"_");
	strcat(fname,dateonly);
	strcat(fname,".csv");
	printf("\nOutput file name: %s\n",fname);
	
	//open file and print date and time
	FILE *f = fopen(fname, "wb");
	fprintf(f,"observation date and time: %s\n", asctime(timeinfo));
	//print spectrometers information to file;
	fprintf(f,"spectrometer: HR2000+\n");
	//print system temperature to file:
	fprintf(f,"ambient temperature: N/A\n");
	
	//get the nonlinearity correction coefficients
	number_of_nonlinearity_coeff_features = sbapi_get_number_of_nonlinearity_coeffs_features(deviceID, &error);
	nonlinearity_coeff_feature_ids = (long *)calloc(number_of_nonlinearity_coeff_features, sizeof(long));
	number_of_nonlinearity_coeff_features = sbapi_get_nonlinearity_coeffs_features(
		deviceID, &error, nonlinearity_coeff_feature_ids, number_of_nonlinearity_coeff_features);
	length_nonlinear_coeff = sbapi_nonlinearity_coeffs_get(deviceID, nonlinearity_coeff_feature_ids[0], &error, 
		nonlinear_coeff, 8);
	fprintf(f, "nonlinearity correction coefficients:\n");
	for (int c = 0; c < (length_nonlinear_coeff - 1); c++){
		fprintf(f, "%e,", nonlinear_coeff[c]);
	}
	fprintf(f, "%e\n", nonlinear_coeff[length_nonlinear_coeff - 1]);
	
	/////////////////////////////////////////////////////////////////////////////////////
	//get the wavelength
	spec_length = sbapi_spectrometer_get_formatted_spectrum_length(deviceID, spectrometer_ids[0],&error);
	
	wavelengths = (double *)calloc((size_t)spec_length, sizeof(double));
	flag = sbapi_spectrometer_get_wavelengths(deviceID,
		spectrometer_ids[0], &error, wavelengths, spec_length);
	
	//print wavelengths to file
	for (int m = 0; m < spec_length-1; m++){
		fprintf(f,"%f,", wavelengths[m]);
	}
	fprintf(f,"%f\n",wavelengths[spec_length-1]);
	free(wavelengths);
	/////////////////////////////////////////////////////////////////////////////
	
	printf("\t\t\tAttempting to set trigger mode to 0\n");
        sbapi_spectrometer_set_trigger_mode(deviceID,
                spectrometer_ids[0], &error, 0);

	//collect signal for irradiance
	//set the lamp off to get irradiance
	number_of_lamps = sbapi_get_number_of_lamp_features(deviceID, &error);
	lamp_ids = (long *)calloc(number_of_lamps, sizeof(long));
	number_of_lamps = sbapi_get_lamp_features(deviceID, &error,
		lamp_ids, number_of_lamps);
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 1);
	printf("\n\t\t\tTurn the lamp on to get irradiance %s\n",sbapi_get_error_string(error));
	sleep(2);

	/////////////////////////////////////////////////////////////////////////////////////////////////////
	
	//get optimum integration time1
	irrInteTime1 = get_opt_integration_time_HR2k(deviceID, spectrometer_ids[0]);
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime1/1000, sbapi_get_error_string(error));
	
	//print integration1 time to file
	fprintf(f,"integration time 1: %ld\n", irrInteTime1);

	//collect signal for radiance
	//set the lamp on to get radiance
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 0);
	printf("\n\t\t\tTurn the lamp off to get irradiance %s\n",sbapi_get_error_string(error));
	sleep(2);
	//get optimum integration time2
	irrInteTime2 = get_opt_integration_time_HR2k(deviceID, spectrometer_ids[0]);
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime2/1000, sbapi_get_error_string(error));
	
	//print integration2 time to file
	fprintf(f,"integration time 2: %ld\n\n", irrInteTime2);

	
	if(spec_length > 0){
		//dark correction coefficient
		//dark pixels are [0~17] for HR2000+
		//start the spectrum collection based on the preset sampling numbers
		for (int j = 0; j < Operation_ct; j++){
			
			//turn off the lamp to get spectrum using irrInteTime1
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 1);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
			sleep(2);
			
			spectrum1 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], 
				&error, spectrum1, spec_length);
			//max_v = max_array(spectrum1, spec_length);
			//printf("...Result is (%d) [%s] max_val %lf integration time %ld\n", flag, 
			//	sbapi_get_error_string(error), max_v, irrInteTime1);
			sleep(1);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], 
				&error, spectrum1, spec_length);
			max_v = max_array(spectrum1, spec_length);

			printf("...Result is (%d) [%s] max_val %lf integration time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime1);

			for (int m = 0; m < spec_length-1; m++){				
				fprintf(f,"%lf,", spectrum1[m]);
			}
			fprintf(f,"%lf\n", spectrum1[spec_length-1]);
			sleep(2);

			//turn on the lamp to get spectrum using irrInteTime2
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 0);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
			sleep(2);

			spectrum2 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], 
				&error, spectrum2, spec_length);
			//max_v = max_array(spectrum2, spec_length);
			//printf("...Result is (%d) [%s] max_val %lf integration time %ld\n", flag, 
			//	sbapi_get_error_string(error), max_v, irrInteTime2);
			sleep(1);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], 
				&error, spectrum2, spec_length);
			max_v = max_array(spectrum2, spec_length);
						
			printf("...Result is (%d) [%s] max_val %lf integration time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime2);
			for (int m = 0; m < spec_length-1; m++){				
				fprintf(f,"%lf,", spectrum2[m]);
			}
			fprintf(f,"%lf\n", spectrum2[spec_length-1]);
			free(spectrum1);
			free(spectrum2);

			sleep(2);

		}
	}
	
	
	fclose(f);
	//free(spectrum);
	printf("\n\nAttempting to close:\n");
	sbapi_close_device(deviceID, &error);
	printf("\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
}

void QE_PRO_operation(long deviceID, int Operation_ct){
	int error;
	int spec_length;
	int number_of_spectrometers = 0;
	int number_of_nonlinearity_coeff_features;
	int flag;
	int length_nonlinear_coeff;
	int number_of_lamps;
	int number_of_tec;
	int number_of_buffer;
	long *buffer_ids = 0;
	long *lamp_ids = 0;
	long *tec_ids = 0;
	long irrInteTime1;
	long irrInteTime2;
	long *spectrometer_ids;
	long *nonlinearity_coeff_feature_ids = 0;
	float temperature;
	double *wavelengths;
	double *spectrum1 = 0;
	double *spectrum2 = 0;
	double *spectrum_dark1 = 0;
	double *spectrum_dark2 = 0;
	double nonlinear_coeff[8];
	double max_v;

	char fname[100];
	char *dateonly;		//date without day and replace space with "_"	

	char *timestring;
	time_t rawtime;
	struct tm *timeinfo;
	
	/* Open the device */
	printf("\tAttempting to open:\n");
	flag = sbapi_open_device(deviceID, &error);
	printf("\t\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
	
	if(flag != 0) {
		return;
	}
	
	//get system time for the output file name
	rawtime = time(0);
	timeinfo = localtime(&rawtime);
	
	timestring = asctime(timeinfo);
	RemoveSpaces(timestring);
	dateonly = &(timestring[4]);
	printf("\nToday is %s\n",dateonly);
	
	number_of_spectrometers = sbapi_get_number_of_spectrometer_features(deviceID, &error);
	spectrometer_ids = (long *)calloc(number_of_spectrometers, sizeof(long));
	number_of_spectrometers = sbapi_get_spectrometer_features(deviceID, &error,
		spectrometer_ids, number_of_spectrometers);
	
	sbapi_spectrometer_set_trigger_mode(deviceID,spectrometer_ids[0], &error, 0);
	
	//get tec temperature for QE-pro
	number_of_tec = sbapi_get_number_of_thermo_electric_features(deviceID, &error);
	tec_ids = (long *)calloc(number_of_tec,sizeof(long));
	number_of_tec = sbapi_get_thermo_electric_features(deviceID, &error, tec_ids, number_of_tec);
	
	temperature = (float)sbapi_tec_read_temperature_degrees_C(deviceID, tec_ids[0],&error);
	
	//get the name of the output file;
	fname[0] = '\0';
 	strcat(fname,"/home/pi/spectrum_data/");
	strcat(fname,"QE_PRO");
	strcat(fname,"_");
	strcat(fname,dateonly);
	strcat(fname,".csv");
	printf("\nOutput file name: %s\n",fname);
	
	//open file and print date and time
	FILE *f = fopen(fname, "wb");
	fprintf(f,"observation date and time: %s\n", asctime(timeinfo));
	//print spectrometers information to file;
	fprintf(f,"spectrometer: QE-PRO\n");
	//print system temperature to file:
	fprintf(f,"ambient temperature: %1.2f\n",temperature);
	
	//enable the TEC and set the temperature to 20 degree
	sbapi_tec_set_enable(deviceID, tec_ids[0], &error, 1);
	sbapi_tec_set_temperature_setpoint_degrees_C(deviceID,tec_ids[0],&error, 20.0);
	
	//wait for 30 second for the TEC to work and reach the optimum temperature
	sleep(30);
	
	//enable the TEC and set the temperature to -10 degree
	//sbapi_tec_set_enable(deviceID, tec_ids[0], &error, 1);
	sbapi_tec_set_temperature_setpoint_degrees_C(deviceID,tec_ids[0],&error, -10.0);
	
	//wait for 30 second for the TEC to work and reach the optimum temperature
	sleep(30);
	temperature = (float)sbapi_tec_read_temperature_degrees_C(deviceID, tec_ids[0],&error);
	//print system temperature to file:
	fprintf(f,"measuring temperature: %1.2f\n",temperature);

		
	//get the nonlinearity correction coefficients
	number_of_nonlinearity_coeff_features = sbapi_get_number_of_nonlinearity_coeffs_features(deviceID, &error);
	nonlinearity_coeff_feature_ids = (long *)calloc(number_of_nonlinearity_coeff_features, sizeof(long));
	number_of_nonlinearity_coeff_features = sbapi_get_nonlinearity_coeffs_features(
		deviceID, &error, nonlinearity_coeff_feature_ids, number_of_nonlinearity_coeff_features);
	length_nonlinear_coeff = sbapi_nonlinearity_coeffs_get(deviceID, nonlinearity_coeff_feature_ids[0], &error, 
		nonlinear_coeff, 8);
	fprintf(f, "nonlinearity correction coefficients:\n");
	for (int c = 0; c < (length_nonlinear_coeff - 1); c++){
		fprintf(f, "%e,", nonlinear_coeff[c]);
	}
	fprintf(f, "%e\n", nonlinear_coeff[length_nonlinear_coeff - 1]);
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	spec_length = sbapi_spectrometer_get_formatted_spectrum_length(deviceID, spectrometer_ids[0],&error);
	wavelengths = (double *)calloc((size_t)spec_length, sizeof(double));
	flag = sbapi_spectrometer_get_wavelengths(deviceID,
		spectrometer_ids[0], &error, wavelengths, spec_length);
	//print wavelengths to file
	for (int m = 0; m < spec_length-1; m++){
		fprintf(f,"%f,", wavelengths[m]);
	}
	fprintf(f,"%f\n",wavelengths[spec_length-1]);
	free(wavelengths);
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	printf("\t\t\tAttempting to set trigger mode to 0\n");
        sbapi_spectrometer_set_trigger_mode(deviceID,
                spectrometer_ids[0], &error, 0);

	//collect signal for irradiance
	//set the lamp off to get irradiance
	number_of_lamps = sbapi_get_number_of_lamp_features(deviceID, &error);
	lamp_ids = (long *)calloc(number_of_lamps, sizeof(long));
	number_of_lamps = sbapi_get_lamp_features(deviceID, &error,
		lamp_ids, number_of_lamps);
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 1);
	printf("\n\t\t\tTurn the lamp on to get irradiance [%s]\n",sbapi_get_error_string(error));
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	number_of_buffer = sbapi_get_number_of_data_buffer_features(deviceID, &error);
	buffer_ids = (long *)calloc(number_of_buffer,sizeof(long));
	number_of_buffer = sbapi_get_data_buffer_features(deviceID, &error,
		buffer_ids, number_of_buffer);
	//sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	sleep(2);
	//get optimum integration time1
	irrInteTime1 = get_opt_integration_time_QEpro(deviceID, spectrometer_ids[0]);
	sleep(2);
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime1/1000, sbapi_get_error_string(error));

	//print integration time to file
	fprintf(f,"integration time 1: %ld\n", irrInteTime1);
	
	//turn on the internal shutter to get the dark measurement 
	set_internal_shutter_for_QE_pro(deviceID, 1);
	sleep(2);
	spectrum_dark1 = (double *)calloc((size_t)spec_length, sizeof(double));
	sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum_dark1, spec_length);
	printf("\nGetting a dark measurement spectrum.\n");
	//turn off the internal shutter to get next measurement
	set_internal_shutter_for_QE_pro(deviceID, 0);
	for (int m = 0; m < spec_length-1; m++){				
		fprintf(f,"%lf,", spectrum_dark1[m]);
	}
	fprintf(f,"%f\n",spectrum_dark1[spec_length-1]);
	
	///////////////////////////////////////////////////////////////////////////////////////////////////////
	//set the lamp on for another radiance measurement
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 0);
	printf("\n\t\t\tTurn the lamp off to get irradiance %s\n",sbapi_get_error_string(error));
	sleep(2);
	
	//get optimum integration time2
	irrInteTime2 = get_opt_integration_time_QEpro(deviceID, spectrometer_ids[0]);
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime2/1000, sbapi_get_error_string(error));
	
	//print integration time to file
	fprintf(f,"integration time 2: %ld\n", irrInteTime2);
	
	//turn on the internal shutter to get the dark measurement 
	set_internal_shutter_for_QE_pro(deviceID, 1);
	sleep(2);
	spectrum_dark2 = (double *)calloc((size_t)spec_length, sizeof(double));
	sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum_dark2, spec_length);
	printf("\nGetting a dark measurement spectrum.\n");
	//turn off the internal shutter to get next measurement
	set_internal_shutter_for_QE_pro(deviceID, 0);
	for (int m = 0; m < spec_length-1; m++){				
		fprintf(f,"%lf,", spectrum_dark2[m]);
	}
	fprintf(f,"%f\n\n",spectrum_dark2[spec_length-1]);
	
	free(spectrum_dark1);
	free(spectrum_dark2);
	
	if(spec_length > 0){
		//dark correction coefficient
		//dark pixels are [0~3, 1040~1043] for QE-pro
		//start the spectrum collection based on the preset sampling numbers
		for (int j = 0; j < Operation_ct; j++){
			//turn off the lamp to get spectrum using irrInteTime1
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 1);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
			sleep(2);
			
			spectrum1 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum1, spec_length);			
			max_v = max_array(spectrum1, spec_length);

			printf("...Result is (%d) [%s] max_val %lf integtation time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime1);
			for (int m = 0; m < spec_length-1; m++){				
				fprintf(f,"%lf,", spectrum1[m]);
			}
			fprintf(f,"%lf\n", spectrum1[spec_length-1]);

			sleep(2);
			///////////////////////////////////////////////////////////////////////////////////////
			//turn on the lamp to get spectrum using irrInteTime2
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 0);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
			sleep(2);
			
			spectrum2 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum2, spec_length);			
			max_v = max_array(spectrum2, spec_length);

			printf("...Result is (%d) [%s] max_val %lf integtation time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime2);
			for (int m = 0; m < spec_length-1; m++){				 
				fprintf(f,"%lf,", spectrum2[m]);
			}
			fprintf(f,"%lf\n", spectrum2[spec_length-1]);

			free(spectrum1);
			free(spectrum2);

			sleep(2);
		}
	}
	
	
	fclose(f);

	printf("\n\nAttempting to close:\n");


	sbapi_tec_set_enable(deviceID, tec_ids[0], &error, 0);
	sbapi_close_device(deviceID, &error);
	printf("\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
}

void QE_PRO_fixed_fime_operation(long deviceID, int Operation_ct){
	int error;
	int spec_length;
	int number_of_spectrometers = 0;
	int number_of_nonlinearity_coeff_features;
	int flag;
	int length_nonlinear_coeff;
	int number_of_lamps;
	int number_of_tec;
	int number_of_buffer;
	long *buffer_ids = 0;
	long *lamp_ids = 0;
	long *tec_ids = 0;
	long irrInteTime1 = 2000000;  //for irradiance
	long irrInteTime2 = 1000000;  //for radiance
	long *spectrometer_ids;
	long *nonlinearity_coeff_feature_ids = 0;
	float temperature;
	double *wavelengths;
	double *spectrum1 = 0;
	double *spectrum2 = 0;
	double *spectrum_dark1 = 0;
	double *spectrum_dark2 = 0;
	double nonlinear_coeff[8];
	double max_v;

	char fname[100];
	char *dateonly;		//date without day and replace space with "_"	

	char *timestring;
	time_t rawtime;
	struct tm *timeinfo;
	
	/* Open the device */
	printf("\tAttempting to open:\n");
	flag = sbapi_open_device(deviceID, &error);
	printf("\t\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
	
	if(flag != 0) {
		return;
	}
	
	//get system time for the output file name
	rawtime = time(0);
	timeinfo = localtime(&rawtime);
	
	timestring = asctime(timeinfo);
	RemoveSpaces(timestring);
	dateonly = &(timestring[4]);
	printf("\nToday is %s\n",dateonly);
	
	number_of_spectrometers = sbapi_get_number_of_spectrometer_features(deviceID, &error);
	spectrometer_ids = (long *)calloc(number_of_spectrometers, sizeof(long));
	number_of_spectrometers = sbapi_get_spectrometer_features(deviceID, &error,
		spectrometer_ids, number_of_spectrometers);
	
	sbapi_spectrometer_set_trigger_mode(deviceID,spectrometer_ids[0], &error, 0);
	
	//get tec temperature for QE-pro
	number_of_tec = sbapi_get_number_of_thermo_electric_features(deviceID, &error);
	tec_ids = (long *)calloc(number_of_tec,sizeof(long));
	number_of_tec = sbapi_get_thermo_electric_features(deviceID, &error, tec_ids, number_of_tec);
	
	temperature = (float)sbapi_tec_read_temperature_degrees_C(deviceID, tec_ids[0],&error);
	
	//get the name of the output file;
	fname[0] = '\0';
 	strcat(fname,"/home/pi/spectrum_data/");
	strcat(fname,"fix_QE_PRO");
	strcat(fname,"_");
	strcat(fname,dateonly);
	strcat(fname,".csv");
	printf("\nOutput file name: %s\n",fname);
	
	//open file and print date and time
	FILE *f = fopen(fname, "wb");
	fprintf(f,"observation date and time: %s\n", asctime(timeinfo));
	//print spectrometers information to file;
	fprintf(f,"spectrometer: QE-PRO\n");
	//print system temperature to file:
	fprintf(f,"ambient temperature: %1.2f\n",temperature);
	
	
	//enable the TEC and set the temperature to -10 degree
	sbapi_tec_set_enable(deviceID, tec_ids[0], &error, 1);
	sbapi_tec_set_temperature_setpoint_degrees_C(deviceID,tec_ids[0],&error, -10.0);
	
	//wait for 5 second for the TEC to work and reach the optimum temperature
	sleep(5);
	temperature = (float)sbapi_tec_read_temperature_degrees_C(deviceID, tec_ids[0],&error);
	//print system temperature to file:
	fprintf(f,"measuring temperature: %1.2f\n",temperature);

		
	//get the nonlinearity correction coefficients
	number_of_nonlinearity_coeff_features = sbapi_get_number_of_nonlinearity_coeffs_features(deviceID, &error);
	nonlinearity_coeff_feature_ids = (long *)calloc(number_of_nonlinearity_coeff_features, sizeof(long));
	number_of_nonlinearity_coeff_features = sbapi_get_nonlinearity_coeffs_features(
		deviceID, &error, nonlinearity_coeff_feature_ids, number_of_nonlinearity_coeff_features);
	length_nonlinear_coeff = sbapi_nonlinearity_coeffs_get(deviceID, nonlinearity_coeff_feature_ids[0], &error, 
		nonlinear_coeff, 8);
	fprintf(f, "nonlinearity correction coefficients:\n");
	for (int c = 0; c < (length_nonlinear_coeff - 1); c++){
		fprintf(f, "%e,", nonlinear_coeff[c]);
	}
	fprintf(f, "%e\n", nonlinear_coeff[length_nonlinear_coeff - 1]);
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	spec_length = sbapi_spectrometer_get_formatted_spectrum_length(deviceID, spectrometer_ids[0],&error);
	wavelengths = (double *)calloc((size_t)spec_length, sizeof(double));
	flag = sbapi_spectrometer_get_wavelengths(deviceID,
		spectrometer_ids[0], &error, wavelengths, spec_length);
	//print wavelengths to file
	for (int m = 0; m < spec_length-1; m++){
		fprintf(f,"%f,", wavelengths[m]);
	}
	fprintf(f,"%f\n",wavelengths[spec_length-1]);
	free(wavelengths);
	/////////////////////////////////////////////////////////////////////////////////////////////////////

	printf("\t\t\tAttempting to set trigger mode to 0\n");
        sbapi_spectrometer_set_trigger_mode(deviceID,
                spectrometer_ids[0], &error, 0);

	//collect signal for irradiance
	//set the lamp off to get irradiance
	number_of_lamps = sbapi_get_number_of_lamp_features(deviceID, &error);
	lamp_ids = (long *)calloc(number_of_lamps, sizeof(long));
	number_of_lamps = sbapi_get_lamp_features(deviceID, &error,
		lamp_ids, number_of_lamps);
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 1);
	printf("\n\t\t\tTurn the lamp on to get irradiance [%s]\n",sbapi_get_error_string(error));
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	number_of_buffer = sbapi_get_number_of_data_buffer_features(deviceID, &error);
	buffer_ids = (long *)calloc(number_of_buffer,sizeof(long));
	number_of_buffer = sbapi_get_data_buffer_features(deviceID, &error,
		buffer_ids, number_of_buffer);
	//sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	
	/////////////////////////////////////////////////////////////////////////////////////////////////////
	sleep(2);
	//use fixed integration time1 = 2000 ms
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime1/1000, sbapi_get_error_string(error));

	//print integration time to file
	fprintf(f,"integration time 1: %ld\n", irrInteTime1);
	
	//turn on the internal shutter to get the dark measurement 
	set_internal_shutter_for_QE_pro(deviceID, 1);
	sleep(2);
	spectrum_dark1 = (double *)calloc((size_t)spec_length, sizeof(double));
	sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum_dark1, spec_length);
	printf("\nGetting a dark measurement spectrum.\n");
	//turn off the internal shutter to get next measurement
	set_internal_shutter_for_QE_pro(deviceID, 0);
	for (int m = 0; m < spec_length-1; m++){				
		fprintf(f,"%lf,", spectrum_dark1[m]);
	}
	fprintf(f,"%f\n",spectrum_dark1[spec_length-1]);
	
	///////////////////////////////////////////////////////////////////////////////////////////////////////
	//set the lamp on for another radiance measurement
	sbapi_lamp_set_lamp_enable(deviceID,
		lamp_ids[0], &error, 0);
	printf("\n\t\t\tTurn the lamp off to get irradiance %s\n",sbapi_get_error_string(error));
	sleep(2);
	
	//use fixed integration time2 =1000 ms
	sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
	printf("\n\nSet integration time to %ld ms [%s]\n", irrInteTime2/1000, sbapi_get_error_string(error));
	
	//print integration time to file
	fprintf(f,"integration time 2: %ld\n", irrInteTime2);
	
	//turn on the internal shutter to get the dark measurement 
	set_internal_shutter_for_QE_pro(deviceID, 1);
	sleep(2);
	spectrum_dark2 = (double *)calloc((size_t)spec_length, sizeof(double));
	sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);
	flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum_dark2, spec_length);
	printf("\nGetting a dark measurement spectrum.\n");
	//turn off the internal shutter to get next measurement
	set_internal_shutter_for_QE_pro(deviceID, 0);
	for (int m = 0; m < spec_length-1; m++){				
		fprintf(f,"%lf,", spectrum_dark2[m]);
	}
	fprintf(f,"%f\n\n",spectrum_dark2[spec_length-1]);
	
	free(spectrum_dark1);
	free(spectrum_dark2);
	
	if(spec_length > 0){
		//dark correction coefficient
		//dark pixels are [0~3, 1040~1043] for QE-pro
		//start the spectrum collection based on the preset sampling numbers
		for (int j = 0; j < Operation_ct; j++){
			//turn off the lamp to get spectrum using irrInteTime1
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 1);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime1);
			sleep(2);
			
			spectrum1 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum1, spec_length);			
			max_v = max_array(spectrum1, spec_length);

			printf("...Result is (%d) [%s] max_val %lf integtation time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime1);
			for (int m = 0; m < spec_length-1; m++){				
				fprintf(f,"%lf,", spectrum1[m]);
			}
			fprintf(f,"%lf\n", spectrum1[spec_length-1]);

			sleep(2);
			///////////////////////////////////////////////////////////////////////////////////////
			//turn on the lamp to get spectrum using irrInteTime2
			sbapi_lamp_set_lamp_enable(deviceID,lamp_ids[0], &error, 0);
			sbapi_spectrometer_set_integration_time_micros(deviceID, spectrometer_ids[0], &error, irrInteTime2);
			sleep(2);
			
			spectrum2 = (double *)calloc((size_t)spec_length, sizeof(double));
			printf("\nGetting a formatted spectrum.\n");
			sbapi_data_buffer_clear(deviceID, buffer_ids[0], &error);			
			flag = sbapi_spectrometer_get_formatted_spectrum(deviceID, spectrometer_ids[0], &error, spectrum2, spec_length);			
			max_v = max_array(spectrum2, spec_length);

			printf("...Result is (%d) [%s] max_val %lf integtation time %ld\n", flag, 
				sbapi_get_error_string(error), max_v, irrInteTime2);
			for (int m = 0; m < spec_length-1; m++){				 
				fprintf(f,"%lf,", spectrum2[m]);
			}
			fprintf(f,"%lf\n", spectrum2[spec_length-1]);

			free(spectrum1);
			free(spectrum2);

			sleep(2);
		}
	}
	
	
	fclose(f);

	printf("\n\nAttempting to close:\n");


	sbapi_tec_set_enable(deviceID, tec_ids[0], &error, 0);
	sbapi_close_device(deviceID, &error);
	printf("\tResult is (%d) [%s]\n", flag, sbapi_get_error_string(error));
}
