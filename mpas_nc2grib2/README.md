# MPAS_NC2GRIB2

This software reads MONAN data files in netcdf format and writes them to FM92-GRIB (version 2)

## 1-Compilation Instructions

To compile MPAS_NC2GRIB on linux, it is necessary to previously install of **ECCODES** and **netcdf-fotran** libraries.  To use the **grid_ccsds packing type** to produce more compressed grib be sure that ECCODES was compiled with the option -DENABLE_AEC=ON (Adaptive Entropy Coding library). The  **libaec** must be istalled or/and loaded.


### 1.1 - Compilation in INPE egeon
 To compile in egeon.cptec.inpe.br with gfortran use: 

	 module load netcdf-fortran
	 module load libaec-1.0.5-gcc-9.4.0-4qmyxr4
	 ln -s makefile_config_egeon makefile_config
	 make 

### 1.2 - Compilation in a linux desktop  
 To compile in a linux desktop with gfortran 

	 ln -s makefile_config_lnx makefile_config
	 make 

To use other fortran compilers or other compiler options, edit makefile_config_lnx 

## 2 - Setting environment variables

Before run MPAS_NC2GRIB2 It is necessay set **ECCODES_DIR** and **NFDIR** environments variables.  

### 2.1 ECCODES_DIR
Set ECCODES_DIR with path where ECCODES software is. This definition is necessary  to software find the grib definitions tables used in coding / decoding process. As example, in case bash terminal  edit the .bashrc file and add the follow commnad.


	export ECCODES_DIR=/home/User_name/eccodes

**Note:** *The eccodes directory usualy contains the follow directories: bin, include, lib, share. The "share" is the directories where the grib definitions tables are.* 

### 2.2 NFDIR
Set the NFDIR with the path where  NETCDF-FORTRAN is.  As examples:
1) In usual instalation add in .bashrc environmet variable the commnad
   
		export NFDIR=/usr/local

2) In egeon.cptec.inpe.br use

		export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

### 2.3 NC2GRIB_DIR
Set the NC2GRIB_DIR with the path where mpas_nc2grib directory is. It is necessary to software find specific configuration in the  mpas_nc2grib2/settings directory.
Example

		export NC2GRIB_DIR=/home/User_name/mpas_nc2grib2

# 3 - Checking if MPAS_NC2GRIB2 was sucessifully compiled
 Type the follow commnad

		./bin/mpas_nc2grib2.x



# 4 - Running the examples for test

## 4.1 - example1 - Generic example
Generic scripts to run MPAS_NC2GRIB software with examples is in the  "examples/example1" diretory.  So, go to the "examples/example1" and run the followed scripts:

 - 1-download_sample_data.sh: This script download a set off MPAS_MONAN NETCDF file from ftp1.cptec.inpe.br and places them in **./datain** 
 	- Type **ls -ltr ./datain** to verify if the files were download.

-  2-running mpas_nc2grib.x: Use one of followed scripts to run mpas_nc2grib.x:

	- a) run_mpas_nc2grib2.sh (with grid simple packing type)
	- b) run_mpas_nc2grib2_cssds.sh (with ccsds packing type - more compressed option)

The scripts runs MPAS_NC2GRIB to convert the MPAS_FILE in datain to GRIB2 file in dataout.  

		MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070903.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070906.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070912.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024071000.x1024002L55.grib2
  - Type **ls -ltr ./dataout** to verify if the grib files were generated 
  - 3-grib_decoding_test.sh: This script do the inverse process, i.e. decoding the grib file to nc as well as into other formats. 

## 4.2 - example2 - Specific example to be use as test in egeon.cptec.inpe.br
 
In the **examples/example2** you will find some scripts to run mpas_nc2grib2.x on Egeon to process data form **MPAS_CONVERT**. In these scripts, the path to access the MONAN data results in netcdf as well as the path to the directory for grib2 have been hard-coded defined. Therefore, before using these scripts, it is recommended to edit and modify these scripts  to make the necessary adjustments.

## 4.3 Others examples.
  Read the specific **readme.txt** in the example directory for other examples 


## 5 - USER GUIDE ##

For more details about use and configuration click on :

[user guide](https://github.com/GCC-DIMNT-CPTEC/tools/blob/master/mpas_nc2grib2/UserGuide.md)
