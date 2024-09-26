# MPAS_NC2GRIB2

The MPAS_NC2GRIB2 is a software to read the MONAN dataout in NETCDF format and write in FM92-GRIB (Version2)

## 1-Compilation Instructions

To compile MPAS_NC2GRIB on linux, it is necessary the previous instalation of EcCodes and netcdf-fotran libraries 

### 1.1 - Compilation in INPE egeon
 To compile in egeon.cptec.inpe.br with gfortran use: 

	 module load netcdf-fortran
	 ln -s makefile_config_egeon makefile_config
	 make 

### 1.2 - Compilation in a linux desktop  
 To compile in a linux desktop with gfortran 

	 ln -s makefile_config_lnx makefile_config
	 make 

To use other fortran compilers or other compiler options, edit makefile_config_lnx 

## 2 - Setting environment variables

It is necessay set ECCODES_DIR and NFDIR environments variables.  

### 2.1 ECCODES_DIR
Set ECCODES_DIR with path where ECCODES software is to software find the grib definitaions tables used in coding / decoding process.  As example,  in case the ECCODES was installed in user home directory, and in case of bash environement, edit the .bashrc file and add the follow commnad.

	export ECCODES_DIR=/home/User_name/eccodes

Note: the eccodes directory usualy contains the follow directories: bin, include, lib, share. The "share" it the directories where the grib definitions tables are. 

### 2.2 NFDIR
Set the NFDIR with the path where  NETCDF-FORTRAN is.  As examples:
1) In usual instalation add in .bashrc environmet variable the commnad
   
		export NFDIR=/usr/local

2) In egeon.cptec.inpe.br use

		export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

# 3 - Checking if MPAS_NC2GRIB2 was sucessifully compiled
 Type the follow commnad

		./bin/mpas_nc2grib2.x



# 4 - Running the examples to text

The scripts to run MPAS_NC2GRIB software with examples file is in the  "examples" diretory.  Goto the "examples" and:

 - Run '1-download_sample_data.sh' to download some  MPAS_MONAN NETCDF for test from ftp1.cptec.inpe.br to ./datain.  Type ls -ltr ./datain to verify if the file have been download:

		MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.nc
 		MONAN_DIAG_G_POS_GFS_2024070900_2024070903.x1024002L55.nc
	 	MONAN_DIAG_G_POS_GFS_2024070900_2024070906.x1024002L55.nc
		MONAN_DIAG_G_POS_GFS_2024070900_2024070912.x1024002L55.nc
		MONAN_DIAG_G_POS_GFS_2024070900_2024071000.x1024002L55.nc
 - Run '2-run.sh' to run MPAS_NC2GRIB to convert the MPAS_FILE to GRIB2 file. The grib2 files are placed in ./dataout 

		MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070903.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070906.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070912.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024071000.x1024002L55.grib2
