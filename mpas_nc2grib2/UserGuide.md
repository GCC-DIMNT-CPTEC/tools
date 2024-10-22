# MPAS_NC2GRIB2 USER GUIDE

This document describe how to use MPAS_NC2GRIB2  software reads MONAN data files in netcdf format and writes them to FM92-GRIB (version 2)

## 1-Introduction

The MPAS_NC2GRIB2 is a software to convert data from MPAS/MONAN in  netcdf format to grib2 format using a configuration table, that associates the particular identifiers for each variable in netcdf, to the variables or conversion processes required for correct encoding in GRIB2.   So Spetiall attentions is required to this table, that is described in the intem ()

Other importat aspected, it that the software was writed in fortran 90 and use netcdf librarie and eccodes libraries. Some functionalitis of eccodes is present, one of this is the use of the diferents packing type, witch is associated with different file compression.  In the present version, there a are options two options, as show in the next topic

## 2 - Options and usage
To see the usage options for mpas_nc2grib2 , simply type mpas_nc2grib2.x at the command line.

	|--------------------------------------------------------------+
    | mpas_nc2grib2.x                                              |
 	|--------------------------------------------------------------+
 	| MCTI-INPE (2024-07-08) V. 1.0                                |
 	|--------------------------------------------------------------+
 	| Use mpas_convert1.x -i <filename.nc> -o <outfile_basename>   |
 	|                          -s yyyymmddhh -f ffff  {-r <option>}|
 	|                                                              |
 	|    -s start_time: (yyyymmddhh)                               |
 	|    -f forecast time  (fff)                                   |
 	|                                                              |
 	|    -p :packing type                                          |
 	|        :0=  grid_simple                                      |
 	|        :1=  grid_ccsds                                       |
 	|                                                              |
 	|     Default: 0                                               |
 	|--------------------------------------------------------------+

 It is important to note that to the condification in grib2, the information about  the start time and forecast time must be informed and this information are not rigid declared in netcdf. So the program "spected" this information as parameter and it will use in codification process as well as to create the output filename.
 
  The start time must be provide in the format yyyymmddhh (year,month day and hour UTC)

  The forecast time must be provide in the format fff (time in hour from star time) : Examples 000, 006, 012, etc.

   The <filename.nc> is the netcddf file name to be convert in grib2

   the <output_basenae> is the file name for  grib file, but in this case, it cam be write using special notition to use start time and forecast time as parte of the output name. 

 - Use the notation **%Y4 %M2 %D2 %H2 (capital letters)**  to represent year, month, day and hour or the **start time**
 
  -Use the notation **%y4 %m2 %d2 %h2 (lowercase letters)** to represent year, month, day and hour or the** forecast time 

  The example bellow show the use of program to convert a netcdf file 
 define 
  Note that the software request 4 mandatory parameters:  
   - 1  <filename.nc> is the netcdf filename to be converted
   - 2 
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
The scripts to run MPAS_NC2GRIB software with examples file is in the  "examples" diretory.  Goto the "examples" and run:

 - 1-download_sample_data.sh: This script download a set off MPAS_MONAN NETCDF file from ftp1.cptec.inpe.br and places them in **./datain** 
 - Type **ls -ltr ./datain** to verify if the files were download.

-  2-run_mpas_nc2grib.sh: This scripts runs MPAS_NC2GRIB to convert the MPAS_FILE in datain to GRIB2 file in dataout.  

		MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070903.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070906.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024070912.x1024002L55.grib2
		MONAN_DIAG_G_POS_GFS_2024070900_2024071000.x1024002L55.grib2
  - Type **ls -ltr ./dataout** to verify if the grib files were generated 
  - 3-grib_decoding_test.sh: This script do the inverse process, i.e. decoding the grib file to nc as well as into other formats. 