# MPAS_NC2GRIB2 USER GUIDE

This document describe how to use MPAS_NC2GRIB2  software reads MONAN data files in netcdf format and writes them to FM92-GRIB (version 2)

## 1-Introduction

The MPAS_NC2GRIB2 is a software to convert data from MPAS/MONAN in  netcdf format to grib2 format using a configuration table, that associates the particular identifiers for each variable in netcdf, to the variables or conversion processes required for correct encoding in GRIB2.   So Spetiall attentions is required to this table, that is described in the iten ()

Other importat aspected, it that the software was writed in fortran 90 and use netcdf librarie and eccodes libraries. Some functionalitis of eccodes is present, one of that is the use of the diferents packing type, witch is associated with different file compression.  In the present version, there a are  two options, as show in the next topic

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

 It is important to note that  the information about  the start time and forecast time must be informed and this information are not rigid declared in netcdf. So the program "spected" this information as parameter and it will use in codification process as well as to create the output filename.
 
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
