# MPAS_NC2GRIB2 USER GUIDE

This document describe how to use MPAS_NC2GRIB2  software reads MONAN data files in netcdf format and writes them to GRIB2 format

## 1-Introduction

The MPAS_NC2GRIB2 is a software to convert data from MPAS/MONAN in netcdf format to GRIB2 format using a **configuration table**, that associates each variable identifier in netcdf, to the variables or conversion processes required for correct encoding in GRIB2.   So Spetiall attentions is required to this table, that is described in the item 3

Other importat aspected, it that the software was writed in fortran 90 and use netcdf librarie and eccodes libraries. Some functionalitis of eccodes is present, one of that is the use of the diferents packing type, witch is associated with different file compression.  In the present version, there a are two options, as show in the next topic

## 2 - Options and usege
To see the usage options for mpas_nc2grib2 , simply type mpas_nc2grib2.x at the command line.


	 | mpas_nc2grib2.x                                              |
 	|--------------------------------------------------------------+
 	| MCTI-INPE (2024-11-06) V. 1.1                                |
 	|--------------------------------------------------------------+
 	| Use mpas_nc2grib2.x -i <filename.nc> -o <outfile_basename>   |
 	|                          -s yyyymmddhh -f fff  { Options }   |
 	|                                                              |
 	|    -s start_time: (yyyymmddhh)                               |
 	|    -f forecast time  (fff)                                   |
 	|                                                              |
 	| Options:                                                     |
	|    -p :packing type                                          |
 	|        :0=  grid_simple                                      |
 	|        :1=  grid_ccsds                                       |
 	|        (Default: 0)                                          |
 	|                                                              |
 	|    -c :<Configuration table name>                            |
 	|        (Default= nc2grib.2.csv )                             |
 	|--------------------------------------------------------------+

 It is important to note that the information about the start time and forecast time must be informed and this information are not rigid declared in netcdf. So the program "spected" this information as parameter and it will use in codification process as well as to create the output filename.
 
 ### 2.1 - Start time and forecast time
  The start time must be provide in the format yyyymmddhh (year,month day and hour UTC)

  The forecast time must be provide in the format fff (time in hour from star time) : Examples 000, 006, 012, etc.

   The <filename.nc> is the netcddf file name to be convert in grib2

### 2.2 - Output basename 
   the <output_basename> is the output file name in grib2 format, but in this case, it cam be write using special notition to represent  start time and forecast time:

 - Use the notation **%Y4 %M2 %D2 %H2 (capital letters)**  to represent year, month, day and hour of the **start time**
 
  - Use the notation **%y4 %m2 %d2 %h2 (lowercase letters)** to represent year, month, day and hour of the **forecast time** 

  The software automaticaly fill the start time and forecast time in the file name in acordance with internall values in the files.
  
### 2.3 - Packing type

Im present version there are only 2 available optins for type avaliable:

 - 0 = grid_simple (Default options)
 - 1 = grid_ccsds ( Is a most compressible packing type)

 The **CCSDS** or Consultative Committee for Space Data Systems,  is a WMO standard for lossless data compression.  It is implemented by libAEC  **(Adaptative Entropy Coding library)**. So to use this options be sure that  libAEC was On in the compilation of ECCODES in your system.

### 2.4 - Verbosity

Use the parameter -v with 0,1,2 or 3  (Defaut is 0) to set the quantity of information that will be displayed on the screen during program executions. 0- Only most important information is displayed


 ## 3 - The Configuration Table

As previously said, the MPAS_NC2GRIB2  use a **configuration table**, that associates each variable identifier in netcdf, to the variables or conversion processes required for correct encoding in GRIB2.

The software use the configuration table: **nc2grib.2.csv**  in settings directory by default. Other tables cam be adopted using the option -c <configuration_table>


It is important to highlight that only the variables included in this table will be converted from netcdf to grib2. All others will be disregarded.

Another important point is to correctly relate the variable in netcdf with the corresponding variable in GRIB2.  This is not an easy task. It is important to highlight that the GRIB format is a format governed by the WMO that strictly defines each variable in GRIB. Therefore, it is necessary to be sure of the meaning, the obtaining processes and the units of each variable, to avoid conversion errors.

See the WMO GRIB2 tables, or use the **table_viewer.py** in settins directory to consult some GRIB2 tables. 

A exemple of the nc2 grib2.csv containt is shown below. See the file nc2grib2.csv in setting directory to see the completed table

        
 | NC_Name|cfVarName|Templete| Discipline|Category|ParNumber|tflevel|sValueFFS|sFactorFFS|timeint|Varname|
 |---|---|---|---|---|---|---|---|---|---|---|
 |t2m|t2m|0| 0|0| 0|103| 2| 0| 0|2 metre temperature|
 |q2|sh2|0|0|1|0|103|2|0|0|2 metre specific humidity|
 |u10|u10|0|0|2|2|103|10|0|0|10 metre U wind component|
 |v10|v10|0|0|2|3|103|10|0|0|10 metre V wind component|
 |zgeo|gh|0|0|3|5|100|0|0|0|Geopotential height (gpm)|

Here we have a explanation of each collums in this table
 - **NC_NAME** = Identification of a variable netcdf. Use ncdump software to see all varibles in netcdf
 - **cfVarName** = Identification of the variable as used in ECMWF and used in the cfVarName.def file. In this file, many important definitions of grib have already been made. But the current version of MPAS_NC2GRIB does not use this definition. The definitions in the subsequent columns are used instead.
 - **Template** = Grib Template Number (see grib table )

Under construction
