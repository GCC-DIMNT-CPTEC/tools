#!/bin/bash

# In egeon type
#module load cdo-2.0.4-gcc-9.4.0-bjulvnd
#module load netcdf-fortran
#export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

export NC2GRIB_DIR=../
dirin=./datain
dirout=./dataout
mkdir -p $dirout

filein=$dirin/MONAN_DIAG_G_POS_GFS_2024062700_2024062700.x1024002L55.nc
fileout=$dirout/MONAN_DIAG_G_POS_GFS_%Y4%M2%D2%H2_%y4%m2%d2%h2.x1024002L55

start_time=2024070900

for fff in 000 003 006 012 024 ; do
    source ./get_date.sh ${start_time}${fff}
    forecast_time=$yy2$mm2$dd2$hh2

    filein=$dirin/MONAN_DIAG_G_POS_GFS_${start_time}_${forecast_time}.x1024002L55.nc

    ../bin/mpas_nc2grib2.x -i $filein -o $fileout -s $start_time -f $fff -v 1
done
