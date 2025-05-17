#!/bin/bash
a=`hostname`
echo $a
if [[ $a == *"egeon"* ]]; then
	module load cdo-2.0.4-gcc-9.4.0-bjulvnd
	module load netcdf-fortran
	export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3
else
	export NC2GRIB_DIR=../../
fi

dirin=./datain
dirout=./dataout
mkdir -p $dirout

fileout=$dirout/monan.t00z.atm.Op25_%Y4%M2%D2%H2_%y4%m2%d2%h2

start_time=2021062600

for fff in 000 006 012 018 024 ; do
    filein=$dirin/monan.t00z.atm.0p25.f${fff}.${start_time}.nc 
    #cdo -f grb2 -copy $filein $filein.grib2
    $NC2GRIB_DIR/bin/mpas_nc2grib2.x -i $filein -o $fileout -s $start_time -f $fff -v 3 -c nc2grib.2_v2.csv

done
