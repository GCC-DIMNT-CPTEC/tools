#!/bin/bash
#-----------------------------------------------------------------
# Download MONAN data to convert from NETCDF to grib2
#
# Note:
# the original NETCDF files with all forecast times were divided
# into files with a single forecast time using CDO
#----------------------------------------------------------------


SCRIPT=`realpath $0` 
RUNDIR=`dirname $SCRIPT`
cd $RUNDIR

mkdir -p ./datain
cd ./datain
for fff in 000 006 012 018 024 ; do 
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/monan.t00z.atm.0p25.f$fff.2021062600.nc
done

