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
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024100100_2024100100.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024100100_2024100106.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024100100_2024100112.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024100100_2024100118.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024100100_2024100200.x1024002L55.nc


