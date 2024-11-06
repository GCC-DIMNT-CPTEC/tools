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
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.nc	
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024070900_2024070903.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024070900_2024070906.x1024002L55.nc	
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024070900_2024070912.x1024002L55.nc
wget -nc ftp1.cptec.inpe.br/avalia/avalia/samples/monan/nc/MONAN_DIAG_G_POS_GFS_2024070900_2024071000.x1024002L55.nc
# Option -nc : no clobber


