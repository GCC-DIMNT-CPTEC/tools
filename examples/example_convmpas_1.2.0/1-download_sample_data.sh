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
wget -nc "https://www.dropbox.com/scl/fi/qofh2eww2f8v7i0quxdb6/MONAN_DIAG_G_POS_GFS_2026082800_2026082800.00.00.x5898242L55.nc?rlkey=rfd219caqb0xhvlqahhyji41x&st=qao0p5hh&dl=1"
# Option -nc : no clobber (sem sobreescrever)


