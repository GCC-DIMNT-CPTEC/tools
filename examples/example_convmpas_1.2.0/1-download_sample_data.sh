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
wget -nc -O ./MONAN_DIAG_G_POS_GFS_2026082800_2026082800.00.00.x5898242L55.nc "https://www.dropbox.com/scl/fi/qofh2eww2f8v7i0quxdb6/MONAN_DIAG_G_POS_GFS_2026082800_2026082800.00.00.x5898242L55.nc?rlkey=rfd219caqb0xhvlqahhyji41x&st=qao0p5hh&dl=1" 
wget -nc -O .//MONAN_DIAG_G_POS_GFS_2026082800_2026082806.00.00.x5898242L55.nc "https://www.dropbox.com/scl/fi/akq8634uucsxqyw6l0thl/MONAN_DIAG_G_POS_GFS_2026082800_2026082806.00.00.x5898242L55.nc?rlkey=r6mucceyxd59iizx63bgx33df&st=08jf779h&dl=1"
wget -nc -O ./MONAN_DIAG_G_POS_GFS_2026082800_2026082812.00.00.x5898242L55.nc "https://www.dropbox.com/scl/fi/o2bx927lhscdcjc7f4en6/MONAN_DIAG_G_POS_GFS_2026082800_2026082812.00.00.x5898242L55.nc?rlkey=gln3kgil2rvtzec6zp70aeh0q&st=qsfbb6nq&dl=1"
wget -nc -O ./MONAN_DIAG_G_POS_GFS_2026082800_2026082818.00.00.x5898242L55.nc "https://www.dropbox.com/scl/fi/jjrxrvtamr3ogddmh7iz5/MONAN_DIAG_G_POS_GFS_2026082800_2026082818.00.00.x5898242L55.nc?rlkey=oxpah8vvr6bnw1ag3z6dpqzw6&st=1fp1ca8q&dl=1"
# Option -nc : no clobber (sem sobreescrever)


