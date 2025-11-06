#!/bin/bash
#----------------------------------------------------------------------------#
#  run_g2ctl: A CTL and iDX files are produced from grib2 to be used in      #
#             grads software                                                 #
#----------------------------------------------------------------------------#
echo "HOST="$a
if [[ $a == *"egeon"* ]]; then
	module load cdo-2.0.4-gcc-9.4.0-bjulvnd
	module load netcdf-fortran
	export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3
else
	export NC2GRIB_DIR=../..
fi

monandir="tc/1.4.1-rc/P1"
sufix='x1024002L55'

dirout=./dataout

start_time=2025102600
for fff in 000 024; do
   source ./get_date.sh ${start_time}${fff}
   forecast_time=$yy2$mm2$dd2$hh2

   dirout=./dataout
   cd $dirout
   gribfile='./MONAN_DIAG_G_POS_GFS_'${start_time}_${forecast_time}'.'$sufix'.grib2'

   if [ -f "$gribfile" ]; then
      ls -ltr $gribfile
      g2ctl -verf $gribfile >  $gribfile.ctl
      gribmap -i $gribfile.ctl
   else
      echo "ERROR!: File not found"
      echo $gribfile
   fi
  cd -

done
