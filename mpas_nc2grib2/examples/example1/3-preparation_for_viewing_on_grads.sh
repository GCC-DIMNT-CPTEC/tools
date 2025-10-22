#!/bin/bash
a=`hostname`
echo $a
if [[ $a == *"egeon"* ]]; then
	module load cdo-2.0.4-gcc-9.4.0-bjulvnd
	module load netcdf-fortran
	export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3
else
	export NC2GRIB_DIR=../
fi
GRIBMAP=/home/sergio.ferreira/GRADS/grads_2.0.a9/bin/gribmap
DIR=./dataout
start_time=2024070900
cd $DIR
for fff in 000 003 006 012 024 ; do
   source ../get_date.sh ${start_time} $fff
   END_TIME=$yy2$mm2$dd2$hh2
   INPUT_GRIB=./MONAN_DIAG_G_POS_GFS_${start_time}_${END_TIME}.x1024002L55.grib2
   OUTPUT_CTL=./MONAN_DIAG_G_POS_GFS_${start_time}_${END_TIME}.x1024002L55.ctl
  #
  # generating ctl and idx files
  #
  cd $DIR
  g2ctl -verf   $INPUT_GRIB > $OUTPUT_CTL
  $GRIBMAP -i $OUTPUT_CTL
  

done


