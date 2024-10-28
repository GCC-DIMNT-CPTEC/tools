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

DIR=./dataout
START_TIME=2024100100
for fct in 000 006 012 018 024 ; do
   source ./get_date.sh ${START_TIME} $fct
   END_TIME=$yy2$mm2$dd2$hh2
   INPUT_GRIB=./MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.grib2
   OUTPUT_NC=./MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.nc
   OUTPUT_GRB1=./MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.grib1
   OUTPUT_CTL=./MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.ctl
   OUTPUT_TXT=./MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.txt
  cd $DIR
  #
  # Conversao GRIB2 para NETCDF com CDO
  #
  cdo -f nc setgridtype,regular $INPUT_GRIB $OUTPUT_NC

  #
  # Conversao GRIB2 para GRIB1 com CDO
  #
  #cdo -f grb copy $INPUT_GRIB $OUTPUT_GRB1

  #
  # GRIBMAP para abertura no GRADS
  #

  g2ctl -verf   $INPUT_GRIB > $OUTPUT_CTL
  gribmap -i $OUTPUT_CTL

  #
  # grib_dump
  #
   grib_dump $INPUT_GRIB > $OUTPUT_TXT
  cd -
done
