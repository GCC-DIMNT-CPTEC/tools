#!/bin/bash
module load cdo-2.0.4-gcc-9.4.0-bjulvnd
module load netcdf-fortran
export NC2GRIB_DIR=../
export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

DIR=./dataout
START_TIME=2024070900
for fct in 000 003 006 012 024 ; do
   source ./.get_date.sh ${START_TIME} $fct
   END_TIME=$yy2$mm2$dd2$hh2
   INPUT_GRIB=./dataout/MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.grib2
   OUTPUT_NC=./dataout/MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.nc
   OUTPUT_CTL=./dataout/MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.ctl
   OUTPUT_TXT=./dataout/MONAN_DIAG_G_POS_GFS_${START_TIME}_${END_TIME}.x1024002L55.txt
   cd $DIR
  #
  # Conversao para NETCDF com CDP
  #
  cdo -f nc setgridtype,regular $INPUT_GRIB $OUTPUT_NC

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
