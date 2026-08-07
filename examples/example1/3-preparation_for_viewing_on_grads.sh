#!/bin/bash
a=`hostname`
echo $a
if [[ $a == *"egeon"* ]]; then
	module load cdo-2.0.4-gcc-9.4.0-bjulvnd
	module load netcdf-fortran
	export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3
	export NC2GRIB_DIR=../..
	export GRIBMAP=gribmap
elif [[ $a == *"ian"* ]]; then
    module load cray-netcdf-hdf5parallel/4.9.0.15
    module load libfabric
    module load grads/2.2.1.oga.1
    module load wgrib2/3.8.0
    export NFDIR=${NETCDF_DIR}
    export NC2GRIB_DIR=../..
    export GASCRP=/p/app/opengrads/2.2.1.oga.1/Resources/Scripts
    export GRIBMAP=gribmap
    export G2CTL=../../../extensions/g2ctl
else
	export NC2GRIB_DIR=../..
	export GRIBMAP='/home/sergio.ferreira/GRADS/grads_2.0.a9/bin/gribmap'
	export G2CTL='g2ctl'
fi


#DIR=./dataout_all32
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
  $G2CTL -verf   $INPUT_GRIB > $OUTPUT_CTL
  pwd -P
  echo $GRIBMAP' -i '$OUTPUT_CTL
  $GRIBMAP -i $OUTPUT_CTL
  
done


