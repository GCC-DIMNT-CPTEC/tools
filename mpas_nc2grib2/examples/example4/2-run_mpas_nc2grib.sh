#!/bin/bash
a=`hostname`
echo "HOST="$a
if [[ $a == *"egeon"* ]]; then
	module load cdo-2.0.4-gcc-9.4.0-bjulvnd
	module load netcdf-fortran
	export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3
else
	export NC2GRIB_DIR=../..
fi

monandir="tc/1.4.1-rc/P1"
sufix='x1024002L18.nc'

dirout=./dataout

start_time=2024050100
for fff in 000 ; do 
   source ./get_date.sh ${start_time}${fff}
   forecast_time=$yy2$mm2$dd2$hh2

   dirin=./datain/$monandir/$yy0$mm0$dd0$hh0/Post
   dirout=./dataout/GRIB2/$yy0$mm0$dd0$hh0
   echo "Origin="$dirin
   echo "Destino="$dirout
   mkdir -p $dirout
   
   file='MONAN_DIAG_G_POS_GFS_'${start_time}'_'${forecast_time}'.00.00.'$sufix
   filein=$dirin/$file
   fileout=$dirout'/MONAN_DIAG_G_POS_GFS_%Y4%M2%D2%H2_%y4%m2%d2%h2.'$sufix

   if [ -f "$filein" ]; then
      ls -ltr $filein
      echo "mpas_nc2grib2.x -i "$filein" -o "$fileout" -s "$yy0$mm0$dd0$hh0" -f "$fff" -v 3"
      #mpas_nc2grib2.x -i $filetmp -o $fileout -s $yy0$mm0$dd0$hh0 -f $fff -v 3
   else
      echo "ERROR!: File not found"
      echo $filein
   fi

done
