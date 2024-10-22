#!/bin/bash
module load netcdf-fortran
module load cdo-2.0.4-gcc-9.4.0-bjulvnd
export NC2GRIB_DIR='/mnt/beegfs/sergio.ferreira/svn/dynameke/energetica/branches/develop/io_tools/mpas_nc2grib'
export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

if [ $# = 0 ]; then
   echo "---------------------------------------------------------"
   echo "Runs mpas_nc2grib2.x to convert data from netcdf to grib2"
   echo "---------------------------------------------------------"
   echo "use:"
   echo "     ./1-run_convert2grib.sh <option> "
   echo "             <option>=: now,"
   echo "                      : run,"
   echo "                      : yesterday"
   echo "                      : <yyyymmdd>"
   echo "                      : 20240708"
   echo "exit"
   exit
fi

run=$1
source ./get_date.sh $run 
hh0=00
run=$yy0$mm0$dd0$hh0
for fff in {0..120..3}  ;  do 
   echo ">"$fff
   source ./get_date.sh $run  $fff
    
   
   dirin=/mnt/beegfs/monan/scripts_CD-CT_dev/scripts_CD-CT/dataout/$yy0$mm0$dd0$hh0/Post
   dirtmp=/mnt/beegfs/sergio.ferreira/DADOS/MONAN/nc/$yy0$mm0$dd0$hh0
   dirout=/mnt/beegfs/sergio.ferreira/DADOS/MONAN/GRIB2/$yy0$mm0$dd0$hh0
   mkdir -p $dirtmp
   mkdir -p $dirout
   file='MONAN_DIAG_G_POS_GFS_'$yy0$mm0$dd0$hh0'.00.00.x1024002L55.nc'
   filein=$dirin/$file
   filetmp=$dirtmp'/MONAN_DIAG_G_POS_GFS_'$yy0$mm0$dd0$hh0'_'$yy2$mm2$dd2$hh2'.x1024002L55.nc'
   fileout=$dirout'/MONAN_DIAG_G_POS_GFS_%Y4%M2%D2%H2_%y4%m2%d2%h2.x1024002L55'
   par1=$yy2'-'$mm2'-'$dd2'T'$hh2':00:00'
   par2=$yy2'-'$mm2'-'$dd2'T'$hh2':00:00'
   echo $par1'-'$par2
   ls -ltr $filein
   cdo select,startdate=$par1,enddate=$par2 $filein $filetmp
   #ncdump -h $filetmp > $filetmp.txt
   mpas_nc2grib2.x -i $filetmp -o $fileout -s $yy0$mm0$dd0$hh0 -f $fff -v 3


done 

