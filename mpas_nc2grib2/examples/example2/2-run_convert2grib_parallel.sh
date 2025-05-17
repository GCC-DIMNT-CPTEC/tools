#!/bin/bash
#SBATCH --job-name=post
#SBATCH --nodes=1
#SBATCH --ntasks=81
#SBATCH --partition=batch
#SBATCH --time=4:00:00
#SBATCH --mem=500000
#SBATCH --output=./logs/my_job_grib2.o%j # Standard output
#SBATCH --error=./logs/my_job_grib2.e%j  # Standard error output
#
#
BASEDIR=$(dirname $(pwd))
echo $BASEDIR

#ls $BASEDIR/example3

EXEDIR=$BASEDIR/../../../bin
echo $EXEDIR

module load netcdf-fortran
module load libaec-1.0.5-gcc-9.4.0-4qmyxr4
module load cdo-2.0.4-gcc-9.4.0-bjulvnd
module load libjpeg-turbo-2.1.0-gcc-9.4.0-bq4j6d4
module load openjpeg-2.3.1-gcc-9.4.0-dxbqvt2 libpng-1.6.37-gcc-9.4.0-g6znw34
module load jasper-3.0.3-gcc-11.2.0-yoivik3
module list


export NC2GRIB_DIR='/mnt/beegfs/sergio.ferreira/GIT/GCC-DIMNT-CPTEC/tools/mpas_nc2grib2/'
export NFDIR=/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3

#
# Period
#
export SDATE=${1:-2024042000}
export EDATE=${2:-2024042100}

Start=`date +%s.%N`
echo $Start > $BASEDIR/example3/logs/Timing2

while [ ${SDATE} -le ${EDATE} ];
do

   dirin=/mnt/beegfs/monan/i610-MONAN-interpolacao_corrigida/scripts_CD-CT/dataout/${SDATE}/Post
   dirtmp=/mnt/beegfs/$USER/DADOS/MONAN/nc/${SDATE}
   dirout=/mnt/beegfs/$USER/DADOS/MONAN/GRIB2/${SDATE}

   #ls  -ltr $dirin
   ls -1 $dirin | wc -l

if [ ! -e ${dirtmp} ]; then
   mkdir -p $dirtmp
fi
if [ ! -e ${dirout} ]; then
   mkdir -p $dirout
fi

for i in {0..240..3}  ;  do

   export FCST=`printf "%03i" ${i}`
   echo ">"$FCST

   #file='MONAN_DIAG_G_POS_GFS_'${SDATE}'.00.00.x1024002L55.nc'
   file='monan.t00z.atm.0p25.f'${FCST}'.'${SDATE}'.nc'
   filein=$dirin/$file
   fileout=$dirout'/MONAN_DIAG_G_POS_GFS_%Y4%M2%D2%H2_%y4%m2%d2%h2.x1024002L55'
   ls -ltr $filein

   # Options
   #---------------------------------------------------
   # -p 0 grid_simple
   # -p 1 grid_ccsd (Adaptive Entropy Coding library)
   # -p 2 grid_jpeg
   # -p 3 grid_png
   #------------------------------------------------------

   #mpas_nc2grib2.x -i $filetmp -o $fileout.ccsds -s $yy0$mm0$dd0$hh0 -f $fff -v 3 -p 1
   #$NC2GRIB_DIR/bin/mpas_nc2grib2.x -i $filetmp -o $fileout -s $yy0$mm0$dd0$hh0 -f $fff -v 0 -p 0
   #ldd $NC2GRIB_DIR/bin/mpas_nc2grib2.x

time $NC2GRIB_DIR/bin/mpas_nc2grib2.x -p 0 -i ${filein} -o ${fileout} -s ${SDATE} -f ${FCST} -v 3 -c nc2grib.2_v2.csv &

done

exit 0
