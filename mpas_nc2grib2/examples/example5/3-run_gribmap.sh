#!/bin/bash -x
a=`hostname`

if [[ $a == *"saveiro"* ]]; then
	gribmap='/home/sergio.ferreira/GRADS/grads_2.0.a9/bin/gribmap'
else
	gribmap='gribmap'
fi

hh=00


ym=202210
ymd=20221008
cd ./dataout/$ymd
pwd -P
for dd in 08 09 ; do
g2ctl -verf MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.grib2 > MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.ctl
$gribmap -i MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.ctl
done


ymd=20230308
ym=202303
cd ../$ymd
pwd -P
for dd in 08 09 ; do
g2ctl -verf MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.grib2 > MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.ctl
$gribmap -i MONAN_DIAG_R_POS_GFS_${ymd}00_${ym}${dd}00.00.00.x1.5898242L55.ctl
done
