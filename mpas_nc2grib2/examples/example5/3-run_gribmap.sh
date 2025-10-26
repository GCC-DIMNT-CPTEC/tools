#!/bin/bash -x
gribmap='/home/sergio.ferreira/GRADS/grads_2.0.a9/bin/gribmap'
gribmap='gribmap'
hh=00

ymd=20221008
cd ./dataout/$ymd
pwd -P
g2ctl -verf MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.grib2 > MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.ctl
$gribmap -i MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.ctl

ymd=20230308
cd ../$ymd
pwd -P
g2ctl -verf MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.grib2 > MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.ctl
$gribmap -i MONAN_DIAG_R_POS_GFS_${ymd}00_${ymd}00.00.00.x1.5898242L55.ctl

