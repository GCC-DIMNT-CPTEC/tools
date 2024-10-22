#!/bin/bash
# this script is just to show the problem to convert nc to grib2 using cdo

cdo -f grb2 copy ./datain/MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.nc ./datain/out.grb
