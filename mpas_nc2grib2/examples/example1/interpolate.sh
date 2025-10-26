#!/bin/bash
#Reference <http://urlib.net/8JMKD3MGP3W34T/4BMPD2L> pag.27
#
#"..describes creating eights to interpolate MPAS\u2019s data to a regular grid conservatively. 
# For example, to create the weights to interpolate the MPAS-A 15km outputs to a 0.25 x 0.25 grid:"

#Qcdo -P 1 --cellsearchmethod spherepart -gencon,r1440x720 -setgrid,mpas:x1.2621442.grid.nc -selgrid,1 diag.2021-06-23_00.00.00.nc weights_.25.nc

cdo -P 1 --cellsearchmethod spherepart -gencon,r720x360 -setgrid,mpas:x1024002L55.grid.nc -selgrid,1 ./datain/MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.nc weights_.25.nc


#Interpolating a variable from MPAS-A 15km to 0.25 x 0.25 grid:
#cdo -P 1 -f nc4 remap,r1440x720,weights_.25.nc -setgrid,mpas:x1.2621442.grid.nc -selname,olrtoa diag.2021-07-08_00.00.00.nc olr.p25.nc


#
# cdo sinfo (get nc information) 
#MONAN_DIAG_G_POS_GFS_2024070900_2024070900.x1024002L55.nc

#  1 : lonlat                   : points=259200 (720x360)
#                        longitude : 0 to 360 by 0.5006954 degree_east
#                         latitude : -90 to 90.00002 by 0.5013928 degree_north
#   Vertical coordinates :
#     1 : pressure                 : levels=22
#                            level : 1000 to 15 hPa
#     2 : surface                  : levels=1
#   Time coordinate :
#                             time : 1 step
