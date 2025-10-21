#!/usr/bin/python3
import datetime as dt
import os
import sys
import glob
from get_date import *

# Script de conversao para pos-processamento 1.2.0
#module load netcdf-fortran
#module load cdo-2.0.4-gcc-9.4.0-bjulvnd
export NC2GRIB_DIR=../..
NFDIR='/usr'

localdir0='/dados5/modelos/global/MONAN/dataout/'
destination0='/dados5/modelos/global/MONAN/dataout/'
resolution='x1024002L55'
conf_table_name='nc2grib_v1.4.1-rc.xml'

#--------------------------
# GET MONAN DATA FROM EGEON 
# -------------------------

def get_monan_egeon(d) :
	print("getting data from egeon : ",ref_date)
	yy=d.strftime("%Y")
	mm=d.strftime("%m")
	dd=d.strftime("%d")
	hh="00"
	remotedir="/oper/dados/ioper/tempo/CRNG/MONAN/"+yy+"/"+mm+"/"+dd+"/"+hh
	localdir=localdir0+"tmp/"+yy+mm+"/"+dd+"/"+hh
	os.makedirs(localdir, exist_ok=True)
	print ("from:",remotedir)
	print ("to:",localdir)
	cmd="sshpass -p sbgl83746 rsync sergio.ferreira@egeon.cptec.inpe.br:/"+remotedir+"/* "+localdir
	os.system(cmd)
#---------------------------
# CONVERT NC TO GRIB2
#--------------------------	
def convert2grib2(d,ff) :
	ok=0
	fff=str(ff).zfill(3)
	d0,d2=get_date(d,ff)
	dx0=date2str(d0)
	dx2=date2str(d2)
	yy=d0.strftime("%Y")
	mm=d0.strftime("%m")
	dd=d0.strftime("%d")
	hh='00'
	print("converting data fron netcdf to grib2")
	localdir=localdir0+"tmp/"+yy+mm+"/"+dd+"/"+hh
	file='MONAN_DIAG_G_POS_GFS_'+dx0+'_'+dx2+".00.00."+resolution+".nc"
	grib2='MONAN_DIAG_G_POS_GFS_'+dx0+'_'+dx2+".00.00."+resolution
	destination=destination0+"grib2/"+yy+mm+"/"+dd
	os.makedirs(destination, exist_ok=True)
	filein=localdir+"/"+file
	fileout=destination+"/"+grib2
	
	if os.path.exists(filein):
		if os.path.exists(fileout+".grib2") :
			print(fileout+".grib2:  Ok \n")
			ok=1
		else:
			f = open(".run_mpas_nc2grib.sh", "w")
			f.write("#!/bin/bash\n")
			f.write("export NC2GRIB_DIR="+NC2GRIB_DIR+"\n")
			f.write("$NC2GRIB_DIR/bin/mpas_nc2grib2.x -c "+conf_table_name+" -i "+filein+" -o "+fileout+" -s "+dx0+" -f "+fff+" -v 3\n")
			f.write("chgrp gan "+fileout+"\n")
			f.close()
			cmd="chmod 755 .run_mpas_nc2grib.sh"
			os.system(cmd)
			cmd='./.run_mpas_nc2grib.sh'
			os.system(cmd)
	else:
		print(filein," doesn't exist" )
	return ok
#-----------------------------
# CONVERT NC TO NC
#------------------------------
def convert2track(d,ff) :
	ok=0
	fff=str(ff).zfill(3)
	d0,d2=get_date(d,ff)
	dx0=date2str(d0)
	dx2=date2str(d2)
	yy=d0.strftime("%Y")
	mm=d0.strftime("%m")
	dd=d0.strftime("%d")
	hh='00'
	print("converting data fron netcdf to netcdf")
	localdir=localdir0+"tmp/"+yy+mm+"/"+dd+"/"+hh
	file1='MONAN_DIAG_G_POS_GFS_'+dx0+'_'+dx2+".00.00."+resolution+".nc"
	file2="MONAN_DIAG_G_POS_GFS_"+dx0+"_"+dx2+".00.00."+resolution+".uv.nc"
	destination=destination0+"netcdf/"+yy+mm+"/"+dd
	os.makedirs(destination, exist_ok=True)
	filein=localdir+"/"+file1
	fileout=destination+"/"+file2
	
	if os.path.exists(filein):
		if os.path.exists(fileout) :
			print(fileout+" Ok \n")
			ok=1
		else:
			cmd="cdo -sellevel,85000 -selname,umeridional,uzonal "+filein+" "+fileout
			os.system(cmd)
			cmd="chgrp gan "+fileout
			os.system(cmd)
	else:
		print(filein," doesn't exist" )

	return ok
#-----------------------------
# REMOVE TMP FILE
#------------------------------
def remove_tmp_file(d):
	d0,d2=get_date(d,0)
	dx0=date2str(d0)
	dx2=date2str(d2)
	yy=d0.strftime("%Y")
	mm=d0.strftime("%m")
	dd=d0.strftime("%d")
	hh='00'
	print("remove tmp files\n")
	localdir="/dados5/modelos/global/MONAN/dataout/tmp/"+yy+mm+"/"+dd+"/"+hh
	file1='MONAN_DIAG_G_POS_GFS_'+dx0+'_'+dx2+".00.00.x1024002L18.nc"
	filein=localdir+"/"+file1
	if os.path.exists(filein):
		os.remove(filein) 
		

#-----------------
# Modulo principal
#------------------

#-------------
# Incializacao
#---------------
NC2GRIB_DIR='/home/sergio.ferreira/PRO/git/GCC-DIMNT-CPTEC/branch/tools/mpas_nc2grib2'
NFDIR='/usr'
conf_table_name='nc2grib_v1.4.1-rc.xml'


#-----------------
# Well come 
#-----------------
now = dt.datetime.now()
d0,d2=get_date(now,0)

args=sys.argv
nargs=len(args)
if ( nargs < 2) :
	print("Forneca pelo menos 1 argumento\n")
	print("  run ")
	print("  before <nhours> " ) 
	print("  test" ) 
	print("  <yyyymmddhh>" ) 
	exit()

run=args[1]
if (run=="yesterday") : 
	nhours=24
	ref_date = d0 - dt.timedelta(hours=nhours)
elif (run=="before" and nargs>1):
	nhours=int(args[2])
	ref_date = d0 - dt.timedelta(hours=nhours)
elif (len(run)==10) :
	ref_date=datetime.strptime(run,'%Y%m%d%H') 
elif (run=="run") :
	ref_date=d0
elif (run=="test") :
	run='2025072700'
	ref_date=datetime.strptime(run,'%Y%m%d%H')	
else :
	exit()	
print ("date=",d0)

#--------------
# main program
#---------------
hx=8*24
get_monan_egeon(ref_date)
for ff in range (0,hx,3):
	ok1=convert2grib2(ref_date,ff)
	ok2=convert2track(ref_date,ff)
ok=ok1*ok2
#if (ok==1) : 
#	remove_tmp_file(ref_date)


