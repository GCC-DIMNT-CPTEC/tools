#!/usr/bin/python3
import datetime as dt
import os
import sys
import glob
import socket
from get_date import *


# Script de conversao para pos-processamento 1.2.0
#module load netcdf-fortran
#module load cdo-2.0.4-gcc-9.4.0-bjulvnd
hostname = socket.gethostname()
#export NC2GRIB_DIR=../..
#NFDIR='/usr'



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
	localdir=localdir0+"/"+yy+mm+dd+hh
	
	file='MONAN_DIAG_R_POS_GFS_'+dx0+'_'+dx2+".00.00."+resolution


	destination=destination0+"/"+yy+mm+dd
	os.makedirs(destination, exist_ok=True)
	filein=localdir+"/"+file+".nc"
	fileout=destination+"/"+file
	
	if os.path.exists(filein):
		if os.path.exists(fileout+".grib2") :
			print(fileout+".grib2:  Ok \n")
			ok=1
		else:
			if  (hostname.find("egeon")>=0) :
				ok=submit_egeon(filein,fileout,dx0,fff)
			else :
				ok=submit_generic(filein,fileout,dx0,fff)
	else:
		print(filein," doesn't exist" )
	return ok
#-----------------------------------------------------------------------
# Write and submit script to run mpas_nc2grib2 in egeon
#-----------------------------------------------------------------------
def submit_egeon(filein,fileout,dx0,fff) :
	sname='.run_mpas_nc2grib.slurn'
	f = open(sname, "w")
	f.write("#!/bin/bash -l\n")
	f.write("#SBATCH -J GRIB2\n")
	f.write("#SBATCH --nodes=1 \n")            # node count
	f.write("#SBATCH --ntasks-per-node=64\n")  #64 ou 128
	f.write("#SBATCH -t 01:00:00\n")
	f.write("#SBATCH -p batch\n")              # proc , batch (use sinfo para ver quais filas estao disponiveis)
	f.write("module load cdo-2.0.4-gcc-9.4.0-bjulvnd\n")
	f.write("module load netcdf-fortran\n")	
	f.write("export NC2GRIB_DIR='../..'\n")
	f.write("export NFDIR='/opt/ohpc/pub/libs/gnu9/openmpi4/netcdf-fortran/4.5.3'\n")
	f.write("$NC2GRIB_DIR/bin/mpas_nc2grib2.x -c "+conf_table_name+" -i "+filein+" -o "+fileout+" -s "+dx0+" -f "+fff+" -v 3\n")
	f.close()
	cmd="chmod 755 "+sname
	os.system(cmd)

	LOGFILE='./nc2grib2.log'
	cmd="sbatch -o "+LOGFILE+" "+sname
	os.system(cmd)
	ok=1
	return ok
#-----------------------------------------------------------------------
# Write and submit script to run mpas_nc2grib2 in generic
#-----------------------------------------------------------------------
def submit_generic(filein,fileout,dx0,fff) :
	f = open(".run_mpas_nc2grib.sh", "w")
	f.write("#!/bin/bash\n")
	f.write("export NC2GRIB_DIR='../..'\n")
	f.write("export NFDIR='/usr'\n")
	f.write("$NC2GRIB_DIR/bin/mpas_nc2grib2.x -c "+conf_table_name+" -i "+filein+" -o "+fileout+" -s "+dx0+" -f "+fff+" -v 3\n")
	f.close()
	cmd="chmod 755 .run_mpas_nc2grib.sh"
	os.system(cmd)
	cmd='./.run_mpas_nc2grib.sh'
	os.system(cmd)
	ok=1
	return ok

#-----------------
# Modulo principal
#------------------

#-------------
# Incializacao
#---------------
localdir0='./datain'
destination0='./dataout'
resolution='x1.5898242L55'
conf_table_name='nc2grib_v1.4.1-rc.xml'

#
# Julia
#
reference_date='2022100800'
ref_date=datetime.strptime(reference_date,'%Y%m%d%H')	
hx=48
for ff in range (0,hx,24):
	ok1=convert2grib2(ref_date,ff)

#
#  Galapagos
#
reference_date='2023030800'
ref_date=datetime.strptime(reference_date,'%Y%m%d%H')
hx=48
for ff in range (0,hx,24):
	ok1=convert2grib2(ref_date,ff)
