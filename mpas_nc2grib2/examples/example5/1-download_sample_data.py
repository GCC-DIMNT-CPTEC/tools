#!/usr/bin/python3
import datetime as dt
import os
import sys
import glob
from get_date import *

#------------------------------------------------------------------------------
#  Download function
#------------------------------------------------------------------------------
#     dd: Reference date
#     ff: forecast time
#------------------------------------------------------------------------------
def Download(d,ff) :
	ok=0
	fff=str(ff).zfill(3)
	d0,d2=get_date(d,ff)
	dx0=date2str(d0)
	dx2=date2str(d2)
	yy=d0.strftime("%Y")
	mm=d0.strftime("%m")
	dd=d0.strftime("%d")
	hh='00'

	remote=remote0+"/"+yy+mm+dd+hh
	destination="./datain/"+yy+mm+dd+hh
	os.makedirs(destination, exist_ok=True)
	file='MONAN_DIAG_R_POS_GFS_'+dx0+'_'+dx2+".00.00."+resolution+".nc"
	cmd="wget -nc "+remote+"/"+file+" -P "+destination
	print(cmd)
	os.system(cmd)

# -------------------------------------------------------------------------------
#  Main program
#--------------------------------------------------------------------------------
resolution='x1.5898242L55'
remote0="https://ftp.cptec.inpe.br/pesquisa/bam/paulo.kubota/externo/Curso_da_OMM_2025_estudos_de_casos/Central_America_Hurricane_Julia/"
run='2022100800'
ref_date=datetime.strptime(run,'%Y%m%d%H')
#hx=8*24
hx=6
for ff in range (0,hx,3):
	ok1=Download(ref_date,ff)



#---------------------------
# Notes
#--------------------------
#wget -r -nH --cut-dirs=6 -P $local $remote  -R “index.html*”
#     |   |            |     |
#     |   |            |     +-Local Path
#     |   |            +-------how many levels you want to remove
#     |   +--------------------avoid creating a host-prefixed directory
#     +------------------------Recursive
