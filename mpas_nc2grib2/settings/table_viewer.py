#!/bin/python3

#import datetime as dt
import os
#from get_date import *

# Default parameter
discipline=0
category=0
version='28'
definition_path= os.environ["ECCODES_DEFINITION_PATH"]+"/grib2/tables/"+version

print ("-----------------------------------------------------------")
print (" GRIB2 TABLE VIEWER ")
print ("-----------------------------------------------------------")
print ("Version=",version)
print ("Definition Path=",definition_path)
print ("-----------------------------------------------------------")
print ('Find the parameter number by product discipline and parameter category ')
op=input("Enter Y to continue:")

if (op.upper()=='Y')  :
    filename=definition_path+"/0.0.table"
    file = open(filename, "r")
    content = file.read()
    print ("-----------------------------------------------------------")
    print(content)
    file.close()
    dis=input("Enter the discipline number :")

    filename=definition_path+"/4.1."+dis+".table"
    file = open(filename, "r")
    content = file.read()
    print ("-----------------------------------------------------------")
    print(content)
    file.close()

    cat=input("Enter the category number :")

    filename=definition_path+"/4.2."+dis+"."+cat+".table"
    print ("table=",filename)
    file = open(filename, "r")
    content = file.read()
    print ("-----------------------------------------------------------")
    print(content)
    file.close()
