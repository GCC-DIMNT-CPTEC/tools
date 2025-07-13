#!/bin/python3

#import datetime as dt
import os
#from get_date import *

# Default parameter
discipline=0
category=0
version='35'
#version='4'
definition_path= os.environ["ECCODES_DEFINITION_PATH"]+"/grib2/tables/"+version

print ("-----------------------------------------------------------")
print (" GRIB2 TABLE VIEWER ")
print ("-----------------------------------------------------------")
print ("Version=",version)
print ("Definition Path=",definition_path)
print ("-----------------------------------------------------------")
print ("4.2 - Parameter number by product discipline and parameter category")
print ("4.5 - Fixed surface types and units")

op=float(input("Enter option to continue:"))

if (op==4.2)  :
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
    par=input("Enter the parameter number :")

    print ("-----------------------------------------------------------")
    print ("Results:")
    print ("Discipline..:",dis)
    print ("Category....:",cat)
    print ("Parameter...:",par)

if (op==4.5) :
    filename=definition_path+"/4.5.table"
    print ("-----------------------------------------------------------")
    print ("table=",filename)
    print ("-----------------------------------------------------------")
    file = open(filename, "r")
    content = file.read()
    print(content)
    file.close()

