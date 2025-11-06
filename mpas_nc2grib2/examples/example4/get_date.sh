#!/bin/bash
#

ee=$1
ff=$2
ii=$3
FFF=006
IC=06
if [ ${#ee} -eq 0 ] ; then
  echo "-----------------------------------------------------------------------"
  echo "| get_date : Return synoptic date and time                            |"
  echo "|---------------------------------------------------------------------|"
  echo "| source get_date.sh <parameter>"
  echo "|   <parameter> : yyyymmddhh"
  echo "|                 yyyymmddhhFFF"
  echo "|                 now {FFF} {IC}"
  echo "|                 run {FFF} {IC}"
  echo "|                 yesterday {FFF} {IC}"
  echo "|                 before dd {FFF} {IC}"
  echo "|" 
  echo "|" 
  echo "|  where: yyyymmddhh = Year,month,day e hour of analise"
  echo "|        FFF        = Forecast time  (006,012,048,etc) Default = 006"
  echo "|        now        = Run with present date"
  echo "|        run        = Run with present date - 6 h"
  echo "|        yesterday  = Run with present date - 24 h"
  echo "|        before dd  = Dated dd days ago"
  echo "|        IC         = Initial Condition interval (06,12) Default = 06"
  echo "-----------------------------------------------------------------------"
  echo " results provide in time1, time2, time3 ( synoptic, analisis, forecast)"
  echo " time= \$yy \$mm \$dd \$hh "
  echo "-----------------------------------------------------------------------"
  exit
fi 
if [ "$ee" == "before" ] ; then
	dt=$2
	yy0=`date +%Y --date "$dt day ago"`  
	mm0=`date +%m --date "$dt day ago"`  
	dd0=`date +%d --date "$dt day ago"`  
	hh0=`date +%H --date "$dt day ago"` 
	mt="1"
	ff=$3
        ii=$4
fi
if [ ${#ee} -eq 10 ] ; then
	yy0=${ee:0:4}
	mm0=${ee:4:2}
	dd0=${ee:6:2}
	hh0=${ee:8:2}
	mt="0"
fi
 
if [ ${#ee} -eq 13 ] ; then
	yy0=${ee:0:4}
	mm0=${ee:4:2}
	dd0=${ee:6:2}
	hh0=${ee:8:2}
	FFF=${ee:10:13}
	mt="0"
fi
 
if [ ${#ff} -eq 3 ] ; then
	FFF=$ff
fi
if [ ${#ii} -eq 2 ] ; then
	IC=$ii
fi

if [ "$ee" == "now" ] ; then 
	yy0=`date +%Y `  # Usar a Data do sistema
	mm0=`date +%m `  # Usar a Data do sistema
	dd0=`date +%d `  # Usar a Data do sistema
	hh0=`date +%H ` 
	mt="1"
fi

if [ "$ee" == "run" ] ; then 
	yy0=`date +%Y --date "6 hour ago"`  # Usar a Data do sistema
	mm0=`date +%m --date "6 hour ago"`  # Usar a Data do sistema
	dd0=`date +%d --date "6 hour ago"`  # Usar a Data do sistema
	hh0=`date +%H --date "6 hour ago"` 
	mt="1"
fi

if [ "$ee" == "yesterday" ] ; then 
	yy0=`date +%Y --date "24 hour ago"`  # Usar a Data do sistema
	mm0=`date +%m --date "24 hour ago"`  # Usar a Data do sistema
	dd0=`date +%d --date "24 hour ago"`  # Usar a Data do sistema
	hh0=`date +%H --date "24 hour ago"` 
	mt="1"
fi


#echo 'hh_in='$hh0

#------------------------------------------------------
# Convertendo hora para horario sinotico (00,06,12,18) 
# -----------------------------------------------------
declare -i c d
c=(10#$hh0/6)
d=(c*6)
hh0=$d
if [ ${#hh0} !=  2 ]; then
	hh0='0'$hh0
fi
echo "Data e hora sinotica > $yy0$mm0$dd0$hh0" 

#------------------------------------------------
# Data da analise 
#-----------------------------------------------
yy=$yy0
mm=$mm0
dd=$dd0
c=(hh0/$IC)
d=(c*$IC)
hh=$d
if [ ${#hh} !=  2 ]; then
	hh='0'$hh
fi

#------------------------------------------------
# Data da previsao
#-----------------------------------------------
declare -i FFR
FFR=(10#$FFF)  # (10#) Anula operacao em base octal processa em decimal
     export yy2=`date +%Y --date="$yy-$mm-$dd $hh +$FFR hour"`
     export mm2=`date +%m --date="$yy-$mm-$dd $hh +$FFR hour"`
     export mmm2=`date +%h --date="$yy-$mm-$dd $hh +$FFR hour"`
     export dd2=`date +%d --date="$yy-$mm-$dd $hh +$FFR hour"`
     export hh2=`date +%H --date="$yy-$mm-$dd $hh +$FFR hour"`
     echo "tempo de previsao....................:[$FFR->$FFF]"
     echo "Data e hora sinotica.................:$yy0$mm0$dd0$hh0"
     echo "Data e hora da analise mais proxima..:$yy$mm$dd$hh"
     echo "Data e hora da previsao..............:$yy2$mm2$dd2$hh2 ($dd2-$mmm2-$yy2 $hh2:00)"
     

