#!/usr/bin/python3
import datetime as dt
from datetime import datetime
 
# function
def get_date(dd,fff):
	yy0=dd.year
	mm0=dd.month
	dd0=dd.day
	hh0=dd.hour
	ic=6
	#hh0=int(hh0/ic)*6
	hh0=00
	d0=dt.datetime(yy0,mm0,dd0,hh0)
	ts=datetime.timestamp(d0)+(fff*60*60)
	d2=datetime.fromtimestamp(ts)
	if (fff > 0 ) :
		return d0,d2
	else:
		return d2,d0	

def date2str(d0):
	yy=d0.strftime("%Y")
	mm=d0.strftime("%m.")
	mm=mm[:2]
	dd=d0.strftime("%d")
	hh=d0.strftime("%H")
	dx=yy+mm+dd+hh	
	return dx
