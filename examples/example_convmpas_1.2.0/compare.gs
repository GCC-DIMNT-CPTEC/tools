function main(args)
'reinit'
'open dataout/MONAN_DIAG_G_POS_GFS_2026082800_2026082800.x1024002L55.ctl'
'open dataout/MONAN_DIAG_G_POS_GFS_2026082800_2026082800.x1024002L55.32.ctl'

*{ Regiao e cores
'set mpdset mres'
'set rgb 50 090 000 100'
'set rgb 51 071 055 162'
'set rgb 52 053 078 232'
'set rgb 53 053 128 253'
'set rgb 54 071 199 203'
'set rgb 85 170 240 241'
'set rgb 86 188 252 195'
'set rgb 55 114 252 131'
'set rgb 56 183 235 080'
'set rgb 57 245 166 056'
'set rgb 58 245 102 051'
'set rgb 59 183 065 064'
'set rgb 60 255 255 255'
'set rgb 61 220 220 220'
'set rgb 62 200 200 200'
'set rgb 63 150 150 150'
'set rgb 64 100 100 100'
'set rgb 65 75  75  75'
'set rgb 66 50  50  50'
'set rgb 67 25  25  25'
'set rgb 68  1   1   1'

* Degrade amarelo
'set rgb 70  255  255 1'
'set rgb 71  220  220 1'
'set rgb 72  200  200 1'
'set rgb 73  150  150 1'
'set rgb 74  100  100 1'
'set rgb 75  75   75  1'
'set rgb 76   50  50  1'
'set rgb 77   25  25  1'
'set rgb 78    1   1  1'
'set lat -70 30'
'set lon -160 10'

*Azul
'set mpdset mres'
'set rgb 30 51 255 255'
'set rgb 31 51 209 236'
'set rgb 32 51 185 227'
'set rgb 33 51 162 218'
'set rgb 34 51 116 199'
'set rgb 35 51 70 181'
'set rgb 36 51 23 162'
'set rgb 37 51 0 153'

*Vermelho

'set rgb 70 255 250 170'
'set rgb 71 255 232 120'
'set rgb 72 255 192 90'
'set rgb 73 255 192 30'
'set rgb 74 255 160 0'
'set rgb 75 255 120 0'
'set rgb 76 255 96 0'
'set rgb 77 255 50 0'
'set rgb 78 225 20 0'
'set rgb 79 192 0 0'
'set rgb 90 165 0 0'

*'set mproj robinson'
*'set map 0'
'set xsize 1200 1200'
'set display color white'

nrows=3
ncols=3
panels(' 'nrows' 'ncols'')
*vari='hgtprs'
vari='capesfc'


******
* DSD (cfa)
******

_vpg.1
'set gxout shaded'
'd 'vari
'draw title 'vari



_vpg.2

'd 'vari'.2'
'draw title 'vari'.2'


_vpg.3
'd 'vari'-'vari'.2'
'draw title 'vari'-'vari'.2'


return

* panels.gsf
* 
* This function evenly divides the real page into a given number of rows
* and columns then creates global variables that contain the 'set vpage' 
* commands for each panel in the multi-panel plot. 
*
* Usage: panels(rows cols)
*
* Written by JMA March 2001
*
function panels(args)

* Get arguments
  if (args='') 
    say 'panels requires two arguments: the # of rows and # of columns'
    return 
  else 
    nrows = subwrd(args,1)
    ncols = subwrd(args,2)
  endif

* Get dimensions of the real page
  'query gxinfo'
  rec2  = sublin(result,2)
  xsize = subwrd(rec2,4)
  ysize = subwrd(rec2,6)

* Calculate coordinates of each vpage
  width  = xsize/ncols
  height = ysize/nrows
  row = 1
  col = 1
  panel = 1
  while (row <= nrows)
    yhi = ysize - (height * (row - 1))
    if (row = nrows)
      ylo = 0
    else
      ylo = yhi - height
    endif
    while (col <= ncols)  
      xlo = width * (col - 1)
      xhi = xlo + width
      _vpg.panel = 'set vpage 'xlo'  'xhi'  'ylo'  'yhi
      panel = panel + 1
      col = col + 1
    endwhile
    col = 1
    row = row + 1
  endwhile
  return

* THE END *


