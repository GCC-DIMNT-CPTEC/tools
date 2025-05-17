program test

   use mgrib_tables, only :init_parm2
   character (len=255):: conftable

   conftable="nc2grib.2.xml"
   call init_parm2 (conftable)
end program
