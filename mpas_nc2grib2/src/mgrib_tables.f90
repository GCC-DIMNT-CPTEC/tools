module mgrib_tables
!--------------------------
! nc-grib2 code matching
!--------------------------
 use mgrib_interface
 use stringflib
 use xmlparse
 implicit none
 public
 integer                                   ::nvar
 integer,parameter                         ::nvarmax=10000
 type(grib_parameter_id),dimension(nvarmax)::var
 integer                                   ::psup_id ! Surface pressure index (for grib1 only) to indicate that this values must
                                                  ! be convert from hPa do Pa
 integer::tablesVersion_default
 character(len=1024)::eccodes_dir
 character(len=1024)::nc2grib_dir
 contains

 subroutine init_parm(parm_table)
   character(len=*),intent(in)::parm_table

   character(len=1024)::table_name
   integer::i
   character(len=256)::line
   character(len=32),dimension(1:20)::values
   integer::nv
   integer::p
   i=0
   call getenv("NC2GRIB_DIR",nc2grib_dir)
   if (len_trim(nc2grib_dir)==0) then 
     print *,"Error! NC2GRIB_DIR environment variable not found"
     print *,"       please set NC2GRIB_DIR with PATH where MPAS_NC2GRIB is"
     stop
   end if 
   

   table_name=trim(nc2grib_dir)//"/settings/"//trim(parm_table)
   print *,"table_name=",trim(table_name)
   open(14,file=table_name,status='old')
      read(14,'(a)',end=1919) line
19    read(14,'(a)',end=1919) line
      p=index(line,"#")
      if (p==1) line="" 

!   Sequence:      
!      1        2        3         4           5       6                7     8          9         10            11
!   NC_Name,cfVarName,Templete, Discipline,Category,ParameterNumber,tflevel,sValueFFS,sFactorFFS,Time_interval,Varname,

      
      if (p>1)line=line(1:p)
       call split(line,",",values,nv)
       if (nv==11) then
          i=i+1
          var(i)%Template=val(values(3))
          var(i)%Discipline=val(values(4))
          var(i)%pCat=val(values(5))
          var(i)%pNum=val(values(6))
          var(i)%tflevel=val(values(7))
          var(i)%cfVarName=values(2)
          var(i)%ncVarName=values(1)
          var(i)%VarName=values(11)
          var(i)%sFactor_FFS=val(values(9)) !scaleFactorOfFirstFixedSurface
          var(i)%sValue_FFS=val(values(8)) !scaleValuesOfFirstFixedSurface
	      var(i)%time_interval=val(values(10)) ! Time interval
          nvar=i
       end if
       goto 19
1919   continue

   close(14)
end subroutine

subroutine init_parm2(parm_table)
   character(len=*),intent(in)::parm_table

   character(len=1024)::table_name,fname
   integer::i,j
   character(len=256)::line
   character(len=32),dimension(1:20)::values
   integer::nv
   integer::p
   logical          :: mustread,op
   type(XML_PARSE)  :: info
   character(len=80):: tag
   logical          :: endtag
   character(len=80),dimension(1:2,1:20) :: attribs
   integer          :: no_attribs
   character(len=200),dimension(1:100):: data
   integer         :: no_data


   i=0
   op=.false.
   tablesVersion_default=4
   call getenv("NC2GRIB_DIR",nc2grib_dir)
   if (len_trim(nc2grib_dir)==0) then
     print *,"Error! NC2GRIB_DIR environment variable not found"
     print *,"       please set NC2GRIB_DIR with PATH where MPAS_NC2GRIB is"
     stop
   end if


  table_name=trim(nc2grib_dir)//"/settings/"//trim(parm_table)
   print *,"table_name=",trim(table_name)
 
 ! Assign the xml-filename to fname and open the file
 mustread = .true.

 call xml_open(info,table_name,mustread)
 ! Check for errors
 if (xml_error(info)) then
   print *,"Error ",info," ",fname
   stop
 else
   !Start reading the file
   call xml_options(info,ignore_whitespace = .true.)

   do  ! While xml_ok
      call xml_get(info,tag,endtag,attribs,no_attribs,data,no_data)
      if (xml_error(info)) then
         print *,"!handel errors"
         exit
      endif
    
      if (tag=="settings") then 
        if (trim(attribs(1,1))=="tablesVersion_default") then
	   tablesVersion_default=val(attribs(2,1))
	   print *,"Settings: tablesVersion_Default=",tablesVersion_Default
        end if 
      end if 

      if ((tag=="element")) then
         if (.not.endtag) then
            i=i+1
            do j=1,no_attribs
               !print *," Element [",i,"]",trim(attribs(1,j)),'<=',trim(attribs(2,j))
                if (trim(attribs(1,j))=="NCVar") var(i)%ncVarName=trim(attribs(2,j))
                if (trim(attribs(1,j))=="cfVarName") var(i)%cfVarName=trim(attribs(2,j))
                if (trim(attribs(1,j))=="Name") var(i)%VarName=trim(attribs(2,j))
            enddo
            write(*,'("var(",i3.3,") -> ",3(" [",A,"]"))')i,trim(var(i)%ncVarName),trim(var(i)%cfVarname),trim(var(i)%Varname)
            nvar=i
         end if
      end if


      if ((tag=="template")) then
        if (.not.endtag) then
            do j=1,no_attribs
                if(trim(attribs(1,j))=="def") var(i)%Template=val(attribs(2,1))
            end do
            op=.true.
        else
            op=.false.
        end if
      end if


      if ((var(i)%template==0).and.(op)) then
        do j=1,no_attribs
          ! write(*,*) i,"< template 4.0 >",trim(attribs(1,j)),'<=',trim(attribs(2,j))
          if (trim(attribs(1,j))=="discipline") var(i)%discipline=val(attribs(2,j))
          if (trim(attribs(1,j))=="parameterCategory") var(i)%pCat=val(attribs(2,j))
          if (trim(attribs(1,j))=="parameterNumber") var(i)%pNum=val(attribs(2,j))
          if (trim(attribs(1,j))=="typeOfFirstFixedSurface") var(i)%tflevel=val(attribs(2,j))
          if (trim(attribs(1,j))=="scaleFactorOfFirstFixedSurface") var(i)%sFactor_FFS=val(attribs(2,j))
          if (trim(attribs(1,j))=="scaledValueOfFirstFixedSurface") var(i)%sValue_FFS=val(attribs(2,j))
          nvar=i
        enddo
         var(i)%time_interval=0
      elseif((var(i)%template==8).and.(op)) then
         do j=1,no_attribs
           !  write(*,*) i,"< template 4.8 >",trim(attribs(1,j)),'<=',trim(attribs(2,j))
             if (trim(attribs(1,j))=="discipline") var(i)%discipline=val(attribs(2,j))
             if (trim(attribs(1,j))=="parameterCategory") var(i)%pCat=val(attribs(2,j))
             if (trim(attribs(1,j))=="parameterNumber") var(i)%pNum=val(attribs(2,j))
             if (trim(attribs(1,j))=="typeOfFirstFixedSurface") var(i)%tflevel=val(attribs(2,j))
             if (trim(attribs(1,j))=="scaleFactorOfFirstFixedSurface") var(i)%sFactor_FFS=val(attribs(2,j))
             if (trim(attribs(1,j))=="scaledValueOfFirstFixedSurface") var(i)%sValue_FFS=val(attribs(2,j))
             if (trim(attribs(1,j))=="timeint") var(i)%time_interval=val(attribs(2,j))
             nvar=i
        enddo

      end if
     !write(*,*) (j,'>',trim(data(j)),'<',j=1,no_data)

     if (.not. xml_ok(info)) exit
    enddo !
  endif
  call xml_close(info)
  end subroutine


 !--------------------------------------------------------
 ! Creates the fist of variable to be extracted from grib
 !---------------------------------------------------------
 subroutine init_parm_grib2(humidity_parm)
    integer,optional,intent(in)::humidity_parm
    integer::i,hp

    i=0
    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=3
    var(i)%pNum=1
    var(i)%tflevel=101
    var(i)%cfVarName="prmsl"
    var(i)%VarName="Pressure reduced to MSL  (Pa)  (grib2/tables/2/4.2.0.3.table)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=3
    var(i)%pNum=5
    var(i)%tflevel=1
    var(i)%cfVarName="topo"
    var(i)%VarName="Surface Geopotential height  (gpm)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=3
    var(i)%pNum=0
    var(i)%tflevel=1
    var(i)%cfVarName="sp"
    var(i)%VarName="Surface pressure (PA)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=3
    var(i)%pNum=5
    var(i)%tflevel=100      !Isobaric surface  (Pa)  (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)
    var(i)%cfVarName="gh"
    var(i)%VarName="Geopotential height  (gpm)  (grib2/tables/2/4.2.0.3.table)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=0
    var(i)%pNum=0
    var(i)%tflevel=100
    var(i)%cfVarName="t"
    var(i)%VarName="Temperature  (K)  (grib2/tables/2/4.2.0.0.table)"

    if (present (humidity_parm)) then
        hp=humidity_parm
    else
        hp=-1
    end if

    if ((hp==0).or.(hp==-1)) then
	i=i+1
	var(i)%Discipline=0
	var(i)%pCat=1
	var(i)%pNum=0
	var(i)%tflevel=100
	var(i)%cfVarName="q"
	var(i)%VarName="Specific humidity  (kg kg-1)  (grib2/tables/2/4.2.0.1.table)"
    end if

    if ((hp>0).or.(hp==-1)) then
	i=i+1
	var(i)%Discipline=0
	var(i)%pCat=1
	var(i)%pNum=1
	var(i)%tflevel=100
	var(i)%cfVarName="rh"
	var(i)%VarName="Relative humidity  (%)  (grib2/tables/1/4.2.0.1.table)"
    end if

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=2
    var(i)%pNum=8
    var(i)%tflevel=100
    var(i)%cfVarName="w"
    var(i)%VarName="Vertical velocity (pressure)  (Pa s-1)  (grib2/tables/2/4.2.0.2.table)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=2
    var(i)%pNum=2
    var(i)%tflevel=100
    var(i)%cfVarName="u"
    var(i)%VarName="u-component of wind  (m s-1)  (grib2/tables/2/4.2.0.2.table)"

    i=i+1
    var(i)%Discipline=0
    var(i)%pCat=2
    var(i)%pNum=3
    var(i)%tflevel=100
    var(i)%cfVarName="v"
    var(i)%VarName="v-component of wind  (m s-1)  (grib2/tables/2/4.2.0.2.table)"



 end subroutine

 !--------------------------------------------------------
 ! Creates the fist of variable to be extracted from grib
 !-----------------------------------------------------------
 subroutine init_parm_grib1
    integer :: i
    i=0

    i=i+1
    var(i)%pNum=2 !
    var(i)%tflevel=102 !Sea Level Pressure (hPA)
    var(i)%cfVarName="prmsl"
    var(i)%VarName="Pressure reduced to MSL  (hPa) "

    i=i+1
    var(i)%pNum=132   ! 132 = topografia
    var(i)%tflevel=1  !"1=surface (of the Earth, which includes sea surface)  (grib1/3.table)
    var(i)%cfVarName="topo"
    var(i)%VarName="Surface Geopotential height  (gpm)"

    i=i+1
    var(i)%pNum=135  !pslc SURFACE PRESSURE [hPa] (grib1/2.46.254.table)  <<<<<**** hPa
    var(i)%tflevel=1
    var(i)%cfVarName="sp"
    var(i)%VarName="Surface pressure (PA)"
    psup_id=i

    i=i+1
    var(i)%pNum=7        !zgeo Geopotential height [gpm] (grib1/2.46.254.table)
    var(i)%tflevel=100
    var(i)%cfVarName="gh"
    var(i)%VarName="Geopotential height  (gpm)  (grib2/tables/2/4.2.0.3.table)"

    i=i+1
    var(i)%pNum=11
    var(i)%tflevel=100
    var(i)%cfVarName="t"
    var(i)%VarName="Temperature  (K)  (grib2/tables/2/4.2.0.0.table)"

    i=i+1
    var(i)%pNum=51
    var(i)%tflevel=100
    var(i)%cfVarName="q"
    var(i)%VarName="Specific humidity  (kg kg-1) "

    i=i+1
    var(i)%pNum=39     ! omeg OMEGA [Pa/s] (grib1/2.46.254.table)
    var(i)%tflevel=100
    var(i)%cfVarName="w"
    var(i)%VarName="Vertical velocity (pressure)  (Pa s-1) "

    i=i+1
    var(i)%pNum=33   ! IndicatorOfParameter=33   uvel ZONAL WIND  (U)
    var(i)%tflevel=100
    var(i)%cfVarName="u"
    var(i)%VarName="u-component of wind  (m s-1) (grib1/2.46.254.table)"

    i=i+1
    var(i)%pNum=34       !vvel MERIDIONAL WIND  (V)  (grib1/2.46.254.table)
    var(i)%tflevel=100   !100-Isobaric level pressure in hectoPascals (hPa)
    var(i)%cfVarName="v"
    var(i)%VarName="v-component of wind  (m s-1)  (grib1/2.46.254.table)"


    ! 43 Vorticity
    ! 35 Stream Function
    ! 36 Velocidade Potencial

!topo  0 132,1,0 ** surface TOPOGRAPHY [m]
!lsmk  0  81,1,0 ** surface LAND SEA MASK [0,1]
!PSLC    0  135,    1,    0  ** sfc    SURFACE PRESSURE                        (HPA             )
!UVES    0  192,    1,    0  ** sfc    SURFACE ZONAL WIND (U)                  (M/S             )
!UVEL   33   33,  100,    0  **        ZONAL WIND (U)                          (M/S             )
!VVES    0  194,    1,    0  ** sfc    SURFACE MERIDIONAL WIND (V)             (M/S             )
!VVEL   33   34,  100,    0  **        MERIDIONAL WIND (V)                     (M/S             )
!OMEG   33   39,  100,    0  **        OMEGA                                   (PA/S            )
!VORT   33   43,  100,    0  **        VORTICITY                               (1/S             )
!FCOR   33   35,  100,    0  **        STREAM FUNCTION                         (M2/S            )
!POTV   33   36,  100,    0  **        VELOCITY POTENTIAL                      (M2/S            )
!ZGEO   33    7,  100,    0  **        GEOPOTENTIAL HEIGHT                     (GPM             )
!PSNM    0    2,  102,    0  ** msl    SEA LEVEL PRESSURE                      (HPA             )
!TEMS    0  188,    1,    0  ** sfc    SURFACE ABSOLUTE TEMPERATURE            (K               )
!TEMP   33   11,  100,    0  **        ABSOLUTE TEMPERATURE                    (K               )
!UMRS    0  226,    1,    0  ** sfc    SURFACE RELATIVE HUMIDITY               (NO DIM          )
!UMRL   33   52,  100,    0  **        RELATIVE HUMIDITY                       (NO DIM          )
!UMES   33   51,  100,    0  **        SPECIFIC HUMIDITY                       (KG/KG           )
!AGPL    0   54,  200,    0  ** atm    INST. PRECIPITABLE WATER                (KG/M2           )
!TSFC    0  187,    1,    0  ** sfc    SURFACE TEMPERATURE                     (K               )
!DSTP    0   85,  112,    0  ** landt  DEEP SOIL TEMPERATURE                   (K               )

 end subroutine

 subroutine read_grib_tables
    character(len=1)::t1
    character(len=3)::t2
    integer::Discipline
    integer::PCat
    character(len=1024)::table
    character(len=1024)::xline

    call getenv("ECCODES_DIR",eccodes_dir)
 !------------------------------------------------------------
 ! path /eccodes/share/eccodes/definitions/grib2/shortName.def
 !------------------------------------------------------------

 !---------------------------------------------------------------------
 ! $(ECCODES_DIR)
 !path  /eccodes/share/eccodes/definitions/grib2/tables/2/4.2.0.3.table
 !                                                      | | | | |
 ! Tabela ----------------------------------------------+ | | | |
 !        ------------------------------------------------+ | | |
 !        --------------------------------------------------+ | |
 ! Discipline ------------------------------------------------+ |
 ! Category-----------------------------------------------------+
 !----------------------------------------------------------------------
 ! Parameter tables
  t1="2"
  t2="4.2"
  Discipline=0 !Meteorological products
  PCat=3 ! Mass
  write (table,111) trim(eccodes_dir),trim(t1),trim(t2),Discipline,PCat
  111 format(a,"/share/eccodes/definitions/grib2/tables/",a,"/",a,".",i1,".",i1,".table")
  print *,":MGRIB_TABLES:TABLE=",trim(table)
  open(33,file=table,status='unknown')
121    read(33,'(a)',end=222)xline
        print *,trim(xline)
        goto 121
222 continue
close(33)


!Code tables#
!  CODE TABLE 4.0, Product Definition Template Number
!# CODE TABLE 4.1, Category of parameters by product discipline
!    0 0 Temperature
!    0 1 Moisture
!    0 2 Momentum (wind)
!    0 3 Mass
!    0 4 Short Wave Radiation
!    0 5 Long  Wave Radiationm
!    0 6 Cloud
!    0 7 Thermodynamic Stability Indices
!    0 13 Aerosols
!    0 14 Trace Gases
!    0 15 Radar
!    0 18 Nuclear Radiology4.2.0.14.table
!    0 19 Physical atmospheric properties
!    0
!# CODE TABLE 4.2.Discipline.Category{ see table 4.1)?!!

!    # Product Discipline 0: Meteorological products, Parameter Category 4: Short-wave Radiation
!# CODE TABLE 4.3, Type of generating process
!# CODE TABLE 4.3, Type of generating process
!# Code table 4.5: Fixed surface types and units
!# CODE TABLE 4.6, Type of ensemble forecas


 end subroutine

 !------------------------------------------------------------------------------
 !convert_parameter_id
 !------------------------------------------------------------------------------
 ! Cnvert parameter ID according to appropriate grib table.
 ! Finds the grib identifier (grib_id) corresponding to the given parameter
 ! code (par_code)
 !-----------------------------------------------------------------------------
   function convert_parameter_id(par_code,grib_id); logical::convert_parameter_id
     character(len=*),       intent(in):: par_code
     type(grib_parameter_id),intent(out)::grib_id

     integer::v

     convert_parameter_id=.false.
     do v=1,nvar
        if (trim(par_code)==trim(var(v)%cfVarName)) then
            convert_parameter_id=.true.
            grib_id=var(v)
            exit
        end if
    end do

    if (.not.convert_parameter_id) then
        do v=1,nvar
            print *,v,var(v)%cfVarName
        end do
        print *,":GRIB_COMMON_PARAMETER: Error! Unknown parameter: ",trim(par_code)

    end if

   end function


 !------------------------------------------------------------------------------
 !read_cfVarName
 !------------------------------------------------------------------------------
 !-----------------------------------------------------------------------------
 subroutine read_cfVarName (grb_version)
    integer,intent(in)::grb_version ! Grib Vertion

    character(len=1024)::cfVar,xline
    character(len=1024)::full_line
     call getenv("ECCODES_DIR",eccodes_dir)
     !---------------------------------------------------------------------
     ! $(ECCODES_DIR)
     !path  /eccodes/share/eccodes/definitions/grib%grb_version/cfVarName.def


     write (cfVar,900) trim(eccodes_dir),grb_version
900 format(a,"/share/eccodes/definitions/grib",i1,"/cfVarName.def")
    print *,"cfVar_file=",trim(cfVar)

    open(9,file=trim(cfVar), status='unknown')
    full_line=""
    nvar=0
901    read(9,'(a)',end=902)xline
        call cutstring(xline,"#")
        full_line=trim(full_line)//trim(xline)
        if (nvar<nvarmax) then
            if (index(xline,"}")>0) then
                nvar=nvar+1
                call get_cfVarName(trim(full_line),var(nvar))
                full_line=""
            end if
        else
            print *,"Error! Number of variable in cfVarName.def ",nvarmax
            stop
        end if
        goto 901
902 continue

   ! Add some variables to the table
   ! Geometric Height
    nvar=nvar+1
    var(nvar)%pCat=3
    var(nvar)%pNum=6
    var(nvar)%tflevel=100
    var(nvar)%cfVarName="zm"
    var(nvar)%VarName="Geometric height (m)"

    print *,":mgrib_tables:read_cfVarName: Nvar=",nvar
    close(9)
 end subroutine

 !------------------------------------------------------------------------------
 !get_ncVarName
 !-----------------------------------------------------------------------------------
 ! return the index of NetCdf VarName  in the nc2grib.2.csv file"
 !------------------------------------------------------------------------------------
 integer function get_cfVarName_index(cfVarName)
   character(len=*),intent(in) ::cfVarName
   integer::i
   get_cfVarName_index=0
   if (nvar<1) then
     print *,"MGRIB_TABLES: Error in the nc2grib table!"
     stop
   end if
   
   do i=1,nvar
          if(trim(cfVarName)==trim(var(i)%cfVarName)) then
            get_cfVarName_index=i
            exit
          end if
     end do
 end function

 !------------------------------------------------------------------------------
 !get_ncVarName
 !-----------------------------------------------------------------------------------
 ! return the index of NetCdf VarName  in the nc2grib.2.csv file"
 !------------------------------------------------------------------------------------
 integer function get_ncVarName_index(ncVarName)
   character(len=*),intent(in) ::ncVarName
   integer::i
   
   get_ncVarName_index=0
   
   if (nvar<1) then
     print *,"MGRIB_TABLES: Error in the nc2grib table!"
     stop
   end if
   
   
   do i=1,nvar
          if(trim(ncVarName)==trim(var(i)%ncVarName)) then
            get_ncVarName_index=i
            exit
          end if
     end do
 end function

subroutine get_cfVarName(fline,par_id)
    character(len=*),         intent(in)::fline
    type (grib_parameter_id),intent(out)::par_id
    integer::ncc,i
    character(len=255),dimension(100)::cc
   ! print *,">",strs(trim(fline)," ")

    call split(fline,'={;}',cc,ncc)
    par_id%cfVarName=strs(trim(cc(1)),"'")
    par_id%pCat=0
    par_id%pNum=0
    par_id%tflevel=0  !102 !Sea Level Pressure (hPA)

    i=0
    do while (i<ncc)
        i=i+1
        if ( index(cc(i),'discipline')>0) then
            i=i+1
            par_id%Discipline=val(cc(i))
            i=i+1
        end if
        if ( index(cc(i),'parameterCategory')>0) then
            i=i+1
            par_id%pCat=val(cc(i))
            i=i+1
        end if
        if ( index(cc(i),'parameterNumber')>0) then
            i=i+1
            par_id%pNum=val(cc(i))
            i=i+1
        end if
    end do

    if (index(par_id%cfVarname,"prmsl")>0) then
			par_id%tflevel=101
		elseif (index(par_id%cfVarname,"sp")>0) then
            par_id%tflevel=1
		else
		    par_id%tflevel=100
    end if
   ! print *,"cfVarName=",par_id%cfVarName
   ! print *,"Discipline=",par_id%Discipline
   ! print *,"parameterCategory=",par_id%pCat
   ! print *,"parameterNumber=",par_id%pNum

 end subroutine


 end module
