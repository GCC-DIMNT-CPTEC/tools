program mpas_nc2grib2
!----------------------------------------------------------------------   
! mpas_nc2grib2
!  
! Simple program to read MPAS post processed data in NETCDF and convert 
! to grib2
!-----------------------------------------------------------------------
! Note: This program was done to process netcdf from mpas with
!       the following dimension sequence :
!
!       Sequence =  Lon, Lat, Level Time
!-----------------------------------------------------------------------
! Autors
!    SHSF sergio.ferreira@inpe.br
!-----------------------------------------------------------------------
! HISTORY
! 2024-07-07  V1.0 SHSF: Original version
! 2024-11-06  V1.1 SHSF: Check of sequence of dimension (matrix shape) in netcdf was implemented
!                      : for 2 cases: xyzt and zxyt
! 2025-05-10  V2.0-beta: New approach: configuration table nc2grib2.csv was replaced with nc2grib.2.xml
! 2025-07-01  Improviment: Check of the coordinated variables sequence were included
  use netcdf
  use stringflib
  use datelib
  use mgrib_interface
  use mgrib_tables, only :init_parm,init_parm2, var,svar=>nvar,get_cfVarName_index,get_ncVarName_index
  use metlib, only :rseca, saturation_Adiabatic,temperature_lapse_rate,w2omega,tvirtw_kelvin

  implicit none

  ! This will be the netCDF ID for the file and data variable.
  integer  ::ncid   !NetCDF ID, from a previous call to NF90_OPEN or NF90_CREATE.
  integer  ::varid
  integer  ::dimid
  integer  ::ndim,nvar
  integer  ::xtype, len, attnum
  integer  ::ndims
  integer  ::nlat,nlon,nlev
  integer,dimension(6)::dimids
  character(len=1024) ::  name
  character(len=1),dimension(4)::cdim    ! Identification of the dimensions (x,y,z, etc)
  character(len=1),dimension(4)::hdim    ! Identification of the dimensions (header)
  character(len=4)             ::seqdim  ! Sequence of dimensions (x,y,z etc)
  character(len=15)            ::Clat,Clon,Clev
  character(len=60)            ::time_unit
  integer                      ::ilat,ilon,ilev,itim
  integer::nt
  integer::vv
  real :: hh
  real,dimension(:),      allocatable  :: lon,lat,lev
  real,dimension(:,:,:,:,:),allocatable:: vxyzt,vzxyt
  real,dimension(:,:,:),allocatable    :: dz,dz2,zp
  logical,dimension(:),allocatable     :: check_var
  type(grib_interface_def)             :: grib_def
  real *8                              :: start_date
  real *8                              :: ref_date
  integer                              :: ifct
  real                                 :: P, Tv
  integer                              :: step
  real *8                              :: lonf,loni,dlon,latf,lati,dlat


  CHARACTER(LEN=15)                ::dname,udname
  character(len=nf90_max_name)     ::vin_name
  character(len=254)               ::longname
  character(len=254)               ::conftable 
  INTEGER                          ::dlength
  character (len = 1024)           ::FILE_NAME,txt
  character(LEN=8)                 ::ymd
  integer*8                        ::iymd

  character (len = 1024)           ::OUTFILE, OUTFILE2,outfile_c
  character(len=1),dimension(100)  ::namearg   !. Nome dos argumentos!
  character(len=256),dimension(100)::arg       !.argumentos!
  integer::nargs                               !. numero de argumentos efetivamente passados!
  integer::i,j,k                               !. Variavel auxiliar!
  integer::x1,x2,X3,X4
  integer::op
  integer::verbose

  x1=0
  x2=0       
  X3=0
  X4=0
  op=0                                                                   !
  ifct=0
  
  !-----------
  ! welcomme 
  !-----------
  
   call get_parameter(namearg,arg,nargs)
   conftable="nc2grib.2.xml"
   verbose=0                                                                          !
      do i=1, nargs 
        select case (namearg(i))
            case ("i") 
                FILE_NAME=trim(arg(i))
                x1=1
            case ("o")
              OUTFILE=trim(arg(i))
              x2=1
            case ("p")
              op=val(arg(i))
            case ("s")
              start_date=fjulian(arg(i))
              x3=1
            case ("f")
              ifct=val(arg(i))
            case ("v")
              verbose=val(arg(i))
	    case ("c")
	      conftable=trim(arg(i))	
            case default
                
	    end select
	!
      end do
       print *,"|--------------------------------------------------------------+"
       print *,"| mpas_nc2grib2.x                                              |"
       print *,"|--------------------------------------------------------------+"
       print *,"| MCTI-INPE (2025-11-06) V. 2.0 beta                           |"
       print *,"|--------------------------------------------------------------+"

      if (x1*x2*x3==0) then
       print *,"| Use mpas_nc2grib2.x -i <filename.nc> -o <outfile_basename>   |"
       print *,"|                          -s yyyymmddhh -f fff  { Options }   |"
       print *,"|                                                              |"
       print *,"|    -s start_time: (yyyymmddhh)                               |"
       print *,"|    -f forecast time  (fff)                                   |"
       print *,"|                                                              |"
       print *,"| Options:                                                     |"
       print *,"|    -p :packing type                                          |"
       print *,"|        :0=  grid_simple                                      |"
       print *,"|        :1=  grid_ccsds                                       |"
       print *,"|        (Default: 0)                                          |"
       print *,"|                                                              |"
       print *,"|    -c :<Configuration table name>                            |"
       print *,"|        (Default= nc2grib.2.xml)                              |"
       print *,"|--------------------------------------------------------------+"

	stop
      endif 
                                                                       !
  !=========
  ! Start 
  !=========
  
  !-------------------------------------------------------------------------------
  ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
  ! Allocate variables
  !-------------------------------------------------------------------------------
  ! Attention:  The  dimensions of variables  must be declared back to front in 
  !             fortran- 
  !             ex.:float mslp(time, lat, lon) ;--> fortran mslp(lon,lat,time)

  !-------------------------------------------------------------------------------
     print *,":MPAS_NC2GRIB2: Input filename = ", trim(FILE_NAME)
     call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid) )
     call check( nf90_inquire(ncid,ndim,nvar))
     if (verbose>0) print *,":MPAS_NC2GRIB2: ndim=",ndim
     if (verbose>0) print *,":MPAS_NC2GRIB2: ncid=",ncid
     if (verbose>0) print *,":MPAS_NC2GRIB2: nvar=",nvar
     if (verbose>0) print *,":MPAS_NC2GRIB2:nf90_max_name=",nf90_max_name

     !-----------------------------------------
     ! search for the Coordinates informations
     !----------------------------------------
     do i =1,ndim
     	call check(nf90_inquire_dimension(ncid,i,dname,len=dlength))
     !	print *,">",i,trim(dname)
        udname=trim(UCASES(dname))
        select case(trim(udname))
		case ('LON','LONGITUDE')
			nlon=dlength
			clon=trim(dname)
			cdim(i)="X"
		case ('LAT','LATITUDE')
			nlat=dlength
			clat=trim(dname)
            cdim(i)="Y"
 		case ('LEV','LEVEL')
			nlev=dlength
			clev=trim(dname)
            cdim(i)="Z"
		case ('TIME')
			nt=dlength
			cdim(i)="T"
			call check(nf90_get_att(ncid, i, "units", time_unit))
			ref_date=start_date+(ifct/24.0)
			if (verbose>0) print *,":MPAS_NC2GRIB2: NC Time unit=",trim(time_unit)
			if (verbose>0) print *,":MPAS_NC2GRIB2: start_date=",grdate(start_date)
			if (verbose>0) print *,":MPAS_NC2GRIB2: ref_date=",grdate(ref_date)
		case default
			print *, ":MPAS_NC2GRIB2: ERROR: nc_check for dimensions!"; stop

		end select
     end do
!------------------------------------------------     
! Check of the coordinate variable sequence
! Supposedly the First 4 Variables
!------------------------------------------------
     do varid = 1,4
      call check( nf90_inquire_variable(ncid, varid, vin_name,xtype,ndims,dimids))
        udname=trim(UCASES(vin_name))
        select case(trim(udname))
		case ('LON','LONGITUDE')
			hdim(varid)="X"
			ilon=varid
		case ('LAT','LATITUDE')
		    hdim(varid)="Y"
		    ilat=varid
 		case ('LEV','LEVEL')
		      hdim(varid)="Z"
		      ilev=varid
		case ('TIME')
		      hdim(varid)="T"
		      itim=varid
        end select
    end do
    if (verbose>0) print *,":MPAS_NC2GRIB2: Coordinate sequence = [ ",hdim(1:4)," ]"
    
     !---------------------------------------------------------------
     ! Check the sequence xyzt defined in the meteorological variables
     ! starts from variable 5
     !---------------------------------------------------------------
     do varid = 5,nvar
      call check( nf90_inquire_variable(ncid, varid, vin_name,xtype,ndims,dimids))
      !print *,"vin_name=",varid, trim(vin_name)
      if (dimids(4)>0) then
        seqdim=""
        do i=1,4
          seqdim=trim(seqdim)//cdim(dimids(i))
        end do
        print *,":MPAS_NC2GRIB2: Matrix shape = [ ",trim(seqdim)," ]"
        exit
      end if
     end do

     ! Inicialize parameter definitions
     call init_parm2 (conftable)


     allocate (check_var(0:svar))
     allocate (lon(0:nlon-1))
     allocate (lat(0:nlat-1))
     allocate (lev(0:nlev-1))
     if (seqdim=="XYZT") then
          allocate (vxyzt(0:svar,0:nlon-1,0:nlat-1,0:nlev-1,0:nt-1))
     elseif (seqdim=="ZXYT") then
          allocate(vzxyt(0:svar,0:nlev-1,0:nlon-1,0:nlat-1,0:nt-1))
     else
         print *,":MPAS_NC2GRIB2: ERROR: unrecognized matrix shape"
         print *,":MPAS_NC2GRIB2: STOP"
         stop
     end if
     check_var(:)=.false.

 !-----------
 ! Read DATA
 !----------
 !*** Read Lon ***
  call check(nf90_get_var(ncid, ilon, lon) )
  if (verbose>0) print *,":MPAS_NC2GRIB2: Reading Longitudes.. "
  ! Recalculate longitudes from boders to the center of grid points
   dlon=(lon(nlon-1)-lon(0))/(nlon-1)
   dlon=round(dlon,4)
   loni=lon(0)
   lonf=lon(nlon-1)

   !loni=lon(0)+dlon/2.0
   !lonf=dlon*(nlon-1)+loni

  if (verbose>0) print *,":MPAS_NC2GRIB2: nlon=",nlon,"lon = [", loni,":",lonf ,":",dlon," ]"
  
  
 !*** Read Lat ***

  call check(nf90_get_var(ncid, ilat, lat) )
  if (verbose>0) print *,":MPAS_NC2GRIB2: Reading Latitudes.. "
   ! Recalculate latitudes from boders to the center of grid points
   dlat=(lat(nlat-1)-lat(0))/(nlat-1)
   dlat=round(dlat,4)
   lati=lat(0)
   latf=lat(nlat-1)

   !lati=lat(0)+dlat/2.0
   !latf=dlat*(nlat-1)+lati


  if (verbose>0) print *,":MPAS_NC2GRIB2: nlat=",nlat,"lat = [", lati,":",latf,":",dlat,"]"

 !*** Read Lev ***

  call check(nf90_get_var(ncid, ilev, lev) )
  if (verbose>0) print *,":MPAS_NC2GRIB2: nlev=",nlev,"lev = [", lev(0),"-",lev(nlev-1),"]"

  if (verbose>0) then
  	print *,":MPAS_NC2GRIB2: Variables in netcdf: nvar=",nvar
	print *,":MPAS_NC2GRIB2: Selected variables : svar=",svar
  end if 
  call check( nf90_inquire(ncid,ndim,nvar))

  !-------------------------------

  do varid = 5,nvar
    vin_name=""

    call check( nf90_inquire_variable(ncid, varid, vin_name,xtype,ndims))

    if ( len_trim(vin_name)==0) then 
        print *,":MPAS_NC2GRIB2: Error in nf90_inquire_variable!"
        print *,"  ncid,varid=",ncid,varid
        stop
     end if 

   i=get_ncVarName_index(vin_name)
   if (i>0) then
      if ((ndims>3).and.(var(i)%tflevel/=100)) then
         print *,varid," nc=[",trim(vin_name),"] cf=[",trim(var(i)%cfVarName),"] Error in the variable dimension deffinition"
      else
         if (verbose>1) print *,varid," nc=[",trim(vin_name),"] cf=[",trim(var(i)%cfVarName),"] OK"
      end if

      check_var(i)=.true.
       if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)

       if (seqdim=="XYZT") then
       !------------------------------------------------------

          if ((var(i)%tflevel/=100).and.(ndims==3)) then
            call check(nf90_get_var(ncid, varid, vxyzt(i,:,:,0,:)))
          else
             call check(nf90_get_var(ncid, varid, vxyzt(i,:,:,:,:)))
          end if
       !-----------------------------------------------------------
       else
       !-------------------------------------------------------------
           if ((var(i)%tflevel/=100).and.(ndims==3)) then
            call check(nf90_get_var(ncid, varid, vzxyt(i,0,:,:,:)))
          else
             call check(nf90_get_var(ncid, varid, vzxyt(i,:,:,:,:)))
          end if
       !--------------------------------------------------------------
       end if

    else
       if (verbose>2) then
           call check( nf90_get_att(ncid, varid,'long_name', longname))
          print *,varid," nc=[",trim(vin_name),"] cf=[ ",trim(color_text("IGNORED",33,.false.))," ] --> ",trim(longname)
        end if
    end if
  end do
  call check( nf90_close(ncid) )

  !-------------------------------------------------
  ! Check vertical level unit. If hPa convert to Pa
  !-------------------------------------------------
   if (lev(0)<lev(nlev-1)) then
      print *,":MPAS_NC2GRIB2: Error in vertical levels"
      stop
   end if

  if (lev(0)<50000) then
    lev(:)=lev(:)*100
  end if


!--------------------------
! GRIB DEFINITIONS
!--------------------------

    grib_def%productionStatus=1
    
    !^^^^^^^^^^^^^^^^^^^^^^^^^-------------------
    ! CODE TABLE 1.3, Production status of data
    !--------------------------------------------
    !0 0  Operational products
    !1 1  Operational test products
    !2 2  Research products
    !3 3  Re-analysis products
    !4 4  TIGGE Operational products
    !5 5  TIGGE test products
    !---------------------------------------------
    
    grib_def%edition=2
    ymd=date_ymd(start_date)
    read(ymd,'(i8)')iymd
    grib_def%idate=iymd
    grib_def%itime=val(time_hm(start_date)//"00")
    !grib_def%GridType="regular_ll"
    allocate (grib_def%lon(0:nlon-1))
    allocate (grib_def%lat(0:nlat-1))
    allocate (grib_def%lev(0:nlev-1))
    grib_def%lon(:)=lon(:)
    grib_def%lat(:)=lat(:)
    grib_def%lev(:)=lev(:)
    grib_def%ilat=lati
    grib_def%ilon=loni
    grib_def%dlat=dlat
    grib_def%dlon=dlon

    grib_def%NI=nlon
    grib_def%NJ=nlat
    grib_def%NK=nlev
    
    !--------------
    ! Packing Type
    !-------------
    
    if (op==1) then
       grib_def%packingType="grid_ccsds"
    else
       grib_def%packingType="grid_simple"
    end if
    
    if (verbose>2)  print *,"idate=",grib_def%idate
    if (verbose>2)  print *,"itime=",grib_def%itime
    step=ifct

!-----------------------------
! Saving data in grib2 format
!------------------------------
    outfile_c=current_filename(OUTFILE,start_date,ifct);
    print *,":MPAS_NC2GRIB2: output filename=",trim(outfile_c)
    call openw_grib(outfile_c,grib_def)


     if (seqdim=="XYZT") then
     !------------------------------
      do i=1,svar
       if (check_var(i)) then
          if (var(i)%tflevel==100) then
             do k=0,grib_def%NK-1
                call write_grib2(var(i),vxyzt(i,:,:,k,0),grib_def%lev(k),step)
             end do
           else
             call write_grib2(var(i),vxyzt(i,:,:,0,0),0.0,step)
           end if
      else
          txt=trim(var(i)%cfVarName)//"-"//trim(var(i)%VarName)
          print *,i,trim(txt)//trim(color_text(": NOT FOUND!",31,.false.))
      end if
    end do
    !------------------------------------
    else
    !-----------------------------------
    do i=1,svar
       if (check_var(i)) then
          if (var(i)%tflevel==100) then
             do k=0,grib_def%NK-1
                call write_grib2(var(i),vzxyt(i,k,:,:,0),grib_def%lev(k),step)
             end do
           else
             call write_grib2(var(i),vzxyt(i,0,:,:,0),0.0,step)
           end if
      else
        txt=trim(var(i)%cfVarName)//"-"//trim(var(i)%VarName)
        print *,i,trim(txt)//trim(color_text(": NOT FOUND!",31,.false.))

      end if
    end do
    !-----------------------------------
    end if

    call close_grib()
  print *,"*** MPAS_NC2GRIB2: DONE *** "
  print *,""

  deallocate (lat,lon,lev)
  deallocate (grib_def%lon,grib_def%lat,grib_def%lev)
  if (seqdim=="XYZT") then
    deallocate (vxyzt)
  else
    deallocate( vzxyt)
  end if


contains
  !
  !
  !
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  


!-------------------------------------------------------------------
! Notas: Existem dois modelos de nome de arquivo comument utilizados
!
!      1) nome_[start_time]_f[Forecast_Time]
!         Onde star_time  =%y4%m2%d2%h2  e Forecast_Time=%f3 (Hour)
!         sao variáveis
!
!      2) nome_[Start_time]_[Reference_Time]
!         Onde: [Start_time] é uma data fixa
!         [Reference_time]=%y4%m2%d2%h2 é a data da previsao,
!         que varia conforme o passo de previsao
!
! Use
! 1)  current_filename (filename_%y4%m2%d2%h2_f%f3,start_time,fff)
!     para obter nome do arquivo no caso 1
!
! 2)  current_filename (filename_[start_time]_%y4%m2%d2%h2,reference_time,0)
!     para obter nome do arquivo no caso 2.
!
  character(len=2024)  function current_filename(filename_in,stime,ifct);

    character(len=*),intent(in)::filename_in !
	real*8,          intent(in)::stime       ! Julian start date and time
	integer,optional, intent(in)::ifct       ! Forecast time indicator (optional)

	real*8:: ftime                           ! Julian forecast date and time
	character(len=4)::yy
	character(len=2)::mm,dd,hh
	character(len=3)::fct
	character(len=1024)::filename

	! Start

	 filename=trim(filename_in)

      write(yy,'(i4)')year(stime)
      write(mm,'(i2.2)')month(stime)
      write(dd,'(i2.2)')day(stime)
      write(hh,'(i2.2)')hour(stime)
      filename=replace(filename,"%Y4",yy)
      filename=replace(filename,"%M2",mm)
      filename=replace(filename,"%D2",dd)
      filename=replace(filename,"%H2",hh)

      if (present(ifct)) then
        ftime=stime+real(ifct)/24.0
      else
        ftime=stime
      end if

        write(fct,'(i3.3)')ifct
        write(yy,'(i4)')year(ftime)
        write(mm,'(i2.2)')month(ftime)
        write(dd,'(i2.2)')day(ftime)
        write(hh,'(i2.2)')hour(ftime)
        filename=replace(filename,"%y4",yy)
        filename=replace(filename,"%m2",mm)
        filename=replace(filename,"%d2",dd)
        filename=replace(filename,"%h2",hh)
        filename=replace(filename,"%f3",fct)



	 current_filename=trim(filename)
   end function

   real function round(val, n)
      implicit none
      real*8,intent(in) :: val
      integer,intent(in) :: n
      real*8::val2
      val2=val*(10.0**n)+0.5
      round = int(val2)/10.0**n
    end function round

end program
