program mpas_nc2grib2
!----------------------------------------------------------------------   
! mpas_convert1 
!  
! Simple program to read MPAS post processed data in NETCDF and convert 
! to other formats  
! 
! 
! (C)2023  sergio.ferreira@inpe.br 
!---------------------------------------------------------------------
! Note: This program was done to process netcdf from mpas with
!       the following dimension sequence :
!
!       Sequence = Level, Lon, Lat, Time

  use netcdf
  use stringflib
  use datelib
  use mgrads_interface
  use mgrib_interface
  use mgrib_tables, only :init_parm,var,svar=>nvar,get_cfVarName_index,get_ncVarName_index
  use metlib, only :rseca, saturation_Adiabatic,temperature_lapse_rate,w2omega,tvirtw_kelvin
  !use mgrib_tables
  implicit none

  ! This will be the netCDF ID for the file and data variable.
  integer ::ncid   !NetCDF ID, from a previous call to NF90_OPEN or NF90_CREATE.
  integer ::varid
  integer ::dimid
  integer::ndim,nvar
  character(len=1024)::  name
  integer  :: xtype, len, attnum

  integer::nlat,nlon,nlev
  character(len=15)       ::Clat,Clon,Clev
  character(len=60)       ::time_unit
  integer::nt
  integer::vv
  real :: hh
  real,dimension(:),      allocatable:: lon,lat,lev
  real,dimension(:,:,:),  allocatable:: mslp,ps
  real,dimension(:,:,:,:),allocatable:: t,z,u,v,w,q
  real,dimension(:,:,:),allocatable :: dz,dz2,zp
  logical,dimension(:),allocatable  :: check_var
  real *8                           :: start_date
  real *8                           :: ref_date
  real                              :: P, Tv


  character(len=30)                 ::z_name,u_name,v_name,w_name

  CHARACTER(LEN=15)        ::dname
  character(len=30)        ::vin_name
  INTEGER                          :: dlength
  character (len = 1024)           ::FILE_NAME
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
  op=-1                                                                   !
  hh=0
  
  !-----------
  ! welcomme 
  !-----------
  
   call get_parameter(namearg,arg,nargs)
                  !
   verbose=3                                                                          !
      do i=1, nargs 
        select case (namearg(i))
            case ("i") 
                FILE_NAME=trim(arg(i))
                x1=1
            case ("o")
              OUTFILE=trim(arg(i))
              x2=1
            case ("r")
              op=val(arg(i))
              if ((op>3).or.(op<0)) x2=0
            case ("s")
              start_date=fjulian(arg(i))
              x3=1
            case ("f")
              hh=val(arg(i))
            case default
                
	    end select
	!
      end do
       print *,"|--------------------------------------------------------------+"
       print *,"| mpas_convert1.x                                              |"
       print *,"|--------------------------------------------------------------+"
       print *,"| sergio.ferreira@inpe.br (2024-03-07) V. 1.0                  |"
       print *,"|--------------------------------------------------------------+"

      if (x1*x2*x3==0) then
       print *,"| Use mpas_convert1.x -i <filename.nc> -o <outfile_basename>   |"
       print *,"|                          -s yyyymmddhh -f ffff  {-r <option>}|"
       print *,"|                                                              |"
       print *,"|    -s start_time: (yyyymmddhh)                               |"
       print *,"|    -f forecast time  (fff)                                   |"
       print *,"|                                                              |"
	   print *,"|    -r :type of data process when the level is beneath surface|"
       print *,"|        :0= remove data beaneath surface using surface pres.  |"
	   print *,"|        :1= remove data beaneath surface using layer thickness|"
	   print *,"|        :2= calculate t,h beaneath surface (lapserate Method )|"
	   print *,"|        :3= calculate t,h beaneath surface (s.adiabats Method)|"
	   print *,"|                                                              |"
	   print *,"|     Default: Read and write without data processing          |"
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
  print *,trim(FILE_NAME) 
     call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid) )
     call check( nf90_inquire(ncid,ndim,nvar))
     
     print *,"Number of dimensions=",ndim
     print *,"Number of variables=",nvar
     print *,"-----------------------------------------------------"
     print *,"Dimensions"
     print *,"-----------------------------------------------------"
     do i =1,ndim
     	call check(nf90_inquire_dimension(ncid,i,dname,len=dlength))
         print *,">>>",i,dname,dlength
	
	select case(trim(dname))
		case ('lon','LON','longitude')
			nlon=dlength
			clon=trim(dname)
		case ('lat','LAT','latitude')
			nlat=dlength
			clat=trim(dname)
 		case ('lev','LEV','level')
			nlev=dlength
			clev=trim(dname)
		case ('time','TIME')
			nt=dlength
			call check(nf90_get_att(ncid, i, "units", time_unit))
			print *,">>> Time unit=",trim(time_unit)

			!start_date=fjulian(time_unit)
			ref_date=start_date+(hh/24.0)

			print *,">>> start_date=",grdate(start_date)
			print *,">>> ref_date=",grdate(ref_date)
		case default
			print *, "ERROR: nc_check for dimensions!"; stop
		end select
     end do
     !print *,"----------------------------------------------------"
     !print *,"Date & time=",nt
      ! Inicialize parameter definitions
     call init_parm("nc2grads.csv")
     allocate (check_var(0:svar))
     allocate (lon(0:nlon-1))
     allocate (lat(0:nlat-1))
     allocate (lev(0:nlev-1))
     allocate (mslp(0:nlon-1,0:nlat-1,0:0))
     allocate (ps(0:nlon-1,0:nlat-1,0:0))
     allocate (t(0:nlev-1,0:nlon-1,0:nlat-1,0:0))         !:"Temperature interpolated to isobaric
     allocate (z(0:nlev-1,0:nlon-1,0:nlat-1,0:0)); z_name="geoph"                !:"Height interpolated to isobaric
     allocate (q(0:nlev-1,0:nlon-1,0:nlat-1,0:0))
     allocate (u(0:nlev-1,0:nlon-1,0:nlat-1,0:0)); u_name="uzonal_isobaric"      !:"Zonal wind interpolated to
     allocate (v(0:nlev-1,0:nlon-1,0:nlat-1,0:0)); v_name="umeridional_isobaric" !:"Meridional
     allocate (w(0:nlev-1,0:nlon-1,0:nlat-1,0:0)); w_name="w_isobaric"           !
     allocate (dz(0:nlev-1,0:nlon-1,0:nlat-1))
     allocate (dz2(0:nlev-1,0:nlon-1,0:nlat-1))
     allocate (zp(0:nlev-1,0:nlon-1,0:nlat-1))

     check_var(:)=.false.

 !-----------
 ! Read DATA
 !----------

 !*** Read Lon ***
  call check( nf90_inq_dimid(ncid, trim(clon),dimid))
  call check(nf90_get_var(ncid, dimid, lon) )
  !print *,"Varid=",varid,"lon = [", lon(0),"-",lon(nlon-1),"]"
  
  
 !*** Read Lat ***
  call check( nf90_inq_dimid(ncid, trim(clat),dimid))
  call check(nf90_get_var(ncid, dimid, lat) )
  !print *,"Varid=",varid,"lat = [", lat(0),"-",lat(nlat-1),"]"

 !*** Read Lev ***
  call check( nf90_inq_dimid(ncid, trim(clev),dimid))
  call check(nf90_get_var(ncid, dimid, lev) )
  !print *,"Varid=",varid,"lat = [", lev(0),"-",lev(nlev-1),"]"





  call check( nf90_inquire(ncid,ndim,nvar))
  do varid = 5,nvar
   call check( nf90_inquire_variable(ncid, varid, vin_name))
   i=get_ncVarName_index(vin_name)
   if (i>0) then
      !print *,varid," nc=[",trim(vin_name),"] cf=[",trim(var(i)%cfVarName),"] OK"
      check_var(i)=.true.
      if (trim(var(i)%cfVarName)=="prmsl") then
        if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
        call check(nf90_get_var(ncid, varid, mslp))
        if (verbose>2) print *,"...mslp=", mslp(20,20,0)

     elseif (trim(var(i)%cfVarName)=="sp") then
       if (verbose>2) print *,"...Reading ",i,"Reading [",trim(vin_name),"]= ",trim(var(i)%VarName)
       call check(nf90_get_var(ncid, varid, ps))
       if (verbose>2) print *,"...ps=", ps(20,20,0)

     elseif (trim(var(i)%cfVarName)=="t") then
       if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
       call check(nf90_get_var(ncid, varid, t))
       if (verbose>2) print *,"t=", t(5,20,20,0)

     elseif (trim(var(i)%cfVarName)=="dist") then
       if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
       call check(nf90_get_var(ncid, varid, z))
       if (verbose>2) print *,"z=", z(5,20,20,0)

      elseif (trim(var(i)%cfVarName)=="q") then
       if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
       call check(nf90_get_var(ncid, varid, q))
       if (verbose>2) print *,"q=", q(5,20,20,0)

     elseif (trim(var(i)%cfVarName)=="u") then
       if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
       call check(nf90_get_var(ncid, varid, u))
       if (verbose>2) print *,"u=", u(5,20,20,0)

     elseif (trim(var(i)%cfVarName)=="v") then
        if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
        call check(nf90_get_var(ncid, varid, v))
        if (verbose>2) print *,"v=", v(5,20,20,0)

       elseif (trim(var(i)%cfVarName)=="w") then
         if (verbose>2) print *,"...Reading ",i," [",trim(vin_name),"]= ",trim(var(i)%VarName)
         call check(nf90_get_var(ncid, varid, w))
         if (verbose>2) print *,"w=", w(5,20,20,0)
     end if

  ! else
  !     print *,varid," nc=[",trim(vin_name),"] cf=[ NONE ]"

   end if
end do


  call check( nf90_close(ncid) )

!---------------
! Check variables
!----------------
do i=1,svar
  if (check_var(i)) then
     print *,i,trim(var(i)%VarName)," = [ OK ]"
  else
     print *,i,trim(var(i)%VarName)," = [ NONE ]"

  end if
end do


!------------------------------
! thickness calculation 
!-----------------------
 dz(:,:,:)=bf%undef
   do i=0, nlon-1
    do j=0,nlat-1
     do k=0,nlev-2
       dz(k+1,i,j)=(z(k,i,j,0)-z(k+1,i,j,0))
      end do
    end do
  end do

  !------------------------
  ! Convert W to Omega
  !------------------------
  do k=0,nlev-1
    P=lev(k)
    do i=0, nlon-1
      do j=0,nlat-1
       tv=Tvirtw_Kelvin(t(k,i,j,0), q(k,i,j,0))
       w(k,i,j,0)=w2omega(w(k,i,j,0),P,Tv)
      end do
    end do
  end do

  !---------------------------
  ! initialize mgrads
  !---------------------------
  outfile_c=current_filename(outfile,ref_date,0)
  call create_grfile(trim(outfile_c),nlon,nlat,nlev,nvar)
  call grfile_defdims(lat,lon,lev,ref_date,6)
  call grfile_defvars
  
!------------------------------
! thickness calculation
!------------------------------
 if ((op==0).or.(op==1)) then
   call cut_beneath_surface(op)
 elseif (op==2) then
   call hydrostatic(1)
  elseif (op==3) then
    call hydrostatic(2)
  else
     zp(:,:,:)=z(:,:,:,0)
  end if
!--------
! save
! -------
  
  call conv_xy2xyz(mslp(0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"prmsl",xyz)

  call conv_xy2xyz(ps(0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"sp",xyz)

  call conv_zxy2xyz(z(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"z",xyz)
 
  call conv_zxy2xyz(t(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"t",xyz)
 
  call conv_zxy2xyz(q(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"q",xyz)

  call conv_zxy2xyz(u(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"u",xyz)
  
  call conv_zxy2xyz(v(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"v",xyz)
  
  call conv_zxy2xyz(v(0:nlev-1,0:nlon-1,0:nlat-1,0))
  call grfile_writevars(1,"w",xyz)
  
  call conv_zxy2xyz(dz(0:nlev-1,0:nlon-1,0:nlat-1))
  call grfile_writevars(1,"dz",xyz)

  call conv_zxy2xyz(dz2(0:nlev-1,0:nlon-1,0:nlat-1))
  call grfile_writevars(1,"dz2",xyz)

  call conv_zxy2xyz(zp(0:nlev-1,0:nlon-1,0:nlat-1))
  call grfile_writevars(1,"h",xyz) ! gh
  call grfile_writevars(1,"gh",xyz)


  deallocate(xyz)
  print *,bf%time_init
  print *,bf%time_step
  call grfile_close
  
  print *,"*** MPAS_CONVERRT1: DONE *** "
  print *,""

  deallocate (lat,lon,lev,mslp,t,z,u,v,dz,dz2,check_var)
  

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

!---------------------------------------------------------------------------------------
!  Thickness
!       returns the thickness values between isobatic levels (Press1 & Press2) with
!       mean virtual temperature (Tv)
!
!---------------------------------------------------------------------------------------

 Function thickness(press1, press2,t,q);real:: thickness

	!{ Variaveis de entrada
		real,intent(in)::press1,press2  ! pressure at base and top of layer  (hPa or Pa)
		real,intent(in)::t              ! Mean layer Temperature (K)
		real,intent(in)::q              ! Mean specific Humidit (kg/kg)
	!}

	!{ Variaveis locais
        real::tv_kelvin	!Temperatura virtual media da camada em graus kelvin)
		real ::p0,p
		real ::dz
		real,parameter::Rd=287 ! J/KgK
		real,parameter:: g=9.8
	!}

	p0=press1
	p=press2
  	If (p0 == 0)  p0 = 0.00001
  	If (p == 0 )  p = 0.00001
  	tv_kelvin = t * (1 + 0.61 * (q/(1-q)))
  	dz = tv_kelvin * Rd/g * Log(p0 / p)
  	thickness = dz
 End Function


 !-----------------------------
 ! cut data beneath surface
 !-----------------------------
 subroutine cut_beneath_surface(option)
  integer,intent(in)::option
  real::qm,tm
  integer::i,j,k
  
 dz2(:,:,:)=bf%undef
  
  Print *,"cutting data beneath the surface: option= ",option
   
   do i=0, nlon-1
    do j=0,nlat-1
     do k=0,nlev-2
      tm=(t(k,i,j,0)+t(k+1,i,j,0))/2.0
      qm=(q(k,i,j,0)+q(k+1,i,j,0))/2.0
      dz2(k+1,i,j)=thickness(lev(k+1),lev(k),tm,qm)

      if (option==1) then
	      if ((dz2(k+1,i,j)-dz(k+1,i,j))>15.0) then
	        zp(k+1,i,j)=bf%undef
	      else
	        zp(k+1,i,j)=z(k+1,i,j,0)
          end if

       else
          if (lev(k+1)>ps(i,j,0))  then
            zp(k+1,i,j)=bf%undef
          else
            zp(k+1,i,j)=z(k+1,i,j,0)
          end if
      end if

      end do
      zp(0,i,j)=z(0,i,j,0)
    end do
  end do
 end subroutine

 !-----------------------------------------------
 !Interpolate beneathe surface using hydrostatic
 !-----------------------------------------------
 subroutine hydrostatic(option)
  integer,intent(in)::option
  integer::i,j,k
  real ::dt
  real :: pm,tm !Mean Pressire (hpa) anb temperature (k) at reference level
  real :: p0,t0 !Final Pressure  hPa and temperature fi
  real :: qm
  integer::op2
  
  if (option==2) then
  print *,"Calculaing temperature beneath surface - (Wet adiabatic ratio method)"
  do i=0, nlon-1
    do j=0,nlat-1

      pm=0
      do k=0,nlev-1
        if (lev(k)>ps(i,j,0) ) then

          if (pm==0)  then
	         pm=lev(k-2)/100.0
	         tm=t(k-2,i,j,0)
	      end if

	     t(k,i,j,0)=Saturation_Adiabatic(pm,tm,lev(k)/100.0)

        end if

       end do !k

    end do !j
  end do !i

  end if

   if(option==1) then
     print *,"Calculaing temperature beneath surface - (standard lapserate method)"
     do i=0,nlon-1
     do j=0,nlat-1
     do k=0,nlev-1
        dt=0
        if (lev(k)>ps(i,j,0))  then
          t(k,i,j,0)=temperature_lapse_rate(t(k-1,i,j,0),lev(k-1)/100.0,lev(k)/100.0)!bf%undef
          !t(k+1,i,j,0)=rumida(lev(k)/100.0,lev(k+1)/100.0,t(k,i,j,0))!bf%undef
        end if
      end do
      end do
      end do
   end if




 dz2(:,:,:)=bf%undef
  op2=1
  Print *,"calculating geopotential height: option= ",option
do i=0, nlon-1
    do j=0,nlat-1
     do k=0,nlev-2
      tm=(t(k,i,j,0)+t(k+1,i,j,0))/2.0
      qm=(q(k,i,j,0)+q(k+1,i,j,0))/2.0
      dz2(k+1,i,j)=thickness(lev(k+1),lev(k),tm,qm)
      
      if (op2==1) then
      	tm=(t(k,i,j,0)+t(k+1,i,j,0))/2.0
      	qm=(q(k,i,j,0)+q(k+1,i,j,0))/2.0
      	dz2(k+1,i,j)=thickness(lev(k+1),lev(k),tm,qm)
      
	      if ((dz2(k+1,i,j)-dz(k+1,i,j))>15.0) then
	        zp(k+1,i,j)=zp(k,i,j)-dz2(k+1,i,j)
	      else
	        zp(k+1,i,j)=z(k+1,i,j,0)
	        zp(k+1,i,j)=z(k+1,i,j,0)
          end if
       end if
       
       if (op2==2) then

        if (lev(k+1)>ps(i,j,0))  then
	    tm=(t(k,i,j,0)+t(k+1,i,j,0))/2.0
      	    qm=(q(k,i,j,0)+q(k+1,i,j,0))/2.0
            dz2(k+1,i,j)=thickness(lev(k+1),lev(k),tm,qm)
            zp(k+1,i,j)=zp(k,i,j)-dz2(k+1,i,j)!bf%undef
        else
          zp(k,i,j)=z(k,i,j,0)
          zp(k+1,i,j)=z(k+1,i,j,0)
        end if
      end if
      end do
      zp(0,i,j)=z(0,i,j,0)
    end do
  end do

 end subroutine


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

end program 



!function nf90_inq_dimid(ncid, name, dimid)
!  integer,intent( in) :: ncid
!   character (len = *), intent( in) :: name
!   integer,intent(out) :: dimid
!   integer:: nf90_inq_dimid


!function nf90_inquire(ncid, nDimensions, nVariables, nAttributes,  unlimitedDimId, formatNum)
!  integer,           intent( in) :: ncid
!  integer, optional, intent(out) :: nDimensions, nVariables, nAttributes, unlimitedDimId, formatNum
!  integer                        :: nf90_inquire
!-------------------


!      function nf90_rename_dim(ncid, dimid, name)
!        integer,             intent( in) :: ncid
!        character (len = *), intent( in) :: name
!        integer,             intent( in) :: dimid
!        integer                          :: nf90_rename_dim
!


! NF90_OPEN  Open existing file
! NF90_INQ_DIMID Get dimensions IDSs  <-Numero da variavel
! NF90_INQ_VARID Get Variable Ids
! NF90_GET_ATT Get Attibute values
! NF90_GET_VAR Get Values of Variables
! NF90_CLOSE close netcdf dataset







!use netcdf
!implicit none
!integer              :: ncid, status
!	integer              :: RHVarID                       ! Variable ID
!integer              :: validRangeLength, titleLength ! Attribute lengths
!real, dimension(:), allocatable, &
!                     :: validRange
!character (len = 80) :: title
!...
!status = nf90_open("foo.nc", nf90_nowrite, ncid)
!if (status /= nf90_noerr) call handle_err(status)
!...
!! Find the lengths of the attributes
!status = nf90_inq_varid(ncid, "rh", rhvarid)
!if (status /= nf90_noerr) call handle_err(status)
!status = nf90_inquire_attribute(ncid, rhvarid, "valid_range", &
!                          len = validrangelength)
!if (status /= nf90_noerr) call handle_err(status)
!status = nf90_inquire_attribute(ncid, nf90_global, "title", len = titlelength)
!if (status /= nf90_noerr) call handle_err(status)
!...
!!Allocate space to hold attribute values, check string lengths
!allocate(validrange(validrangelength), stat = status)
!if(status /= 0 .or. len(title) < titlelength)
!  print *, "Not enough space to put attribute values."
!  exit
!end if


! Read the attributes.
!status = nf90_get_att(ncid, rhvarid, "valid_range", validrange)
!if (status /= nf90_noerr) call handle_err(status)
!status = nf90_get_att(ncid, nf90_global, "title", title)
!if (status /= nf90_noerr) call handle_err(status)


