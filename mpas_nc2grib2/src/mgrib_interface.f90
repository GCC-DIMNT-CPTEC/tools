!------------------------------------------------------------------------------
! mgrib_interface
! Module to decoded grib data using EcCodes Library and stract specific parameter
! to be use by a FORTRAN program (energetica program )
!
! Author: Sergio Henrique Soares Ferreira (Fev 2022)
!-----------------------------------------------------------------------------
! References About EcCodes
! https://confluence.ecmwf.int/display/UDOC/GRIB+Keys+-+ecCodes+GRIB+FAQ
! https://sites.ecmwf.int/docs/eccodes/classeccodes.html
! https://confluence.ecmwf.int/display/ECC/grib_set_bitmap
!-----------------------------------------------------------------------------
! 25/02/2022 SHSF Preliminar version
! 26/10/2025 SHSF grib2 codification has been done. The bitmap codification has been included


module mgrib_interface
 use eccodes
 use msort
 implicit none
	private
	public open_grib
	public read_grib2
	public read_grib1
	public klevel
	public grib_parameter_id
	public grib_interface_def
	public openw_grib
	public write_grib2
	public close_grib
	public set_bitmap
	
 type grib_parameter_id
   integer           ::Template
   integer           ::Discipline
   integer           ::pCat
   integer           ::pNum
   integer           ::tflevel
   integer           ::time_interval !Time Interval 
   real              ::sFactor_FFS   !scaleFactorOfFirstFixedSurface
   real              ::sValue_FFS    !scaledValueOfFirstFixedSurface
   character(len=8)  ::cfVarName
   character(len=32) ::ncVarName
   character(len=250)::VarName
   real              ::missing
 end type

 type grib_interface_def
   integer::edition
   integer::productionStatus
   integer::gridType              ! 1-regular_ll, 2-regular_gg
   character(len=20)::packingType !"grid_ccsds, etc"
   real,pointer  ::lev(:)         !Vertical levels
   real,pointer  ::lat(:)         !Latitudes
   real,pointer  ::lon(:)         !Longitues
   integer       ::NI
   integer       ::NJ
   integer       ::NK
   integer       ::idate
   integer       ::itime
   real          ::ilat
   real          ::ilon
   real          ::dlat
   real          ::dlon
 end type

  integer            ::tablesVersion_default
  integer            ::rfile,igrib,out1
  integer            ::iret
  integer            ::n
  integer            ::status
  integer            ::nb_values
  character(len=1024)::infile
  logical            ::jscan_inversion  ! If true, invert longitude scan to be from -90 to 90
  real,dimension(300)::klevel
  integer            ::kmax
  integer            ::editionNumber
  integer            ::NI,NJ,NK

  character(len=20)::packingType
  logical                              :: bitmapPresent=.false. !If .true. Bitmap and missing values will be included for write
  integer                              :: bitMapIndicator=255
  real                                 :: current_missing
 contains
 
!-----------------------------
! open_grib to write a grib fi
!------------------------------
 subroutine openw_grib(grib_file,grib_def,tablesVersion)
	character(len=*),        intent(in)::grib_file
	type(grib_interface_def),intent(in)::grib_def
	integer,                 intent(in)::tablesVersion

	real::ilat,ilon
	real::dlat,dlon

	character(len=1024)::outfile
	real::flat,flon
	tablesVersion_default=tablesVersion
    NI=grib_def%NI
    NJ=grib_def%NJ
    NK=grib_def%NK
    ilat=grib_def%ilat
    ilon=grib_def%ilon
	editionNumber=grib_def%edition
	flat=ilat+(Nj-1)*grib_def%dlat
	flon=ilon+(Ni-1)*grib_def%dlon
	dlat=grib_def%dlat
	dlon=grib_def%dlon
	packingType=grib_def%packingType

	write(outfile,'(a,".grib",i1)')trim(grib_file),editionNumber
!	print *,":MGRIB_INTERFACE: output filename = ",trim(outfile)
	!call codes_grib_open_file(out1,grib_file,'w')
	call codes_open_file(out1,outfile,'w')
	!call codes_grib_new_from_samples(igrib, "./sh_sfc_grib2")   !Example Sherical Hamonics - sfc grib 2
	call codes_grib_new_from_samples(igrib,"./regular_ll_sfc_grib2")
	call codes_set(igrib, "tablesVersion", tablesVersion_default,status)
	call codes_set(igrib,'editionNumber',grib_def%edition)

	!----------
	!section 1
	!---------
	call codes_set(igrib,'centre',46)
	!subcenter
	!localTableVersion
	!significanceOfReferenceTime
	call codes_set(igrib,'date',grib_def%idate)
	call codes_set(igrib,'time',grib_def%itime)
	call codes_set(igrib,'productionStatusOfProcessedData',grib_def%productionStatus)
	!productionStatusOfProcessedData
	!typeOfProcessedData

	!---------
	!section 3
	!---------
	!source_Of_gridDefinition
	!NumberOfDataPoints
	!NumberOfOctectsForNumberOfDataPoints
	!12-interpretationOfNumberOfPoints
	!13-14 _GridDefinitionTemplateNumber
	!15    - shapeOfTheEarth

	call codes_set(igrib,"Ni",Ni) !31-34
	call codes_set(igrib,"Nj",Nj) !35-36
	!39-42 BasicAngleOfTheinitialProcutionDomain
	call codes_set(igrib,"latitudeOfFirstGridPointInDegrees",flat) !swap (ilat,flat)
	call codes_set(igrib,"longitudeOfFirstGridPointInDegrees",ilon)
	!55 - ResolutionAndComponentFlags
	call codes_set(igrib,"latitudeOfLastGridPointInDegrees",ilat) !swap (flat,ilat)
	call codes_set(igrib,"longitudeOfLastGridPointInDegrees",flon)
	call codes_set(igrib,"iDirectionIncrementInDegrees",dlon)
	call codes_set(igrib,"jDirectionIncrementInDegrees",dlat)
    !72 - Scaning Mode
	nb_values=ni*nj


 end subroutine

!---------------------------------------------------------
!
!--------------------------------------------------------
 subroutine write_grib2(parm_id,par,level,step)
   type(grib_parameter_id),intent(in)::parm_id
   real,dimension(:,:),  intent(in)::par
   real,                 intent(in)::level
   integer,              intent(in)::step

!{ Local variables
   real,dimension(:),allocatable ::values
   integer:: SFactor_FFS
   integer:: SValue_FFS
   integer:: tlevel
   integer:: NB
   integer:: i,j,k
   character(len=20)::stepRange
   integer          ::initStep, endStep
!}

    call codes_set(igrib,"tablesVersion", tablesVersion_default,status)
	call codes_set(igrib,"packingType",packingType)
	call codes_set(igrib,"productDefinitionTemplateNumber",parm_id%Template)
	call codes_set(igrib,"discipline",parm_id%Discipline)
		!call codes_get(igrib,"editionNumber",editionNumber,status)
		!Section 1
		!call codes_set(igrib,"data",date,status)
		!call codes_get(igrib,"dataTime",Time,status)
		!?

		call codes_set(igrib,"typeOfLevel",parm_id%tfLevel)
		!call codes_get(igrib,"level",level)
		!Section 3
		call codes_set(igrib,"parameterCategory",parm_id%pCat)
		call codes_set(igrib,"parameterNumber",parm_id%PNum)
		!Section 4
		if (step>0) then
			call codes_set(igrib,"typeOfGeneratingProcess",2)
		end if 
		
	!------------------------
	! Step range definitions
	!------------------------
	!{
		if (parm_id%time_interval>0) then
			endStep=step
			initStep=endstep-parm_id%time_interval
			if (initStep<0) initStep=0 
				write(stepRange,'(i3,"-",i3)')initStep,endStep
				call codes_set(igrib,"stepRange",stepRange)
				call codes_set(igrib,"endStep",endStep)
			elseif (parm_id%time_interval==0) then
				call codes_set(igrib,"stepRange",step)
			else   ! Accumulation since the start time
			endStep=step 
			write(stepRange,'("0-",i3)')endStep
			call codes_set(igrib,"stepRange",stepRange)
			call codes_set(igrib,"endStep",endStep)
		endif 	
		
	!*** step type = "accuml, ave, instant
	!*** codes_set__nc(Number of bit..... )
	!}

	!# Code table 4.5: Fixed surface types and units
	!print *,"cfVarName=",parm_id%cfVarname
	!{
		tlevel=parm_id%tflevel
		if (parm_id%tflevel==100) then
		   SFACTOR_FFS=0
		   SValue_FFS=level
		elseif (parm_id%tflevel==103) then
			SFactor_FFS=parm_id%sFactor_FFS
			SValue_FFS=parm_id%sValue_FFS
		else
			SFactor_FFS=0
			SValue_FFS=0
		end if
		NB=Ni*Nj
		call codes_set(igrib,"typeOfFirstFixedSurface",tlevel)
		call codes_set(igrib,"scaleFactorOfFirstFixedSurface",SFactor_FFS)
		call codes_set(igrib,"scaledValueOfFirstFixedSurface",SValue_FFS)
		!call codes_set_size(igrib,"values",nb_values)
	!}
	!------------------------------------------------------------------------------------
	! Enable Bitmap and define the missing values for the current variable / grib message
	!------------------------------------------------------------------------------------
	!{
		if (bitmapPresent) then
			call codes_set(igrib, 'missingValue',current_missing)
			call codes_set(igrib, 'bitmapPresent', 1)
		end if
	!}

	!--------------
	! Write values
	!--------------
	!{
		allocate(values(NB))

	    nb=0
		do j=Nj,1,-1
			do i=1,Ni
				nb=nb+1
				values(nb)=par(i,j)
			end do
		end do
	call codes_set(igrib,"values",values)
	deallocate(values)
	call codes_write(igrib,out1)
	!}


 end subroutine

 subroutine close_grib
	call codes_release(igrib)
	call codes_close_file(out1)

end subroutine

!------------------------------------------------------------------------------
! subroutine |set_bitmap                                               | SHSF |
!-----------------------------------------------------------------------------
! This subroutine detects if missing values are presents in the provide data
! Case yes then set bitmapPresent=.true. as well as current_missing value
! to include the condification of bitmap in grib2
!------------------------------------------------------------------------------
! Note: bitmap(i,j) is not been used because EcCodes performs it
! internally for enconde. It is here just for future uses
!-----------------------------------------------------------------------------
subroutine set_bitmap(data,missing,bitmap)
 !{ Interface Variables
   real,dimension(:,:),intent(in)::data
   real,               intent(in)::missing
   logical,dimension(:,:),intent(out)::bitmap
 !}
 !{Local variables
   integer ::i,j
  !}
  bitmapPresent=.false.
  bitMapIndicator=255
  current_missing=missing
  do i=1,Ni
     do j=1,Nj
        if (data(i,j)==missing) then
			bitmap(i,j)=.false.
			bitmapPresent=.true.
		else
			bitmap(i,j)=.true.
		end if
	end do
  end do
  if (.not.bitmapPresent) bitmap(:,:)=.false.

  !The bitmap size is the number of points in the grid
!(numberOfPoints)
!0 -> value is missing
!1 -> value is present
!• When encoding, you can use the key missingValue to tell
!the library where data is missing
!• By default this is 9999 but it can be changed by the user
!e.g. a value out of the range of normal data
!• You must also set the key bitmapPresent to 1
!• When the library encounters a value equal to the missing
!value in the data array, it will set the bitmap entry to 0 for
!that grid point
!• When decoding, you can directly query the bitmap to
!discover missing data values
!14EUROPEAN CENTRE FOR MEDIUM-RANGE WEATHER FORECASTS October 29, 2014
!Bitmap: Practicals
!cd $SCRATCH
!cd grib_packing/bitmap
!1.You have a GRIB start.grib with 4 messages. Set
!1.bitsPerValue=8, bitmapPresent=0 in the first message
!2.bitsPerValue=16, bitmapPresent=0 in the second message
!3.bitsPerValue=24, bitmapPresent=0 in the third message
!4.bitsPerValue=8, bitmapPresent=1 in the fourth message
!2. Set values = {0.2, 0.4, 0.6, 0.7, 9999}
!3. Print the values
!(Hint: you can use grib_filter)


end subroutine
!-----------------------------
! open_grib to read a grib fie
!------------------------------
 subroutine open_grib(grib_file,gid,Ni,Nj,Nk,date,time,ifct,ilat,ilon,dlat,dlon,missingValue)
  character(len=*),intent(in)::grib_file
  type(grib_interface_def),intent(out)::gid  ! Interface_definition
  integer,         intent(out)::Ni,Nj,Nk
  integer,         intent(out)::date !dataDate 
  integer,         intent(out)::time !dataTime = 0;	
  integer,         intent(out)::ifct
  real,            intent(out)::ilat,ilon
  real,            intent(out)::dlat,dlon
  real,            intent(out)::missingValue
  
  character(len=20)::gridtype
  real    ::flat,flon,dlat2,dlon2,flat2,flon2
  real    ::aux
  integer ::iret,pCat,pNum
  real    :: SFFFS,SVFFS
  integer :: tlevel
  integer :: k,i,j
  logical :: newk
  integer ::GDSPresent
  real,dimension(:),allocatable ::values,lats,lons

	kmax=0
	infile=grib_file
	call codes_open_file(rfile,infile,'r')
	call codes_grib_new_from_file(rfile,igrib,iret)
	call codes_get(igrib,"editionNumber",editionNumber,status)
	gid%edition=editionNumber
	call codes_set(igrib, "tablesVersio", 28,status)
	print *,":MGRIB_INTERFACE:OPEN_GRIB: editionNumber=",editionNumber

		!Section 2
		call codes_get(igrib,"dataDate",date,status)
		call codes_get(igrib,"dataTime",Time,status)
		call codes_get(igrib,"stepRange",ifct,status)
		!Section 3
		call codes_get(igrib,"gridType",gridType)
		if (trim(gridType)=="regular_ll") gid%gridtype=1
		if (trim(gridType)=="regular_gg") gid%gridtype=2
		write(*,'(1x,":MGRIB_INTERFACE:gridType=",i2,1x,a)')gid%gridtype,trim(gridType)

		call codes_get(igrib,"longitudeOfFirstGridPointInDegrees",ilon)
		call codes_get(igrib,"longitudeOfLastGridPointInDegrees",flon)
		call codes_get(igrib,"iDirectionIncrementInDegrees",dlon)
		call codes_get(igrib,"Ni",Ni)
		call codes_get(igrib,"Nj",Nj)
		call codes_get(igrib,"missingValue",missingValue)
		if (gid%gridtype==1) then
			call codes_get(igrib,"latitudeOfFirstGridPointInDegrees",ilat)
			call codes_get(igrib,"latitudeOfLastGridPointInDegrees",flat)
			call codes_get(igrib,"jDirectionIncrementInDegrees",dlat)
		elseif(gid%gridtype==2) then
                    
			! Grib1 data format
			!Section 0: Indicator Section.
			!Section 1: Product Definition Section.
			!Section 2: Grid Description Section (GDS) - Optional
			!Section 3: Bit Map Section - Optional.
			!Section 4: Binary Data Section.
			!Section 5: '7777' - ASCII Characters indicating end of GRID record.

			call codes_get_size(igrib,"values",nb_values)

			call codes_get(igrib,"GDSPresent",GDSPresent)
			allocate (lats(0:nb_values-1),lons(0:nb_values-1),values(0:nb_values-1))
			allocate (gid%lat(nj))

			call codes_grib_get_data(igrib,lats,lons,values)
		
			j=0
			aux=0
			do i=0,nb_values-1,1
				if (lats(i)/=aux) then
					j=j+1
					aux=lats(i)
					gid%lat(j)=aux
				end if
			end do
			call sortr4(gid%lat,Nj)  ! Note: It shound't be necessary - Check it later
			flat=gid%lat(NJ)
			ilat=gid%lat(1)
			deallocate (lats,lons,values)
		end if



	if (editionNumber==2) then 	
	!----------------------------------
	! Only grib_edition=2 (jscan)
	!----------------------------------
	if (ilat>flat) then
			aux=flat
			flat=ilat
			ilat=aux
			jscan_inversion=.true.
		else
			jscan_inversion=.false.
		end if
		!-----------------
		! Vertical levels
		!----------------
		iret=0

		do while(iret/=CODES_END_OF_FILE) 
			call codes_grib_new_from_file(rfile,igrib,iret)	
			if (iret/=CODES_END_OF_FILE) then 
				call codes_get(igrib,"scaleFactorOfFirstFixedSurface",sFFFS)
				call codes_get(igrib,"scaledValueOfFirstFixedSurface",sVFFS)
				call codes_get(igrib,"typeOfFirstFixedSurface",tlevel)
				call codes_get(igrib,"parameterCategory",pCat)
				call codes_get(igrib,"parameterNumber",pNum)
				!print *,"kmax, sFFFS, sVFFS,tlevel,pCat,pNum=",kmax,sFFFS,sVFFs,tlevel,pCat,pNum

				if (pCat==3) then
				if ((sFFFs==0).and.(sVFFs/=0).and.(tlevel==100)) then 
					newk=.true.
					do k=1,kmax
						if (klevel(k)==sVFFs) then 
							newk=.false.
							exit
						end if 
					end do
					if (newk) then 
						kmax=kmax+1
						klevel(kmax)=sVFFs
						!print *,"sFFFS, sVFFS,tlevel,pCat,pNum=",sFFFS,sVFFs,tlevel,pCat,pNum
					end if 
				end if
				end if
			end if 
		end do
		Nk=kmax
		!----------------------
		! sort vertical levels
		!----------------------
	 
		 call sortr4(klevel,kmax)
		 do k=1,int(kmax/2)
			aux=klevel(k)
			klevel(k)=klevel(kmax-k+1)
			klevel(kmax-k+1)=aux
		 end do
		 
	 else
	 !----------------------------------
	 ! Only: grib_edition=1
	 !----------------------------------
		if (ilat>flat) then
			aux=flat
			flat=ilat
			ilat=aux
			jscan_inversion=.true.
		else
			jscan_inversion=.false.
		end if
		 iret=0
	 
		 do while(iret/=CODES_END_OF_FILE) 
			call codes_grib_new_from_file(rfile,igrib,iret)	
			if (iret/=CODES_END_OF_FILE) then 
				call codes_get(igrib,"level",sVFFS)
				call codes_get(igrib,"indicatorOfTypeOfLevel",tlevel)
				call codes_get(igrib,"indicatorOfParameter",pNum)
				if ((sVFFs/=0).and.(tlevel==100)) then
					sVFFs=sVFFs*100.0
					newk=.true.
					do k=1,kmax
						if (klevel(k)==sVFFs) then 
							newk=.false.
							exit
						end if 
					end do
					if (newk) then 
						kmax=kmax+1
						klevel(kmax)=sVFFs
					end if 
				end if
			end if 
		 end do
	 
		!----------------------
		! sort vertical levels
		!---------------------- 
		Nk=kmax
		call sortr4(klevel,kmax)
	
		do k=1,int(kmax/2)
			aux=klevel(k)
			klevel(k)=klevel(kmax-k+1)
			klevel(kmax-k+1)=aux
		end do
	 
	 end if
	 ! pf=pi+inc*(n-1)
	 flat2=ilat+dlat*float(nj-1)
	 flon2=ilon+dlon*float(ni-1)
	 if (flat2<flat)  then 
		ilat=ilat+dlat/2.0
		flat2=ilat+dlat*float(nj-1) 
	 end if 
	 print *,":MGRIB_INTERFACE: Lat1,Lat2,dlat =",ilat,flat,dlat
	 print *,":MGRIB_INTERFACE: Lon1,Lon2,dlon =",ilon,flon,dlon
	  
	 call codes_release(igrib)
	 call codes_close_file(rfile)

 end subroutine

 
!------------------------------------------------------------------------------
! read_grib
!------------------------------------------------------------------------------
 subroutine read_grib2(grib_file,parm_id,par)
  character(len=*),                    intent(in)::grib_file
  type(grib_parameter_id),dimension(:),intent(in)::parm_id
  real,dimension(:,:,:,:),            intent(out)::par

 integer :: np
 integer ::nmessages
 
! GRIB KEYNAMES
  integer           ::typeOfLevel
  integer           ::Level

  integer           :: discipline    !0=Meteorological products (grib2/tables/2/0.0.table)  
  integer           :: editionNumber
  integer           :: centre 
  integer           :: subCentre
  integer           :: significanceOfReferenceTime !1= Start of forecast (grib2/tables/2/1.2.table)  
  integer           ::date !dataDate
  integer           ::time !dataTime = 0;
  integer           :: ifct
 
  !# Operational products (grib2/tables/2/1.3.table)  
  !productionStatusOfProcessedData = 0;
  !# Forecast products (grib2/tables/2/1.4.table)  
  !typeOfProcessedData = 1;
  integer           :: numberOfDataPoints
  !# There is no appended list (grib2/tables/2/3.11.table)  
  !interpretationOfNumberOfPoints = 0;

  integer :: gridDefinitionTemplateNumber != 0 # Latitude/longitude. Also called equidistant cylindrical, or Plate Carree (grib2/tables/2/3.1.table)  
  integer :: shapeOfTheEarth  != 6 # Earth assumed spherical with radius of 6,371,229.0 m (grib2/tables/2/3.2.table)  
  integer :: Ni,i
  integer :: Nj,j,k
  integer :: nb
  integer :: parameterCategory  ! # 3 = Mass, 1 PRNMM;subroutine getlevel(vlevel)
  integer :: parameterNumber 
  integer :: sFFFs, sVFFs,tlevel
  integer :: v
  real,dimension(:),allocatable ::values
  
  
 !-----------------------------
 ! OPEN GRIB FILE with ECCODES
 !----------------------------
 print *,":MGRIB_INTERFACE:READ_GRIB2:Filename=",trim(infile)
 np=size(parm_id)
 infile=grib_file
 call codes_open_file(rfile,infile,'r')
 iret=0
 n=0
 nmessages=0
 do while(iret/=CODES_END_OF_FILE) 
	 call codes_grib_new_from_file(rfile,igrib,iret)
	 if (iret/=CODES_END_OF_FILE) then 
		!Section 0
		call codes_get(igrib,"discipline",discipline,status)
		call codes_get(igrib,"editionNumber",editionNumber,status)
		!Section 1
		call codes_get(igrib,"dataDate",date,status)
		call codes_get(igrib,"dataTime",Time,status)
		call codes_get(igrib,"stepRange",ifct,status)
		!?
		call codes_get(igrib,"typeOfLevel",typeOfLevel)
		call codes_get(igrib,"level",level)
		!Section 3
		call codes_get(igrib,"Ni",Ni)
		call codes_get(igrib,"Nj",Nj)
		call codes_get(igrib,"parameterCategory",parameterCategory)
		call codes_get(igrib,"parameterNumber",parameterNumber)
		!Section 4

		call codes_get(igrib,"typeOfFirstFixedSurface",tlevel)
		call codes_get(igrib,"scaleFactorOfFirstFixedSurface",sFFFS)
		call codes_get(igrib,"scaledValueOfFirstFixedSurface",sVFFS)
		call codes_get_size(igrib,"values",nb_values)
		
		allocate(values(nb_values))
		n=n+1
		
		v=get_parm_id(EditionNumber,Discipline,parameterCategory,parameterNumber,tlevel,parm_id,np);
		if (v>0) then  
			call codes_get(igrib,"values",values)
			k=getlevelid(sFFFs,sVFFs,tlevel)
			!print *,"k,Cat,Num=",k,parameterCategory,parameterNumber
			if (k>0) then 
			nmessages=nmessages+1	
			if (jscan_inversion) then 
			!----------------------------------------------------
			! Inver latitude scan to direction from south to north
			!-----------------------------------------------------
			
			
			nb=0
			do j=Nj,1,-1
				do i=1,Ni
					nb=nb+1
					par(i,j,k,v)=values(nb)
				end do
			end do
			
			
			else
			!----------------------------------------------------
			! normal latitude scan 
			!-----------------------------------------------------
			
			nb=0
			do j=1,Nj
				do i=1,Ni
					nb=nb+1
					par(i,j,k,v)=values(nb)
				end do
			end do
			end if
			
			!print *,"Cat,Num,par(1,Nj,k),values(1),k,klevel=",parameterCategory,parameterNumber,par(1,Nj,k,v),values(1),k,klevel(k)	
					
		end if
		end if
		deallocate(values)
		
 	 end if
 end do
 print *,":MGRIB_INTERFACE:read_grib2: Total Number of messages     = ",n
 print *,":MGRIB_INTERFACE:read_grib2: Number of selected nmessages = ",nmessages
 call codes_release(igrib)
 call codes_close_file(rfile)
 end subroutine read_grib2

 !------------------------------------------------------------------------------
! read_grib1
!------------------------------------------------------------------------------
 subroutine read_grib1(grib_file,parm_id,par)
  character(len=*),                    intent(in)::grib_file
  type(grib_parameter_id),dimension(:),intent(in)::parm_id
  real,dimension(:,:,:,:),            intent(out)::par

 integer :: np
 integer ::nmessages
 
! GRIB KEYNAMES
  integer            ::typeOfLevel
  integer            ::Level

  integer           :: discipline    !0=Meteorological products (grib2/tables/2/0.0.table)  
  integer           :: editionNumber
  integer           :: centre 
  integer           :: subCentre
  integer           :: significanceOfReferenceTime !1= Start of forecast (grib2/tables/2/1.2.table)  
  integer            ::date !dataDate 
  integer            ::time !dataTime = 0;
 
  !# Operational products (grib2/tables/2/1.3.table)  
  !productionStatusOfProcessedData = 0;
  !# Forecast products (grib2/tables/2/1.4.table)  
  !typeOfProcessedData = 1;
  integer           :: numberOfDataPoints
  !# There is no appended list (grib2/tables/2/3.11.table)  
  !interpretationOfNumberOfPoints = 0;

  integer :: gridDefinitionTemplateNumber != 0 # Latitude/longitude. Also called equidistant cylindrical, or Plate Carree (grib2/tables/2/3.1.table)  
  integer :: shapeOfTheEarth  != 6 # Earth assumed spherical with radius of 6,371,229.0 m (grib2/tables/2/3.2.table)  
  integer :: Ni,i
  integer :: Nj,j,k
  integer ::nb
  integer :: parameterCategory  ! # 3 = Mass, 1 PRNMM;subroutine getlevel(vlevel)
  integer :: parameterNumber 
  integer ::sFFFs, sVFFs,tlevel
  integer ::v

  real,dimension(:),allocatable ::values
  
  
 !----------------------------
 ! OPEN GRIB FILE with ECCODES
 !----------------------------
 print *,":MGRIB_INTERFACE:READ_GRIB1:Filename=",trim(infile)
 np=size(parm_id)
 infile=grib_file
 call codes_open_file(rfile,infile,'r')
 iret=0
 n=0
 nmessages=0
 do while(iret/=CODES_END_OF_FILE) 
	 call codes_grib_new_from_file(rfile,igrib,iret)
	 if (iret/=CODES_END_OF_FILE) then 
		!Section 0
		call codes_get(igrib,"discipline",discipline,status)
		call codes_get(igrib,"editionNumber",editionNumber,status)
		!Section 1
		call codes_get(igrib,"dataDate",date,status)
		call codes_get(igrib,"dataTime",Time,status)
		!?
			
		call codes_get(igrib,"typeOfLevel",typeOfLevel)
		call codes_get(igrib,"level",level)
		
		call codes_get(igrib,"Ni",Ni)
		call codes_get(igrib,"Nj",Nj)
		call codes_get(igrib,"indicatorOfParameter",parameterNumber)
		call codes_get(igrib,"indicatorOfTypeOfLevel",tlevel)
		call codes_get(igrib,"level",sVFFS)
		sVFFS=sVFFS*100
		call codes_get_size(igrib,"values",nb_values)
		
		allocate(values(nb_values))
		n=n+1
		
		v=get_parm_id(EditionNumber,0,0,parameterNumber,tlevel,parm_id,np);
		if (v>0) then  
			call codes_get(igrib,"values",values)
			k=getlevelid1(sVFFs,tlevel)
			!print *,"k,Cat,Num=",k,parameterCategory,parameterNumber
			if (k>0) then 
			nmessages=nmessages+1	
			if (jscan_inversion) then 
			!----------------------------------------------------
			! Inver latitude scan to direction from south to north
			!-----------------------------------------------------
			
			
			nb=0
			do j=Nj,1,-1
				do i=1,Ni
					nb=nb+1
					par(i,j,k,v)=values(nb)
				end do
			end do
			
			
			else
			!----------------------------------------------------
			! normal latitude scan 
			!-----------------------------------------------------
			
			nb=0
			do j=1,Nj
				do i=1,Ni
					nb=nb+1
					par(i,j,k,v)=values(nb)
				end do
			end do
			end if
			
			!print *,"Cat,Num,par(1,Nj,k),values(1),k,klevel=",parameterCategory,parameterNumber,par(1,Nj,k,v),values(1),k,klevel(k)	
					
		end if
		end if
		deallocate(values)
		
 	 end if
 end do
 print *,":MGRIB_INTERFACE:read_grib1: Total Number of messages     = ",n
 print *,":MGRIB_INTERFACE:read_grib1: Number of selected nmessages = ",nmessages
 call codes_release(igrib)
 call codes_close_file(rfile)
 end subroutine

 !-----------------------------------------------------
 !
 !-----------------------------------------------------
 function getlevelid(sFFFs,sVFFs,tlevel); integer::getlevelid
	integer,intent(in)::sFFFs,SVFFs,tlevel
	integer::k
	getlevelid=0
	
	if (tlevel==100) then 
	
		if (sFFFs==0) then 
			do k=1,kmax
				if (klevel(k)==sVFFs) then 
					getlevelid=k
					exit
				endif
			end do
		end if 
	elseif (tlevel==101) then  ! Pressure redulced to mean sea level 
		getlevelid=1
	elseif (tlevel==1) then  ! Ground or water surface  (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table) 
		print *,"Ground or water surface  (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)"
		getlevelid=1
	end if 
 end function
 
 !-----------------------------------------------------
 !
 !-----------------------------------------------------
 function getlevelid1(sVFFs,tlevel); integer::getlevelid1
	integer,intent(in)::SVFFs,tlevel
	integer::k
	getlevelid1=0
	
	if (tlevel==100) then 
		do k=1,kmax
			if (klevel(k)==sVFFs) then 
				
				getlevelid1=k
				exit
			endif
		end do
	elseif (tlevel==102) then  ! Pressure redulced to mean sea level 
		getlevelid1=1
	elseif (tlevel==1) then  ! 1=surface (of the Earth, which includes sea surface)  (grib1/3.table)   
		print *,"surface (of the Earth, which includes sea surface)  (grib1/3.table)"
		getlevelid1=1
	end if 
 end function
 
 !
 !
 !
  function get_parm_id(Edition,Discipline,pCat,pNum,tflevel,parm_id,np); integer::get_parm_id
   integer,intent(in)::Edition
   integer,intent(in)::Discipline
   integer,intent(in)::pCat,pNum,tflevel
   type(grib_parameter_id),dimension(:),intent(in)::parm_id
   integer,intent(in)::np
   
   integer ::n
   get_parm_id=0
   if (edition==2) then 
	do n=1,np
	if (parm_id(n)%Discipline==Discipline) then 
		if (parm_id(n)%pCat==pcat) then 
			if (parm_id(n)%pNum==pNum) then
				if (parm_id(n)%tflevel==tflevel) then  
					get_parm_id=n
					exit
				end if
			end if
		end if
	end if 
	end do
  else
	do n=1,np
		if (parm_id(n)%pNum==pNum) then
			!if (parm_id(n)%tflevel==tflevel) then  
				get_parm_id=n
				exit
			!end if
		end if
	 
	end do
  
  end if
  end function
 
  
end  module

!  typeOfFirstFixedSurface 
!   1  # Ground or water surface  (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table) 
!        Visibility,wind speed, wind gust, gh
!  100 # Isobaric surface  (Pa)  (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)   
!  101 # Mean sea level (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)  
!  105 # Hybrid level (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)  
!  220;# Planetary boundary layer (grib2/tables/2/4.5.table , grib2/tables/local/kwbc/1/4.5.table)
