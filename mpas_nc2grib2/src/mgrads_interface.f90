module  mgrads_interface

use mgrads
implicit none
public
real, dimension(:,:,:),allocatable :: xyz
type(bindef)::bf


contains

!---------------------------------------------------------------------------+
! conv_zxy2xyz                                                              |
!---------------------------------------------------------------------------+
!Copy 3D matriz zxy to xyz as well change the initial index fron 0 to 1     |
!                                                                           |
!(Copia Matrix 3D zxy para xyz assm como muda o índice inicial de 0 para 1) |
!---------------------------------------------------------------------------+
subroutine conv_zxy2xyz (zxy)
    real,dimension(:,:,:),intent(in)::zxy
    integer::xdim,ydim,zdim
    integer ::x,y,z,h

        xdim=size(zxy,2)
        ydim=size(zxy,3)
        zdim=size(zxy,1)
	if(.not.allocated(xyz)) allocate (xyz(1:xdim,1:ydim,1:zdim))
   !Invert vertical levels
    do z=1,zdim
        h=zdim-z+1
    	xyz(:,:,h)=zxy(z,:,:)
    end do
    
end subroutine

!---------------------------------------------------------------------------+
! conv_xy2xyz                                                               |
!---------------------------------------------------------------------------+
!Copy 2D matriz xy to xyz as well change the initial index fron 0 to 1      |
!                                                                           |
!(Copia Matrix 2D xy para xyz assm como muda o índice inicial de 0 para 1)  |
!---------------------------------------------------------------------------+
subroutine conv_xy2xyz (xy)
    real,dimension(:,:),intent(in)::xy
    integer::xdim,ydim,zdim
    integer ::x,y,z,h

        xdim=size(xy,1)
        ydim=size(xy,2)
        zdim=bf%kmax
    if(.not.allocated(xyz)) allocate (xyz(1:xdim,1:ydim,1:zdim))
	xyz(:,:,:)=bf%undef
    xyz(:,:,1)=xy(:,:)

end subroutine

!---------------------------
!  Create/open grads file
!---------------------------

subroutine create_grfile(outfile,xdim,ydim,zdim,nvarmax)
    character(len=*),intent(in)::outfile
    integer,intent(in)::xdim,ydim,zdim,nvarmax
     
    call OPENW_mgrads(bf,outfile,xdim,ydim,zdim,nvarmax)
    bf%imax=xdim
    bf%jmax=ydim
    bf%kmax=zdim
    bf%tmax=1
    allocate(bf%varlevs(1:zdim))
    allocate(bf%lat(1:ydim))
    allocate(bf%lon(1:xdim))
    
     
    !character(len=8),POINTER  ::code(:)   !Codigo/sigla da variavel 
    !character(len=255),POINTER::name(:)   !Nome da variavel 
    !character(len=1024)       ::ctlname   !Nome do arquivo descritor (CTL) 
   ! character(len=1024)       ::binname   !Nome do arquivo de dados binarios (BIN)
   ! character(len=80)         ::tlable           
   ! nteger,pointer           ::varlevs(:)! Numero de niveis verticais de cada variavel
   ! integer                   ::imax      !Numero de pontos de grade na direcao x
   ! integer                   ::jmax      !Numero de pontos de grade na direcao y
   ! integer                   ::kmax      !Numero de niveis verticais (maximo)  
   ! integer                   ::tmax      !Numero de passos de tempo 
   ! integer                   ::fct       !Forecast period identification  
   !integer                   ::nvars     !Numero de variaveis 
   !integer                   ::nvarsmax  !Numero maximo de variaveis (para alocacao) 
   ! real                      ::dlat      !Tamanho do elemento de grade na direcao das latitudes
   !real                      ::dlon      !Tamanho do elemento de grade na direcao das longitude
   !real,pointer              ::lev(:)    !Valores dos Niveis verticais 
   !real,pointer              ::lat(:)    !Valores de latitude
   !real,pointer              ::lon(:)    !Valores de longitude
   !real*8                    ::time_init !Data juliana inicial em dias e decimos de dias 
   !real*8                    ::time_step !Passo de tempo em dias e decimos de dias 
   !real                      ::undef     !Valor indefinido
   !integer*8                 ::nregtime  !Numero de registros por passo de tempo 
   !integer*8                 ::NREG      !Numero total de registros
   !integer*8                 ::recsize   !Tamanho dos resgistros (bytes)
   !integer*8                 ::lof        !Tamanho do arquivo (bytes)
   !integer                   ::horizontal ! Identificacao da tipo de coordenada  horizontal
                                          ! 1 = Lat e lon em graus 2 = Distancia em metros
   !integer                   ::vertical   !Identificacao da variavel da coordenada  vertical
   !integer                   ::mtype         !Tipo de modelo: 0=Modelo global 1 = modelo regional

   !logical                   ::vcoords_from_ctl    !Se .true. = Coordenadas verticais provenientes do ctl 
   !logical                   ::xrev                !Indica se o eixo x esta em ordem reversa
   !logical                   ::yrev                !Indica se o eixo y esta em ordem reversa	
   !logical                   ::optemplate          !Se .true. indica template 
   !character(len=80)         ::title               !Titulo

end subroutine

subroutine grfile_defdims(lat,lon,lev,jdate,time_step)
   real,dimension(:),intent(in)::lat,lon,lev
   real*8,           intent(in)::jdate
   integer,          intent(in)::time_step ! Hours
   integer::k,l
   
   bf%varlevs(1:bf%kmax)=lev(1:bf%kmax)
   bf%lat(1:bf%jmax)=lat(1:bf%jmax)
   bf%lon(1:bf%imax)=lon(1:bf%imax)
   bf%dlon=bf%lon(2)-bf%lon(1)
   bf%dlat=bf%lat(2)-bf%lat(1)
   bf%time_init=jdate
   bf%time_step=time_step/24.0
   
   !Invert vertical levels
   do k=1,bf%kmax
   	l=bf%kmax-k+1
	!print *,k,lev(k)/100.0
	bf%lev(l)=lev(k)/100.0
  end do    
  
   !print *,"kmax=",bf%kmax
   !print *,"lev=",lev(1),lev(bf%kmax)
   !print *,"lat=",lat(1),lat(bf%jmax)
   !print *,"lon=",lon(1),lon(bf%imax)
   !print *,trim(bf%ctlname)
   !print *,trim(bf%binname)

end subroutine


subroutine grfile_defvars

  call addvar_mgrads(bf,"prmsl",0,"Pressure reduced to MSL     (Pa)")
  call addvar_mgrads(bf,"sp"   ,0,"Surface Pressure            (Pa)")
  call addvar_mgrads(bf,"z"   ,bf%kmax,"Interpolated Height     (m)")
  call addvar_mgrads(bf,"t"   ,bf%kmax,"Temperature             (K)")
  call addvar_mgrads(bf,"q"   ,bf%kmax,"Specific Humidith   (kg/kg)")
  call addvar_mgrads(bf,"u"   ,bf%kmax,"Zonal wind            (m/s)")
  call addvar_mgrads(bf,"v"   ,bf%kmax,"Meridional wind       (m/s)")
  call addvar_mgrads(bf,"w"   ,bf%kmax,"Vertical wind         (m/s)")
  call addvar_mgrads(bf,"dz  ",bf%kmax,"layer thickness         (m)")
  call addvar_mgrads(bf,"dz2 ",bf%kmax,"layer thickness       (mgp)")
  call addvar_mgrads(bf,"h",bf%kmax,"Hight at pressure level (m)")  ! Mudado de gh p/ z
  call addvar_mgrads(bf,"gh",bf%kmax,"Geopotential Hight   (mgp)")
end subroutine

subroutine grfile_writevars(tx,varcode,xyz)
 integer,                 intent(in)::tx  ! Time_Index
 character(len=*),        intent(in)::varcode
 real,dimension(:,:,:),intent(inout)::xyz
 call writebin_mgrads (bf,varcode,tx,xyz)

end subroutine 

subroutine grfile_close
  call writectl_mgrads(bf)
  call close_mgrads(bf)
end subroutine

end module
