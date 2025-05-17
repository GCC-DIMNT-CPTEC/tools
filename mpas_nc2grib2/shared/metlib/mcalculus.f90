module mcalculus
!--------------------------------------------------------------------------------------------------
!                                         mcalculus                                               !
!         Modulo de funcoes basicas de calculo vetorial, diferencial e integral                   !
!                          em coordenadas ortogonais esfericas                                    !
!--------------------------------------------------------------------------------------------------
! Autores:                                                                                        !
!   1 - SHSF: Sergio Henrique Soares Ferreira (2009)  MCT-INPE-CPTEC                              !                               
!-------------------------------------------------------------------------------------------------!
! HISTORICO
!   200910   SHSF : Versao original
!   20091111 SHSF : Revisado eliminacao de pontos indefinidos em Advec2D
!   20100726 SHSF : Corrigido gravidade no calculo de vento geostrofico a partir de altura geopotencial
!   20110402 SHSF : Acrescentado subrotina grad2D, corrigido bug na GRAD3D relativo a defesa de dados missing
!                   Esta correcao afeta o calculo dos gradientes nas bordas do dominio do modelo 
!   20110512 SHSF :Adicionado subrotuna para calculo da  advecao horizontal de um escalar (advec2De)
!   20110521 SHSF :Adicionado compensacao de curvatura da Terra 
!   20110615 SHSF :Revisao da compensacao de curvatura da Terra 
!   20110626 SHSF : Adicao de funcao para calculo de adveccao 2D de um escalar por vento horizontal (
!   20120323 SHSF : Adicao de funcao para calculo de integralt (int f(t) dt) 
!   20141014 SHSF : Adicao de rotina de inicializacao do modulo e mudanca de dy que antes era valor constante e agora pode ser valor variavel
!   20151031 SHSF : Adicao de rotina de somatorio recursivo(nao utilizado). Aumentado precisao do calculos na subrotina average,
!                   usado no calculo de termo barotropico 
    implicit none
    private

    !{Rotinas publicas deste modulo 
      public init_mcalculus
      public kDivV           !Calcula o divergente do ventor V multiplicado por K (onde K pode ser um valor constante ou um campo escalar)
      public GeostrophicWind !Calculo do vento geostrofico (vg)
      public FGeostrophicWind ! Calculo de f.Vg (Corriolis*Vento Geostrofico)
      public Corriolis       ! Calculo do parametro de Corriolis
      public bethapf2         ! Calculo do betha/f**2
      public thermalwind     !Calculo do vento termico 
      public Advec2D         !Advecao 2D de campo vetorial 
      public Advec2DE        !Advecao 2D de campo escalar 
      public Advec2DEvh      !Advecao 2D de campo escalar pelo vento horizontal
      public Advec3DP        !Advecao 3d de um campo escalar 
      public grad2d          !Gradiente de um campo escalar (2D) 
      public grad3d          !Gradiente de um campo escalar (3D)
      public grad3dp         !Gradiente de um campo escalar (3D) em niveis de pressao 
      public integrall       !Integral de linha de Fx.dx
      public integrall2      !Integral de linha de Fx.dx
      public integralt       !Integral de area fx.dt
      public integral_ab     !Integral f(a-b) = F
      public dfdx
      public dfdt
      public pbase_ptop
      public isundef
      public average
      public movaverage
      public recsum
      public mult
      public mcalculus_log 
      public f0_mcalculus
      public dx,dy
    !}
    !{
     interface KDivV
	module procedure kdivv1
	module procedure kdivv2
     end interface
    !}
     real,parameter:: rad=0.01745329252
     real,parameter:: omega=7.292*1E-5 ! Velocidade angular da terra
     real,parameter:: a=6.37e6         ! Raio da Terra
     integer       :: mcalculus_log    ! Se 1 indica para mcalculos imprimir logo na tela 
     real          :: undef
     integer       :: cx               !Domain type (0=global, 1= regional)
     Integer       :: Ordem             !coefficients used in this module for the finite difference generic formulation
  
     !{ public variables 
     real,allocatable   ::dx(:)        !Delta X em funcao da lati (em metros)
     real,allocatable   ::dy(:)            
     !}
contains
!--------------------------------------------------------------------------------------------------
! Init|   Inicializa este modulo configurando o sistema de coordenadas                      | SHSF|
!---------------------------------------------------------------------------------------------------
!
subroutine init_mcalculus(udef,dtype,ord)
 real,   intent(in)::udef
 integer,intent(in)::dtype !domain type (0 = global, 1=regional)
 integer,intent(in)::ord   !finite diference order (2=second-order scheme, 4=forth-order scheme)
 undef=udef
 cx=dtype
 ordem=ord
end subroutine

!--------------------------------------------------------------------------------------------------
! f0_mcalculus |   Parametro de Corriolis horizontal                                         | SHSF|
!---------------------------------------------------------------------------------------------------
function f0_mcalculus(llat_in,minlat);real:: f0_mcalculus
  !{ Interface
	real,intent(in)::llat_in
      real,optional,intent(in)::minlat
  !}
  real::llat
  if (present(minlat)) then 
    if (abs(llat_in)>=abs(minlat)) then
               llat=llat_in
          else
               llat=sign(minlat,llat_in)
          end if
   else
      llat=llat_in
   end if 

     f0_mcalculus=2.*omega*sin(llat*rad)
 
end function
!--------------------------------------------------------------------------------------------------
! integrall ! Calculo da integral(f(x)dx)/Integral(dx)                                         |SHSF 
!--------------------------------------------------------------------------------------------------
!     Esta funcao retorna o calculo da integral de uma funcao f(x)
!     dado por  int(f(x)i)/int(dx)i  com i=[i1,i2,..,in]
! 
! Nota: 
!   utilizado o metodo da area dos trapezios adaptado para valores indefinidos
!--------------------------------------------------------------------------------------------------    
  function integrall(fx,x,i1,in,err,positive);real::integrall
!{ variaveis  da interface    
	real,dimension(:),intent(in)::x  ! valores x(i) da funcao F(x(i)) 
	real,dimension(:),intent(in)::fx ! funcao F(x(i)) a ser integrada entre i1 a in
	integer,intent(in)::i1  ! Indice inicial da integracao
	integer,intent(in)::in  ! Indice final da integracao
!	real,intent(in)::undef ! Valor indefinido 
	integer,optional,intent(out)::err
	logical,optional::positive
!}
!{ Variaveis locais
	integer::i,no
	integer::ct
	real::dx,sdx,s
!}
	if (present(err)) err=0
	sdx=0
	s=0
	ct=0
	do i=i1,in-1
		no=0
		if((fx(i)==undef).or.(fx(i+1)==undef))  no=1 
		if((x(i)==undef).or.(x(i+1)==undef)) no=1
		if (no==0) then 
			dx=(x(i+1)-x(i))
			s=s+((fx(i)+fx(i+1))/2.0)*dx
			sdx=sdx+dx
			ct=ct+1
		end if
	end do



	if(ct>0) then
	  integrall=s/sdx
	else
	  if (present(err)) then 
		err=1
	 ! else
		!print *,":MCALCULUS:integrall: Warning! Integral return undefined value"
	  end if
	  integrall=undef
	end if

	if( present (positive)) then 
	  if ((positive).and.(integrall<0)) then 
	      print *,":MCALCULUS:Integrall: Error! Integral must be positive"
	      stop
	  elseif ((.not. positive).and.(integrall>0)) then 
	      print *,":MCALCULUS:Integrall: Error! Integral must be negative"
	      stop
	end if
	end if

  end function
  !--------------------------------------------------------------------------------------------------
! integralt ! Calculo da integral(f(t)dt)                                                |SHSF
!--------------------------------------------------------------------------------------------------
!     Esta funcao retorna o calculo da integral de uma funcao f(t) dt
! 
! Nota: 
!   utilizado o metodo da area dos trapezios adaptado para valores indefinidos
!--------------------------------------------------------------------------------------------------    
  function integralt(ft,dt,i1,in,err,positive);real::integralt
!{ variaveis  da interface    
	real,             intent(in)::dt  ! Intevalor dt 
	real,dimension(:),intent(in)::ft !  funcao F(t) a ser integrada entre i1 a in
	integer,          intent(in)::i1  ! Indice inicial da integracao
	integer,          intent(in)::in  ! Indice final da integracao
	!real,             intent(in)::undef ! Valor indefinido 
	integer,optional, intent(out)::err
	logical,optional             ::positive
!}
!{ Variaveis locais
	integer::i,no
	integer::ct
	real::s
!}
	if (present(err)) err=0
	s=0
	ct=0
	do i=i1,in-1
		no=0
		if((ft(i)==undef).or.(ft(i+1)==undef))  no=1
		if (no==0) then 
			s=s+((ft(i)+ft(i+1))/2.0)*dt
			ct=ct+1
		end if
	end do



	if(ct>0) then
	  integralt=s
	else
	  if (present(err)) then 
		err=1
          end if
	  integralt=undef
	end if

	if( present (positive)) then 
	  if ((positive).and.(integralt<0)) then
	      print *,":MCALCULUS:Integrall: Error! Integral must be positive"
	      stop
	  elseif ((.not. positive).and.(integralt>0)) then
	      print *,":MCALCULUS:Integrall: Error! Integral must be negative"
	      stop
	end if
	end if

  end function
!--------------------------------------------------------------------------------
! pbase_ptop ! Obtem a base e o topo de um perfil vertical                      | 
!--------------------------------------------------------------------------------
! Caso nao seja encontrada a base/topo da coluna vertical retorna o valor 0
!-------------------------------------------------------------------------------- 
  subroutine pbase_ptop (p,i1,in,pbase,ptop)
!{ variaveis  da interface    
	real,dimension(:),intent(in)::p  ! valores de niveis de pressao 
	integer,intent(in)          ::i1 ! Indice inicial da integracao
	integer,intent(in)          ::in  ! Indice final da integraca
	!real,intent(in)             ::undef ! Valor indefinido 
	real, intent(out)           ::pbase
	real, intent(out)           ::ptop
!}
!{ Variaveis locais
	integer::i,no

	
!}
	pbase=0.0
	ptop=200000.0
	do i=i1,in-1
		no=0
		if((p(i)==undef).or.(p(i+1)==undef)) no=1
		if (no==0) then 
			  if (pbase==0) pbase=p(i)
			  if (ptop>p(i+1)) ptop=p(i+1)
		end if
	end do
        if (pbase==0.0) ptop=0.0
        

  end subroutine
!--------------------------------------------------------------------------------------------------
!isundef !                                                                                   |SHSF 
!--------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------- 
function isundef  (p,i1,in); logical::isundef
!{ variaveis  da interface    
	real,dimension(:),intent(in)::p  ! valores de niveis de pressao 
	integer,intent(in)::i1  ! Indice inicial da integracao
	integer,intent(in)::in  ! Indice final da integraca
!	real,intent(in)::undef ! Valor indefinido 
!}(p(i,j,k)/=undef).and.(p(i,j,k-1)/=undef)
!{ Variaveis locais
	integer::i

	
!}
	isundef=.true.
	do i=i1,in
		if((p(i)/=undef)) isundef=.false.
	end do
		

  end function
!-
!--------------------------------------------------------------------------------------------------
! integrall ! Calculo da integral(f(x)dx)                                                |SHSF 
!--------------------------------------------------------------------------------------------------
!     Esta funcao retorna o calculo da integral de uma funcao f(x)
!     dado por  int(f(x)i)/int(dx)i  com i=[i1,i2,..,in]
! 
! Nota: 
!   utilizado o metodo da area dos trapezios adaptado para valores indefinidos
!--------------------------------------------------------------------------------------------------    
  function integrall2(fx,x,i1,in);real::integrall2
!{ variaveis  da interface    
	real,dimension(:),intent(in)::x  ! valores x(i) da funcao F(x(i)) 
	real,dimension(:),intent(in)::fx ! funcao F(x(i)) a ser integrada entre i1 a in
	integer,intent(in)::i1  ! Indice inicial da integracao
	integer,intent(in)::in  ! Indice final da integracao
!	real,intent(in)::undef ! Valor indefinido 
!}
!{ Variaveis locais
	integer::i,no
	integer::ct
	real::dx,sdx,s
!}
	
	sdx=0
	s=0
	ct=0
	do i=i1,in-1
		no=0
		if((fx(i)==undef).or.(fx(i+1)==undef))  no=1 
		if((x(i)==undef).or.(x(i+1)==undef)) no=1
		if (no==0) then 
			dx=(x(i+1)-x(i))
			s=s+((fx(i)+fx(i+1))/2.0)*dx
			ct=ct+1
		end if
	end do
	if(ct>0) then
	  integrall2=s
	else
	  integrall2=undef
	end if
  end function
!--------------------------------------------------------------------------------------------------
! integral-ab ! Calculo da integral(df(p)/dp) / integral (dp) c/ p = (a --> b                |SHSF 
!--------------------------------------------------------------------------------------------------
!     Esta funcao retorna o calculo da integral de uma funcao f(1:n) 
!     Ou seja. O resultado da soma (f(2)-f(1))+(f(3)-f(2))+...+f(n)-f(n-1)] = [f(n)-f(1)]
!    dividido pela somatoria de [p(2)-p(1)]+[p(3)-p(2)]+...+P(n)-p(n-1) = P(n)-p(1)
!
!
!--------------------------------------------------------------------------------------------------    
  function integral_ab(f,p,a,b);real::integral_ab
!{ variaveis  da interface    
	real,dimension(:),intent(in)::f ! funcao f(p) a ser integrada entre a a b
	real,dimension(:),intent(in)::p ! p da funcao 
	integer,intent(in)::a   ! Indice inicial da integracao
	integer,intent(in)::b  ! Indice final da integracao
!}
!{ Variaveis locais
	integer::i
	real::s
	real::sdp
	integer::ct
!}
	ct=0
	sdp=0
	s=0
	do i=a,b-1
		if((f(i)/=undef).and.(f(i+1)/=undef))  then 
			s=s+(f(i+1)-f(i))
			sdp=sdp+(p(i+1)-p(i))   
			ct=ct+1
		end if
	end do
	
	if (ct==0) then
	  integral_ab=undef
	else
	  integral_ab=s/sdp
	end if
	
  end function
!-------------------------------------------------------------------------------
! KDivV | Calcula   K * Nabla  V(Vx(),Vy()) 
!-------------------------------------------------------------------------------
! Calcula K * Divergente de V onde
!    K = e uma constante real .
!    V = e um vetor bidimensional com componentes Vx e Vy
! Nota:
!    Se K=-1.0 o resultado e a convergencia de V 
! Use 
!   Call KDivV1(K,VX,VY,dx,dy,cx,undef,KGV)
!-------------------------------------------------------------------------------
  subroutine KDivV1(K,VX,VY,KGV)

	Real,               intent(in) ::K      !Uma constante de multiplicacao:( Use -1.0 para calculo de convergencia)  
	Real,dimension(:,:),intent(in) ::Vx     !Componentes X do vetor V nos pontos P(i,j)
	Real,dimension(:,:),intent(in) ::Vy     !Componente Y do vetor V nos pontos P(i,j)
	real,dimension(:,:),intent(out)::KGV    !Resultado 
   !}

   !{ Variaveis locais
   
	integer            ::i1,i2,i
	integer            ::j1,j2,j
	real               ::ddx,ddy,n1,n2
	integer            ::imax,jmax
	integer            ::cx_in
	integer,allocatable::undefr(:,:)

    !}

 	cx_in=cx
	imax=size(KGV,1)
	jmax=size(KGV,2)
	allocate(undefr(1:imax,1:jmax))

	
	
!{ Mapenado pontos de grade indefinidos
	
	undefr(:,:)=1
	do i=1,imax;do j=1,jmax
	  if ((vx(i,j)==undef).or.(vy(i,j)==undef)) undefr(i,j)=0
	end do; end do
	
	call indefine_bordas2d(undefr,cx_in,imax,jmax)
        if (ordem==4) call indefine_bordas2d(undefr,cx_in,imax,jmax)
!}
	
!{ Processamento principal de KdivV
	do i=1,imax
	do j=1,jmax 
		
		if (Dx(j)==0) then	
		  print *,":MCALCULUS: KDIVV: Error: Division by zero"
		  stop
		end if
                
                if (undefr(i,j)==0) then 
			KGV(i,j)=undef
		else
                   ddx=derive(vx(:,j),dx(j),i,imax,cx_in) 
                   ddy=derive(vy(i,:),dy(j),j,jmax,    1)
		   KGV(i,j)=k*(ddx+ddy)
		end if

	end do
	end do
	deallocate(undefr)

end subroutine
!-------------------------------------------------------------------------------
! KDivV2 | Calcula   K * Nabla  V(Vx(),Vy()) 
!-------------------------------------------------------------------------------
! Calcula K * Divergente de V onde
!    K = e um campo escalar real K(i,j) 
!    V = e um vetor bidimensional com componentes Vx e Vy
! Use 
!   Call KDivV2(K,VX,VY,dx,dy,cx,undef,KGV)
!-------------------------------------------------------------------------------
  subroutine KDivV2(K,VX,VY,KGV)

	Real,dimension(:,:),intent(in) ::K      !Um campo escalar 
	Real,dimension(:,:),intent(in) ::Vx     !Componentes X do vetor V nos pontos P(i,j)
	Real,dimension(:,:),intent(in) ::Vy     !Componente Y do vetor V nos pontos P(i,j)
!	Real,dimension(:),  intent(in) ::dx     !Espacamento da grade X (variavel na direcao y)!
!	Real,               intent(in) ::dy     !Espacamento da grade Y (fixa) 
!	integer,            intent(in) ::cx     !Se 0 = Modelo global (grade circular em x) se 1 = Modelo regional 
!	real,               intent(in) ::undef  !Valor indefinido
	real,dimension(:,:),intent(out)::KGV    !Resultado 
   !}

   !{ Variaveis locais
   
	integer            ::i1,i2,i
	integer            ::j1,j2,j
	real               ::ddx,ddy,n1,n2
	integer            ::imax,jmax
	integer            ::cx_in
	integer,allocatable::undefr(:,:)

    !}

 	cx_in=cx
	imax=size(KGV,1)
	jmax=size(KGV,2)
	allocate(undefr(1:imax,1:jmax))

	
	
!{ Mapenado pontos de grade indefinidos
	
	undefr(:,:)=1
	do i=1,imax;do j=1,jmax
	  if ((vx(i,j)==undef).or.(vy(i,j)==undef).or.(k(i,j)==undef) ) undefr(i,j)=0
	end do; end do
	
	call indefine_bordas2d(undefr,cx_in,imax,jmax)
       if (ordem==4) call indefine_bordas2d(undefr,cx_in,imax,jmax)
!}
	
!{ Processamento principal de KdivV
	do i=1,imax
	do j=1,jmax 
		
		if (Dx(j)==0) then	
		  print *,":MCALCULUS: KDIVV: Error: Division by zero"
		  stop
		end if
		
        
        !       if((VX(i2,j2)==undef).or.(vx(i1,j1)==undef).or.(vy(i1,j1)==undef).or.(vy(i2,j2)==undef)) then
                if (undefr(i,j)==0) then
			KGV(i,j)=undef
		else
		      ddx=derive(vx(:,j),dx(j),i,imax,cx_in) 
                      ddy=derive(vy(i,:),dy(j),j,jmax,    1)
		      KGV(i,j)=k(i,j)*(ddx+ddy)
		end if

	end do
	end do
	deallocate(undefr)

end subroutine
!-------------------------------------------------------------------------------
!GeostrophicWind! Calcula Vento Geostrofico                             | SHSF
!------------------------------------------------------------------------------
! Calcula Vento geostrofico em coordenadas isobaricas (a partir da
! gradiente do geopotencial
!------------------------------------------------------------------------------
subroutine GeostrophicWind(Z,lat,Ug,Vg)
  !{ Variaveis da interface
   real,dimension(:,:),intent(in) ::Z     !Alturas geopotenciais (metros)
   real,dimension(:),  intent(in) ::lat   ! Latitude
   real,dimension(:,:),intent(out)::Ug,Vg !Componentes U e V do vento geostrofico
	
  !}
  !{Variaveis locais
    
    integer            ::imax,jmax
    real,allocatable   ::f(:)                ! Parametro da Forca de corriolis
    integer            ::i,j,i1,i2,j1,j2
    real               ::n1,n2
    real               ::dzdx, dzdy
    integer            ::cx_in
    integer,allocatable::undefr(:,:)
    real               ::llat             !Latitude local se < 2.5 =2.5

  !}

    cx_in=cx
    imax=size(Z,1) 
    jmax=size(Z,2) 
    allocate(undefr(1:imax,1:jmax))
    allocate(f(1:jmax))
   
    !{ Mapenado pontos de grade indefinidos

      undefr(:,:)=1
      do i=1,imax;do j=1,jmax
        if (z(i,j)==undef) undefr(i,j)=0
      end do; end do
     
      call indefine_bordas2d(undefr,cx_in,imax,jmax)
    !}
      
   !{ Calculo do parametro de corriolis conforme a latitude. Cado a latitude
   ! for muito proxima do equacao (menor que 5.0) graus, considera-se 5.0 graus

    do j=1,jmax 
      if (abs(lat(j))>=5.0) then
        llat=lat(j)
      else
        llat=sign(5.0,lat(j))
      end if
      f(j)=2.*omega*sin(llat*rad)
      !f(j)=2.*omega*sin(45.0*rad)
      
     end do

    !}

    do j=1,jmax 
      do i=1,imax
        
        call get_index(i,imax,cx_in,i1,i2,n1)
        call get_index(j,jmax,1,j1,j2,n2)
         
        if(undefr(i,j)==0) then !(Z(i2,j)==undef).or.(Z(i1,j)==undef).or.(Z(i,j1)==undef).or.(Z(i,j2)==undef)) then
           
            Vg(i,j)=undef
            Ug(i,j)=undef
           
        else
            
             dzdx=(Z(i2,j)-z(i1,j))/(n1*dx(j)*f(j))
             Vg(i,j)=9.8*dzdx
             
              dzdy=(Z(i,j2)-Z(i,j1))/(n2*dy(j)*f(j))
              Ug(i,j)=-9.8*dzdy
            
        end if
      enddo
    enddo
  deallocate(undefr,f)
  end subroutine
!-------------------------------------------------------------------------------
!Corriolis | Calcula o parametro de Corriolis
!------------------------------------------------------------------------------
subroutine Corriolis(lat,f,fv)
  !{Interface
    real,dimension(:),intent(in)           ::lat ! Latitudes
    real,dimension(:),intent(out)          ::f  !Corriolis parameter
    real,dimension(:),optional,intent(out) ::fv !Corriolis parameter (vertical)
  !}
  !{Variaveis locais
   integer::j,jmax
   real::llat
  !}
  jmax=size(lat,1)
    
     
  do j=1,jmax
    llat=lat(j)
    f(j)=2.*omega*sin(llat*rad)
  end do

  if (present(fv)) then 
    do j=1,jmax
      llat=lat(j)
      fv(j)=2.*omega*sin(llat*rad)
   end do
  end if  
end subroutine
!-------------------------------------------------------------------------------
!bethapf2 | Calcula o termo  betha/f^2 numericamente
!------------------------------------------------------------------------------
! o calculo e feito entre polos e +/- 1 grau de latitude.
! entre -1 e 1 grau considerase -1 ou 1 grau para evitar divisao por zero
! no polo sul e norte considera o valor do ponto de grade anterior 
!-------------------------------------------------------------------------------
subroutine bethapf2(lat,dx,dy,bf2)
!{ inteface
   Real,dimension(:),  intent(in) ::dx    !Espacamento da grade X (variavel na direcao y)
   Real,dimension(:),  intent(in) ::dy    !Espacamento da grade Y (fixa)
   real,dimension(:),  intent(in) ::lat   ! Latitude
   real,dimension(:),  intent(out)::bf2 ! Valores do termo bera por latitude
!}
!{ Local
   integer ::j,jmax
   real   ::llat
 !}
 
 jmax=size(lat,1)
  do j=2,jmax-1
    if (abs(lat(j))<1.0) then
       llat=sign(1.0,lat(j))
       bf2(j)=-abs((sin(lat(j+1)*rad)*dx(j+1)-sin(lat(j-1)*rad)*dx(j-1))/(dx(j)*dy(j)*4.0*omega*sin(llat*rad)**2))
       !bf2(j)=(sin(lat(j+1)*rad)-sin(lat(j-1)*rad))/(dy*4.0*omega*sin(llat*rad)**2)
    else
       bf2(j)=-abs((sin(lat(j+1)*rad)*dx(j+1)-sin(lat(j-1)*rad)*dx(j-1))/(dx(j)*dy(j)*4.0*omega*sin(lat(j)*rad)**2))
       !bf2(j)=(sin(lat(j+1)*rad)-sin(lat(j-1)*rad))/(dy*4.0*omega*sin(lat(j)*rad)**2)
   end if
   
  end do
  bf2(1)=bf2(2)
  bf2(jmax)=bf2(jmax-1)
  
end subroutine

!-------------------------------------------------------------------------------
!fGeostrophicWind! Calcula Corriolis * Vento Geostrofico                  | SHSF
!------------------------------------------------------------------------------
! Calcula Vento geostrofico em coordenadas isobaricas (a partir da
! gradiente do geopotencial
! Call fGeostrophicWind(Z(:,:),dx(:)),dy(:),cx,undef,fUg(:,:),fVg(:,:)
!------------------------------------------------------------------------------
! HISTORICO
!  20110521 - SHSF: Incluido compensacao de curvatura 

subroutine fGeostrophicWind(Z,fUg,fVg)
  !{ Variaveis da interface
   real,dimension(:,:),intent(in) ::Z       !Alturas geopotenciais (metros)
   real,dimension(:,:),intent(out)::fUg,fVg !Componentes U e V do vento geostrofico
   

  !}
  !{Variaveis locais
    integer            ::imax,jmax
    integer            ::i,j,i1,i2,j1,j2
    real               ::n1,n2
    real               ::dzdx, dzdy
    integer            ::cx_in
    integer,allocatable::undefr(:,:)
     real               ::llat             !Latitude local se < 2.5 =2.5

  !}

    cx_in=cx
    imax=size(Z,1)
    jmax=size(Z,2)
    allocate(undefr(1:imax,1:jmax))

    !{ Mapenado pontos de grade indefinidos
      undefr(:,:)=1
      do i=1,imax;do j=1,jmax
         if (z(i,j)==undef) undefr(i,j)=0
      end do; end do
      call indefine_bordas2d(undefr,cx_in,imax,jmax)
      !}

      do j=1,jmax 
        do i=1,imax
          call get_index(i,imax,cx_in,i1,i2,n1)
          call get_index(j,jmax,1,j1,j2,n2)
          if(undefr(i,j)==0) then !(Z(i2,j)==undef).or.(Z(i1,j)==undef).or.(Z(i,j1)==undef).or.(Z(i,j2)==undef)) then
            fVg(i,j)=undef
            fUg(i,j)=undef
          else
            dzdx=(Z(i2,j)-z(i1,j))/(n1*dx(j))
            fVg(i,j)=9.8*dzdx
            dzdy=(Z(i,j2)-Z(i,j1))/(n2*dy(j))
            fUg(i,j)=-9.8*dzdy
          end if
        enddo
      enddo
      deallocate(undefr)
  end subroutine

!-------------------------------------------------------------------------------
!ThermalWind! Calcula Vento TÃ©rmico                              | SHSF
!------------------------------------------------------------------------------
! Calcula Vento tÃ©rmico em coordenadas isobaricas (a partir da
! gradiente da temperatura mÃ©dia entre as camadas de pressÃ£o
!------------------------------------------------------------------------------
subroutine ThermalWind(T,P,lat,Ut,Vt)
  !{ Variaveis da interface
   real,dimension(:,:,:),intent(in) ::T     !Temperatura Virtual  (Kelvin)
   real,dimension(:,:,:),intent(in) ::P     !Pressao atmosferica 
   real,dimension(:),    intent(in) ::lat   ! Latitude
   real,dimension(:,:,:),intent(out)::Ut,Vt !Componentes U e V do vento geostrofico
	
  !}
  !{Variaveis locais
    real,parameter     :: omega=7.292*1E-5 ! Velocidade angular da terra
    real,parameter     :: a=6.37e6         ! Raio da Terra
    real,parameter     :: Rd=287.05        ! Constante Termodinamica do ar seco 
    integer            :: imax,jmax,kmax
    real               :: f0                ! Parametro da Forca de corriolis
    integer            :: i,j,k
    integer            :: i1,i2,j1,j2
    real               :: n1,n2
    real               :: dtdx, dtdy        ! Gradiente do vento termico (x e Y)
    integer            :: cx_in
    integer,allocatable:: undefr(:,:)
    real,   allocatable:: tm(:,:,:)
    real               :: llat            !Latitude local se < 2.5 =2.5

  !}

      cx_in=cx
      imax=size(T,1) 
      jmax=size(T,2)
      kmax=size(T,3) 
      allocate(undefr(1:imax,1:jmax))
      allocate(tm(1:imax,1:jmax,1:kmax))

      !{ calculando temperatura virtual media da camada       
      do i=1,imax;do j=1,jmax
       
       do k=2,kmax
 
         if ((t(i,j,k-1)/=undef).and.(t(i,j,k)/=undef).and.(p(i,j,k)/=undef).and.(p(i,j,k-1)/=undef)) then 
           tm(i,j,k)=(t(i,j,k-1)+t(i,j,k))/2
         else 
           tm(i,j,k)=undef
         end if
 
       end do  
      end do
      end do
      tm(:,:,1)=undef
      !}
      

      do k=1,kmax
        !{ Mapenado pontos de grade indefinidos
        undefr(:,:)=1
        do i=1,imax;do j=1,jmax
           if (tm(i,j,k)==undef) undefr(i,j)=0
        end do; end do

        call indefine_bordas2d(undefr,cx_in,imax,jmax)
        !}
                
        do j=1,jmax 
          !{ Calculo do parametro de corriolis conforme a latitude. Cado a latitude
          ! for muito proxima do equacao (menor que 2.5) graus, considera-se 2.5 graus
         if (abs(lat(j))>=5.0) then
               llat=lat(j)
          else
               llat=sign(5.0,lat(j))
          end if

          f0=2.*omega*sin(llat*rad)
          !}

          do i=1,imax
             call get_index(i,imax,cx_in,i1,i2,n1)
             call get_index(j,jmax,1,j1,j2,n2)
             if(undefr(i,j)==0) then !(Z(i2,j)==undef).or.(Z(i1,j)==undef).or.(Z(i,j1)==undef).or.(Z(i,j2)==undef)) then
                 Vt(i,j,k)=undef
                 Ut(i,j,k)=undef
             else

                 dtdx=(Tm(i2,j,k)-Tm(i1,j,k))/(n1*dx(j))
                 Vt(i,j,k)=Rd*dtdx/f0*log(p(i,j,k-1)/p(i,j,k))

                 dtdy=(tm(i,j2,k)-tm(i,j1,k))/(n2*dy(j))
                 Ut(i,j,k)=-Rd*dtdy/f0*log(p(i,j,k-1)/p(i,j,k))
            end if
          end do !i 
        end do !j
     end do !k 
     deallocate(undefr,tm)
end subroutine
!-------------------------------------------------------------------------------
!Advec2D! Calcula  Advecccao horizontal de Vb por Va                     | SHSF
!------------------------------------------------------------------------------
! Calcula a Adveccao horizontal de Vb por Va en coordenadas isobaricas 
!
!  Va e Vb sao vetores (i,j,k) sendo i,j coordenadas horizontais 
!   e k coordenada vertical de pressao 
!
!    
!  Nabla eh o operador   (d/dx()i +d/dy()j + d/dz()k)
!------------------------------------------------------------------------------
! Utilizacao:
!   1) Fornece Va e Vb de dimensoes (i,j,k,e2)  onde :
!     i,j,k sao as coordendas do campo 
!     e2 = 1,2,3 identificando as componentes vetorias nas direcoes x,y e z respectivamente
!
!   2_ Foneca dx(lat), dy : as variacoes da grade em metros nas direcoes x e y
!  
!   3) Forneca P(i,j,k) : os valores de pressao para o calculo interno de dp
!
!   4) Cx = 0 Coordenadas circulares de longitude (modelo global)
!         = 1 Coordenadas aberta (modelo regional)  
!   5) Undef = Valor indefinido 
!
!   O resultado e retornado em Va_Nvb(i,j,k,e1) onde
!      e1 =1,2 representa as componentes x e y 
!
!     Ou seja 
!
!      Va_Dvb(i,j,k,1) = Va(Nabla Vb)i (Componente do eixo x) 
!      Va_Dvb(i,j,k,2) = Va(Nabla Vb)J (Componente do eixo y) 
!
!      O terceiro termo (eixo z) nao e calculado
!----------------------------------------------------------------------------        
!HISTORY
! 2011-05-21-SHSF: Inclusao da compensacao de curvatura na derivada 
! 2013-08-05-SHSF: Eliminated unnecessary calculation of dx     
subroutine Advec2D(va,vb,P,va_Dvb)
  !{ Variaveis da interface
    real,dimension(:,:,:,:),intent(in) :: Va    !.Va(i,j,k,e) com e=1,2,3  u,v,w
    real,dimension(:,:,:,:),intent(in) :: Vb    !.Vb(i,j,k,e) com e=1,2,3  u,v,w
    real,dimension(:,:,:),intent(in)   :: P     !.Campo de pressao P(i,j,k)(supostamente constante em k
    real,dimension(:,:,:,:),intent(out):: Va_Dvb !.....Resultado 3 (i,j,k,e2)
                                                ! e1 =1,2  representa du e dv
                                                ! e2 =1,2  u*(e1)dx+v*(e1)dy+w*(e1)dp
	
  !}
  !{Variaveis locais
     integer::i,j,i1,i2,j1,j2,k,k1,k2
     real,dimension(3)  ::n
     integer            ::imax,jmax,kmax
     integer            ::cx_in
     integer            ::e1,e2
     real,dimension(2,3)::Vj_DviDxj !  Vj * (d(Vi)/dXj)  Onde i=1,2  e j=1,2,3
                                    !  Resumindo, conforme valor de i,j representa:
                                    ! (1,1:3) =  ( u *du/dx ; v dudy ; w du/dp)
                                    ! (2,1:3))=  ( u *dv/dx ; v dvdy ; w dv/dp)
     real,allocatable::d(:,:,:,:)
     integer,allocatable::undefr(:,:,:)
     integer,allocatable::undefrr(:,:,:)
     real::Dvi,Dxj
       INTEGER::CHUNK
       INTEGER            :: TID
       INTEGER            :: NTHREADS
       INTEGER            :: OMP_GET_NUM_THREADS  ! Funcao do omp que retorna o numero de processadores 
       INTEGER            :: OMP_GET_THREAD_NUM   ! fUNCAO DO OMP que retorna o numero do processador corrente
 !}
  !$OMP PARALLEL PRIVATE(TID)
           TID = OMP_GET_THREAD_NUM()
            IF (TID .EQ. 0) THEN
                NTHREADS = OMP_GET_NUM_THREADS()
                CHUNK=int(NTHREADS/2)+1  !int(bi%imax*bi%jmax*bi%kmax/nthreads)
	     END IF     
  !$OMP END PARALLEL 
   !}
    ! PRINT *,":MCALCULOS:CHUNK=",CHUNK
     cx_in=cx
     imax=size(Va,1)
     jmax=size(va,2)
     kmax=size(va,3)
     if (imax/=size(va_Dvb,1)) then 
       print *,":MCALCULUS:ADVEC2D:Error! invalid dimension!"
       stop
     end if 
     allocate(d(1:imax,1:jmax,1:kmax,1:3))
     allocate(undefr(1:imax,1:jmax,1:kmax))
     allocate(undefrr(1:imax,1:jmax,1:kmax))
!----------------------------------------------------------------
! Obtendo dx,dy,dp para todos os pontos de grade 
!-----------------------------------------------------------------
! Notas:
!  a)
!  d(,,,1) = dx  em todos os pontos de grade 
!  d(,,,2) = dy  em todos os pontos de grade
!  d(,,,3) = dp  em todos os pontos de grade 
! 
!  b) Para considerar enventuais variavoes da coordenada de pressao em 
!  		em funcao do sistema de coordenada do modelo 
! 		 Dp Ã© obtido em cada ponto e nivel isobarico 
!------------------------------------------------
!{ 
!{ Mapenado pontos de grade indefinidos
	undefr(:,:,:)=1
 !$OMP PARALLEL SHARED (va,vb,undefr) PRIVATE(I,j,k)
   !$OMP DO SCHEDULE(DYNAMIC,CHUNK)	
	do i=1,imax
	do j=1,jmax
	do k=1,kmax
	
	if ((va(i,j,k,1)==undef).or.(vb(i,j,k,1)==undef).or.(p(i,j,k)==undef)) undefr(i,j,k)=0
	if ((va(i,j,k,2)==undef).or.(vb(i,j,k,2)==undef)) undefr(i,j,k)=0
   	if ((va(i,j,k,3)==undef).or.(vb(i,j,k,3)==undef)) undefr(i,j,k)=0

	end do; end do; end do
     !$OMP END DO NOWAIT
     !$OMP END PARALLEL
	undefrr(:,:,:)=undefr(:,:,:)

!$OMP PARALLEL SHARED (d,p,undefr,undefrr) PRIVATE(I,j,k,i1,i2,j1,j2,k1,k2,n)
   !$OMP DO SCHEDULE(DYNAMIC,CHUNK)	
  do i=1,imax
  do j=1,jmax
  do k=1,kmax
!{ Caldula dx,dy,dp considerando valores indefinidos  ao redor 
   do e2=1,3
      i2=i;i1=i
      j1=j;j2=j
      k1=k;k2=k

      if (e2==1)  then

         call get_index(i,imax,cx,i1,i2,n(1))
         d(i,j,k,1)=dx(j)*n(1)

      elseif (e2==2) then

         call get_index(j,jmax,1,j1,j2,n(2))
         d(i,j,k,2)=dy(j)*n(2)

      elseif ((e2==3).and.(p(i,j,k2)/=undef).and.(p(i,j,k1)/=undef))  then
	 
         call get_index(k,kmax,1,k1,k2,n(3))
         if ((p(i,j,k1)==undef)) then 
            k1=k         
            if (p(i,j,k)<45000) then
                  print *,":MCALCULOS:Advec2D:Warning! Pressure value=",p(i,j,k),"At level=",k
            end if
         end if
         d(i,j,k,3)=(p(i,j,k2)-p(i,j,k1))
         if (d(i,j,k,3)>=0) then
            print *,":MCALCULOS:Advec2D: ERROR! dP>=0 "
            print *,":MCALCULOS:Advec2D: (k, dp) = ",k,d(i,j,k,3)
	    print *,":MCALCULOS:Advec2D: (p2,p,p1)= ",p(i,j,k2),p(i,j,k),p(i,j,k1)
	    print *,":MCALCULOS:Advec2D: (k2,p,k1)= ",k2,k,k1
            print *,"*** STOP ***"
            stop
            !d(i,j,k,3)=0 
	   ! undefrr(i,j,k)=0
         endif

      else
        undefr(i,j,k)=0
        d(i,j,k,3)=0

      end if
	  
     !{ Localizando possuem pontos indefinidos ao redor
       if (undefr(i2,j2,k2)==0) undefrr(i,j,k)=0
       if (undefr(i1,j1,k1)==0) undefrr(i,j,k)=0
      !}
   end do
   end do
   end do
   end do
    !$OMP END DO NOWAIT
     !$OMP END PARALLEL
!}
!{ Restringindo a area de pontos indefinidos
	undefr(:,:,:)=undefr(:,:,:)*undefrr(:,:,:)
!}

!}
!}

!-------------------------
! Processamento principal
!
!------------------------
!{
!            END IF 

   !$OMP PARALLEL SHARED (va,vb,vj_DviDxj,va_Dvb) PRIVATE(I,j,k,I1,I2,J1,J2,K1,K2,N,DXJ,E1,E2)
   !$OMP DO SCHEDULE(DYNAMIC,CHUNK)
   do i=1,imax
   do j=1,jmax
   do k=1,kmax
     
     if (undefr(i,j,k)==1) then 
       !{ Calculando termos isoladamente
        do e2=1,3
          i2=i;i1=i
          j1=j;j2=j
          k1=k;k2=k
          if (e2==1) call get_index(i,imax,cx,i1,i2,n(1))
          if (e2==2) call get_index(j,jmax,1,j1,j2,n(2))
          if (e2==3) call get_index(k,kmax,1,k1,k2,n(3))
          do e1=1,2     
             
             Dxj= d(i,j,k,e2)

            if (dxj/=0.0) then            
              Vj_DviDxj(e1,e2)=Va(i,j,k,e2)*(Vb(i2,j2,k2,e1)-Vb(i1,j1,k1,e1))/Dxj	
            else
              Vj_DviDxj(e1,e2)=undef
            end if

           ! IF (overflow_err(Vj_DviDxj(e1,e2))) then
           !   print *,"Vj_DviDxj(e1,e2)=",Vj_DviDxj(e1,e2),"(",e1,e2,")"
           !   print *,"i,j,k,dvi,dxj",i,j,k,dvi,dxj
           !   print *,"Va(i,j,k,e2)",va(i,j,k,e2)
           ! end if
          end do !next e1
        end do !next e2	
        !}
        !{ somando os termos nas suas respectivas componentes e1 = x e y
        do e1=1,2
          if (Vj_DviDxj(e1,1)/=undef) then 
            va_Dvb(i,j,k,e1)=Vj_DviDxj(e1,1)+Vj_DviDxj(e1,2)+Vj_DviDxj(e1,3)
          else
            va_Dvb(i,j,k,e1)=undef
          end if 
        end do
        !}
      else
        va_Dvb(i,j,k,:)=undef
      end if 
    enddo !next k
    enddo !next j
    enddo !next i
     !$OMP END DO NOWAIT
     !$OMP END PARALLEL
    deallocate(d,undefr,undefrr)
  end subroutine

!-------------------------------------------------------------------------------
!Advec2DE! Calcula  Va(Nabla E)                                | SHSF
!------------------------------------------------------------------------------
! Calcula a Adveccao horizontal de um campo escala E  por vetor Va  
!
!  Va Ã© um vetor (i,j,k) bidimensional e E um campo escalar onde 
!   i,j coordenadas horizontais 
!   e k coordenada vertical de pressao 
!
!  Nabla eh o operador   (d/dx()i +d/dy()j + d/dz()k)
!------------------------------------------------------------------------------
! Utilizacao:
!   1) Fornece Va  de dimensoes (i,j,e2)  onde :
!     i,j sao as coordendas do campo 
!     e1 = 1 ou 2 identificando as componentes vetorias nas direcoes x,y  respectivamente
!
!   2_ Foneca dx(lat), dy : as variacoes da grade em metros nas direcoes x e y
!  
!   3) Cx = 0 Coordenadas circulares de longitude (modelo global)
!         = 1 Coordenadas aberta (modelo regional)  
!   4) Undef = Valor indefinido 
!
!   O resultado Ã© um campo escalar  retornado em Va_De(i,j) onde
!     i,j tem o significado usual
!     
!      O terceiro termo (eixo z) nao e calculado
!----------------------------------------------------------------------------    
! HISTÃRICO
!  20110521-SHSF- Introduzido compoensacao de curvatura           
subroutine Advec2DE(Va,E,va_DE)
  !{ Variaveis da interface
    real,dimension(:,:,:),intent(in) :: Va    !.Va(i,j,e) com e=1,2,3  u,v,w
    real,dimension(:,:),intent(in)  :: e    !.e(i,j)  um campo escalar 
    real,dimension(:,:),intent(out):: Va_DE    ! Resultado campo escalarn
	
  !}
  !{Variaveis locais
     integer::i,j,i1,i2,j1,j2
     real,dimension(3)  ::n
     integer            ::imax,jmax,kmax
     integer            ::cx_in
     integer            ::e2
     real,dimension(2)  ::Vj_DeDxj !  Vj * (dE/dXj)  Onde j=1,2
                                    !  Resumindo, conforme valor de j representa:
                                    ! (1:2) =  ( u *de/dx ; v dEdy)
     real,allocatable   ::d(:,:,:)
     integer,allocatable::undefr(:,:)
     integer,allocatable::undefrr(:,:)
     real::DE,Dxj
   !}
     cx_in=cx
     imax=size(Va,1)
     jmax=size(va,2)
     allocate(d(1:imax,1:jmax,1:2))
     allocate(undefr(1:imax,1:jmax))
     allocate(undefrr(1:imax,1:jmax))
!----------------------------------------------------------------
! Obtendo dx,dy,dp para todos os pontos de grade 
!-----------------------------------------------------------------
! Notas:
!  a)
!  d(,,,1) = dx  em todos os pontos de grade 
!  d(,,,2) = dy  em todos os pontos de grade
!  d(,,,3) = dp  em todos os pontos de grade 
! 
!  b) Para considerar enventuais variavoes da coordenada de pressao em 
!  		em funcao do sistema de coordenada do modelo 
! 		 Dp Ã© obtido em cada ponto e nivel isobarico 
!------------------------------------------------
!{ 
!{ Mapenado pontos de grade indefinidos
	undefr(:,:)=1
	do i=1,imax
	do j=1,jmax

	
	if ((va(i,j,1)==undef).or.(e(i,j)==undef)) undefr(i,j)=0
	if (va(i,j,2)==undef) undefr(i,j)=0
   	
	end do; end do
	undefrr(:,:)=undefr(:,:)
	
  do i=1,imax
  do j=1,jmax
!{ Caldula dx,dy,dp considerando valores indefinidos  ao redor 
   do e2=1,2
      i2=i;i1=i
      j1=j;j2=j
      if (e2==1)  then
         call get_index(i,imax,cx,i1,i2,n(1))
         d(i,j,1)=dx(j)*n(1)
      elseif (e2==2) then
         call get_index(j,jmax,1,j1,j2,n(2))
         d(i,j,2)=dy(j)*n(2)
      end if
	  
     !{ Localizando possuem pontos indefinidos ao redor
       if (undefr(i2,j2)==0) undefrr(i,j)=0
       if (undefr(i1,j1)==0) undefrr(i,j)=0
      !}
   end do
   end do
   end do

!}
!{ Restringindo a area de pontos indefinidos
	undefr(:,:)=undefr(:,:)*undefrr(:,:)
!}

!}
!}

!-------------------------
! Processamento principal
!
!------------------------
!{
   do i=1,imax
   do j=1,jmax
     if (undefr(i,j)==1) then 
       !{ Calculando termos isoladamente
        do e2=1,2
          i2=i;i1=i
          j1=j;j2=j
          if (e2==1) call get_index(i,imax,cx,i1,i2,n(1))
          if (e2==2) call get_index(j,jmax,1,j1,j2,n(2))
           DE=(E(i2,j2)-E(i1,j1))
          Dxj= d(i,j,e2)
          if (dxj/=0.0) then
              Vj_DeDxj(e2)=Va(i,j,e2)*DE/Dxj
            else
              Vj_DeDxj(e2)=undef
            end if
            IF (overflow_err(Vj_DeDxj(e2))) then
              print *,"Vj_DeDxj(e2)=",Vj_DeDxj(e2),"(",e2,")"
              print *,"i,j,dvi,dxj",i,j,de,dxj
              print *,"Va(i,j,e2)",va(i,j,e2)
            end if
        end do !next e2	
        !}
        !{ somando os termos nas suas respectivas componentes  x e y
          if (Vj_DeDxj(1)/=undef) then 
            va_De(i,j)=Vj_DeDxj(1)+Vj_DeDxj(2)
          else
            va_De(i,j)=undef
          end if 
        !}
      else
        va_De(i,j)=undef
      end if 
    enddo !next j
    enddo !next i

    deallocate(d,undefr,undefrr)
  end subroutine


!-------------------------------------------------------------------------------
!Advec2DEvh  | A Advecao de um campo escalar pelo vento horizontal 
!------------------------------------------------------------------------------!
subroutine advec2DEvh(u,v,e,advec_e)
  !{ Interface
    real,dimension(:,:,:),intent(in) :: u     !Componente zonal da vento u(i,j,k)
    real,dimension(:,:,:),intent(in) :: v     !Componente meridional do vento v(i,j,k)
    real,dimension(:,:,:),intent(in) :: e     !Campo escalar e (i,j,k) 
    real,dimension(:,:,:),intent(out):: Advec_E  ! Resultado da advecao de E(i,j,k)
  !}
  !{ local 
    integer::imax,jmax,kmax
    integer::i,j,k
    real,allocatable::gradE(:,:,:)
  !}  
    
    imax=size(e,1)
    jmax=size(e,2)
    kmax=size(e,3)
    allocate (grade(1:imax,1:jmax,1:2))

    do k=1,kmax
      call grad2D(1.0,e(:,:,k),gradE(:,:,:))
     
      do i=1,imax
        do j=1,jmax
          if ((u(i,j,k)/=undef).and.(v(i,j,k)/=undef).and.(grade(i,j,1)/=undef).and.(grade(i,j,2)/=undef)) then
            advec_e(i,j,k)=u(i,j,k)*gradE(i,j,1)+v(i,j,k)*grade(i,j,2) 
          else
            advec_e(i,j,k)=undef 
          end if
        end do !Next j
      end do !next i
    end do !next k

    deallocate (grade)
end subroutine
!------------------------------------------------
!Advec3DP  | A Advecao 3D de um  campo escalar
!------------------------------------------------
subroutine advec3DP(u,v,w,e,p,adh,adv)
  !{ Interface
    real,dimension(:,:,:),intent(in) :: u     !Componente zonal da vento u(i,j,k)
    real,dimension(:,:,:),intent(in) :: v     !Componente meridional do vento v(i,j,k)
    real,dimension(:,:,:),intent(in) :: w     !Componente vertical do vento w(i,j,k)
    real,dimension(:,:,:),intent(in) :: e     !Campo escalar e (i,j,k) 
    real,dimension(:,:,:),intent(in) :: P     ! Niveis verticais de pressao
    real,dimension(:,:,:),intent(out):: Adh   ! Resultado da advecao horizontal de E(i,j,k)
    real,dimension(:,:,:),intent(out):: Adv   ! Resultado da advecao vertical de E(i,j,k)
  !}
  !{ local 
    integer::imax,jmax,kmax
    integer::i,j,k
    real,allocatable::gradE(:,:,:,:)
  !}  
    
    imax=size(e,1)
    jmax=size(e,2)
    kmax=size(e,3)
    allocate (grade(1:imax,1:jmax,1:kmax,1:3))

    call grad3DP(E,P,gradE(:,:,:,:))
     
     do k=1,kmax
      do i=1,imax
        do j=1,jmax

          if ((u(i,j,k)/=undef).and.(v(i,j,k)/=undef).and.(grade(i,j,k,1)/=undef).and.(grade(i,j,k,2)/=undef)) then
            adh(i,j,k)=u(i,j,k)*gradE(i,j,k,1)+v(i,j,k)*grade(i,j,k,2)
          else
            adh(i,j,k)=undef
          end if
          
          if ((w(i,j,k)/=undef).and.(grade(i,j,k,3)/=undef)) then
            adv(i,j,k)=w(i,j,k)*gradE(i,j,k,3)
          else
            adv(i,j,k)=undef
          end if
          
        end do !Next j
      end do !next i
    end do !next k

    deallocate (grade)
end subroutine 

!-------------------------------------------------------------------------------
!grad3D | Obtem o gradiente de um campo escalar 3D                       | SHSF
!------------------------------------------------------------------------------
!Historico
! 2011-05-21 -shsf: Adicionado compensacao de curvatura 
subroutine grad3D(F,dz,gradF)
!{ Interface 
   real,dimension(:,:,:),intent(in)::F ! matriz do campos escalar
   real,                 intent(in)::dz !
   real,dimension(:,:,:,:),intent(out)::gradF !X,Y,Z,3
  
 !}
 !{Variaveis locais
   integer::i,i1,i2,imax
   integer::j,j1,j2,jmax
   integer::k,k1,k2,kmax
   real::n1,n2,n3
   integer::cx_in
 !}

    cx_in=cx
    imax=size(F,1)
    jmax=size(F,2)
    kmax=size(F,3)


  ! Gradientes na direcao X,Y,Z
  do i=1,imax
    do j=1,jmax
      do k=1,kmax
        call get_index(i,imax,cx_in,i1,i2,n1)
        call get_index(j,jmax,1,j1,j2,n2)
        call get_index(k,kmax,1,k1,k2,n3)
        if((F(i2,j,k)==undef).or.(F(i1,j,k)==undef)) then
          gradf(i,j,k,1)=undef
        else
          gradf(i,j,k,1)=(F(i2,j,k)-F(i1,j,k))/(n1*dx(j))!....... Gradiente na direcao X
        end if

        if((F(i,j2,k)==undef).or.(F(i,j1,k)==undef)) then
          gradf(i,j,k,2)=undef
        else
          gradf(i,j,k,2)=(F(i,j2,k)-F(i,j1,k))/(n2*dy(j)) !.... Gradiente na direcao y
        end if

        if((F(i,j,k2)==undef).or.(F(i,j,k1)==undef)) then
          gradf(i,j,k,3)=undef
        else
          gradf(i,j,k,3)=(F(i,j,k2)-F(i,j,k1))/(n3*dz) !..... Gradiente na direcao Z
        end if
      end do !K
    end do !j
  end do !I

end subroutine
!-------------------------------------------------------------------------------
!grad3D | Obtem o gradiente de um campo escalar 3D                       | SHSF
!------------------------------------------------------------------------------
!Historico
! 2011-07-06 Criada a subrotina
!            No momento e por facilidade, a derivada vertical de pressao
!            nao Ã© realizada quando os niveis de pressao interceptam a superficie 
!            Nestes casos Ã© atribuido o valor zero ao gradiente
!            Esta e uma solucao provisoria. Posteriormente precisa-se dar uma
!            solucao mais adequada junto a superficie
!
subroutine grad3DP(F,p,gradF)
!{ Interface 
   real,dimension(:,:,:),  intent(in)::F ! matriz do campos escalar
   real,dimension(:,:,:),  intent(in)::p !Campo de pressoes (niveis isobaricos)
   real,dimension(:,:,:,:),intent(out)::gradF !X,Y,Z,3
 !}
 !{Variaveis locais
   integer::i,i1,i2,imax
   integer::j,j1,j2,jmax
   integer::k,k1,k2,kmax
   real::n1,n2,n3
   integer::cx_in
   real   ::dp           !Variacao da pressao (coordenada vertical)
  ! integer,allocatable::undefr(:,:,:)
   
 !}

    cx_in=cx
    imax=size(F,1)
    jmax=size(F,2)
    kmax=size(F,3)
   ! allocate(undefr(1:imax,1:jmax,1:kmax))
!!{ Mapenado pontos de grade indefinidos
!	
!	undefr(:,:)=1
!        do k=1,kmax
!	do i=1,imax;do j=1,jmax
!	  if ((gradf(i,j)==undef).or.(p(i,j,k)=undef)) undefr(i,j)=0
!	end do; end do
!	end do
!	call indefine_bordas2d(undefr,cx_in,imax,jmax)
!       if (ordem==4) call indefine_bordas2d(undefr,cx_in,imax,jmax)
!}

  ! Gradientes na direcao X,Y,Z
  do i=1,imax
    do j=1,jmax
      do k=1,kmax
        call get_index(i,imax,cx_in,i1,i2,n1)
        call get_index(j,jmax,1,j1,j2,n2)
        call get_index(k,kmax,1,k1,k2,n3)
        !-------------------------
        ! Calculo do  intervalo dp
        !No caso de campo indefinido (provalvel nivel de superficie) atribui zero
        !-------------------------
        !{
        if ((p(i,j,k1)/=undef).and.(p(i,j,k2)/=undef)) then
          dp=p(i,j,k1)-p(i,j,k2)
        else
          dp=0.0
        end if 
        !}
        if((F(i2,j,k)==undef).or.(F(i1,j,k)==undef)) then
          gradf(i,j,k,1)=undef
        else
          gradf(i,j,k,1)=(F(i2,j,k)-F(i1,j,k))/(n1*dx(j))!....... Gradiente na direcao X
        end if

        if((F(i,j2,k)==undef).or.(F(i,j1,k)==undef)) then
          gradf(i,j,k,2)=undef
        else
          gradf(i,j,k,2)=(F(i,j2,k)-F(i,j1,k))/(n2*dy(j)) !.... Gradiente na direcao y
        end if

        if((F(i,j,k2)==undef).or.(F(i,j,k1)==undef)) then
          gradf(i,j,k,3)=undef
        elseif (dp==0.0) then 
          gradf(i,j,k,3)=0.0 
        else 
          gradf(i,j,k,3)=(F(i,j,k2)-F(i,j,k1))/(n3*dp) !..... Gradiente na direcao Z
        end if
      end do !K
    end do !j
  end do !I
 ! deallocate (undefr)
end subroutine
!-------------------------------------------------------------------------------
!grad2D | Obtem o gradiente de um campo escalar 2D                       | SHSF
!------------------------------------------------------------------------------
! Exemplo de uso 
!  call grad2D (g,F(:,:),dx(:),dy,dx,cx,undef,gradF(:,:,:) )
!  
!
! Obs.: Falta verificar se existe a necessidade de defesa adicional para
! o caso em que o gradiente em uma direcao for indefinido e na outra nao
! Se isto causar erros Ã© interessante criar defesa para garantir que 
! quando o gradiente for indefinido em uma direcao seja tambem na outra 
!
!----------------------------------------------------------------------------
! Historico:
!  20110521-shsf- Adicao de compensacao de curvatura

subroutine grad2D(g,F,gradF)
  !{ Interface 
   real,                 intent(in) ::g     ! uma constante de multiplicacao
   real,dimension(:,:),  intent(in) ::F     ! matriz do campos escalar F(i,j)
   real,dimension(:,:,:),intent(out)::gradF ! Resultado gradF(i,j,e) onde e=1,2 (direcao x e y respectivamente) 
  !}

  !{Variaveis locais
   integer::i,i1,i2,imax
   integer::j,j1,j2,jmax
   real::n1,n2
   integer::cx_in
   integer,allocatable::undefr(:,:)
  !}

   	cx_in=cx
	imax=size(gradF,1)
	jmax=size(gradF,2)
	allocate(undefr(1:imax,1:jmax))

	
	
!{ Mapenado pontos de grade indefinidos
	
	undefr(:,:)=1
	do i=1,imax;do j=1,jmax
	  if (F(i,j)==undef) undefr(i,j)=0
	end do; end do
	
	call indefine_bordas2d(undefr,cx_in,imax,jmax)
       if (ordem==4) call indefine_bordas2d(undefr,cx_in,imax,jmax)
!}

 !{ Calculo do Gradiente 
    do i=1,imax
      do j=1,jmax
               if (undefr(i,j)==0) then 
			gradF(i,j,:)=undef
		else
                   gradf(i,j,1)=g*derive(F(:,j),dx(j),i,imax,cx_in) !.... Gradiente na direcao X 
                   gradf(i,j,2)=g*derive(F(i,:),dy(j),j,jmax,    1) !.... Gradiente na direcao y
		end if
       end do !j
    end do !I
    deallocate (undefr) 

end subroutine
!-------------------------------------------------------------------------------
!dfdx | Obtem a derifada de uma funcao                                | SHSF
!------------------------------------------------------------------------------
! call dfdx(F(:),dx,undef,ddx(:))
subroutine dfdx(F,dx,ddx)
	real,dimension(:),intent(in)::F ! Vetor F(x)
	real,intent(in)::dx ! Intervalo de x entre dois pontos
	real,dimension(:),intent(out)::ddx 

	
  !}
  !{Variaveis locais

	integer::i,i1,i2,imax
	real::n1
	
	
  !}

	
      imax=size(F,1) 
    
        
	! Gradiente na direcao X
	do i=1,imax
		call get_index(i,imax,1,i1,i2,n1)
		
		if((F(i2)==undef).or.(F(i1)==undef)) then
			ddx(i)=undef
		else
			ddx(i)=(F(i2)-F(i1))/(n1*dx)!....... Gradiente na direcao X
		end if

	end do !I
end subroutine

!-------------------------------------------------------------------------------
!dfdt | Obtem a derifada de uma funcao no tempo                          | SHSF
!------------------------------------------------------------------------------
! Exemplo de utilizacao
!
!    call dfdt(F(:),dt,undef,ddt(:))
!
!------------------------------------------------------------------------------
subroutine dfdt(F,dt,ddt)
  !{ interface
   real,dimension(:),intent(in) ::F     ! Vetor F(t)
   real,intent(in)              ::dt    ! Intervalo de tempo entre dois pontos
   real,dimension(:),intent(out)::ddt   ! Resultado da derivada no tempo 
  !}
  !{Locals
   integer::t,t1,t2,tmax
   real::n1
  !}

   tmax=size(F,1) 
    
   do t=1,tmax
      call get_index(t,tmax,1,t1,t2,n1)
      if((F(t2)==undef).or.(F(t1)==undef)) then
        ddt(t)=undef
      else
        ddt(t)=(F(t2)-F(t1))/(n1*dt)!....... Gradiente na direcao X
      end if
    end do !I
end subroutine


!-------------------------------------------------------------------------------
!get_index !Obterm os indices  anterior e o posterior ao indice i        | SHSF
!------------------------------------------------------------------------------
! Obterm os indices  anterior e o posterior ao indice i, onde 
! i assume valores = i=(1,2,...,imax-1,imax) 
!   i=1 Ã© borda direita de uma grade 
!   i=imax Ã¡ a borda esquerda de uma grade
!
!   A grade pode ser aberta o circular
!   Se for circular 
!   imax+1 = 1
!   i-1 = imax
!
!   Se for aberta
!   i-1 = i
!   imax+1=imax
!
!  A variavel Cx = 0 indica grade circular
!-------------------------------------------------------------------------------
	subroutine get_index(i,imax,cx,i1,i2,n)
	!{Variaveis da interface
		integer,intent(in) ::i    ! Indice fornecido
		integer,intent(in) ::imax ! I maximo (indice da borda esquerda) 
		integer,intent(in) ::cx   !=0 Grade circular, =1 Grade aberta
		integer,intent(out)::i1   ! Indice anterior
		integer,intent(out)::i2   ! indice posterior
		real,intent(out)   ::n    ! 2.0 ou 1,0  conforme numero de pontos da grade utilizado
	!}
	  
	if (cx==0) then
!--------------------  
! caso grade circular 
!--------------------
!{
	if (i==1) then  
                !{Resolve borda direita
                i1=imax
                i2=2    
                n=2.0
                !}
        elseif(i==imax) then  
                !{ Resolve borda esquerda
		i2=1
		i1=imax-1 
		n=2.0
		!}
	else
		!{ Resolve meio 
		i2=i+1
		i1=i-1
		n=2.0
		!}
	end if
!}
	else
!------------------
! caso grade aberta 
!------------------
!{
	if (i==1) then 
		!{Resolve borda direita
		i1=1
		i2=2
		n=1.0
	elseif(i==imax) then 
		!{ Resolve borda esquerda
		i2=imax
		i1=imax-1
		n=1.0
	else
       		!{ Resolve meio 
		i2=i+1
		i1=i-1
		n=2.0
		!}
	end if
	
!}
	end if

	!{ calculo do espacamento = 2.0 ou 1.0


	end subroutine 
	!{ Mapenado pontos de grade indefinidos
	


!------------------------------------------------------------------------------
!indefine_bordas2d|                                                  |SHSF
!-----------------------------------------------------------------------------
! Dado matriz onde existem regiÃµes com dados zero, aumenta o tamanho destas  
! regioes em 1 ponto de grade de cada lado  
!-----------------------------------------------------------------------------
	subroutine indefine_bordas2d(undefr,cx_in,imax,jmax)
	!{ Variaveis da interface
	  integer,dimension(:,:),intent(inout)::undefr
	  integer,intent(in)::imax,jmax
	  integer,intent(in)::cx_in
	!}  
	!{Variaveis locais
	  integer,dimension(1:imax,1:jmax)::undefrr
	  integer::i,j,i1,j1,i2,j2
	  real::n1,n2
	!}
	

	undefrr(:,:)=undefr(:,:)
	do i=1,imax
	do j=1,jmax
		i2=i;i1=i
		j1=j;j2=j
		call get_index(i,imax,cx_in,i1,i2,n1)
                if (undefr(i2,j)==0) undefrr(i,j)=0 ! Faz Borda Direita = 0
                if (undefr(i1,j)==0) undefrr(i,j)=0 ! Faz Borda esquerda = 0 
                
                call get_index(j,jmax,1,j1,j2,n2)
                if (undefr(i,j2)==0) undefrr(i,j)=0  !Faz borda inferior = 0 
                if (undefr(i,j1)==0) undefrr(i,j)=0  !Faz borda superior = 0 
        end do
	end do
	undefr(:,:)=undefr(:,:)*undefrr(:,:)
	!}
      end subroutine


!---------------------------------------------------------------------------------------
!average  | Media  aritimetica                                                | SHSF   |
!---------------------------------------------------------------------------------------
! Calculo da media aritimetica
!---------------------------------------------------------------------------------------

function average(value,i1,i2); real::average
!{Variaveis da interface
	real,dimension(:),intent(in)::value  !Valores para calculo da media 
	integer,          intent(in)::i1 !indice do valor inicial 
	integer,          intent(in)::i2 !indece do valor final 
!}
!{Variaveis locais
	integer::ct ! Contador 
	integer::i
        real*8::aux

!}
	ct=0
	do i=i1,i2
		    if(value(i)/=undef) then
			aux=(aux*real(ct)+value(i))/real(ct+1)
			ct=ct+1
		    end if
	end do

	if (ct==0) then 
              average=undef
        else
              average=aux
        end if

        end function

!---------------------------------------------------------------------------------------
!recsum |Soma recursiva somatorio de                  | SHSF   |
!---------------------------------------------------------------------------------------
! Calculo da media aritimetica através de cálculo partical para matriz quadridimensiona  
!---------------------------------------------------------------------------------------

 subroutine recsum(newval,counter,psum)
!{Variaveis da interface
	real,   dimension(:,:,:,:),   intent(in)::newval  !Novo valor 
	integer,dimension(:,:,:,:),intent(inout)::counter !Numero de somas efetuadas
        real*8, dimension(:,:,:,:),intent(inout)::psum    !Valor parcial da somatoria 
!}
!{ Variaveis locais
    integer::i,j,k,e
    integer::imax,jmax,kmax,emax
!}
    imax=size(newval,1)
    jmax=size(newval,2)
    kmax=size(newval,3)
    emax=size(newval,4)
  
    do i=1,imax
    do j=1,jmax
    do k=1,kmax
    do e=1,emax
        if(newval(i,j,k,e)/=undef) then
	    psum(i,j,k,e)=psum(i,j,k,e)+newval(i,j,k,e)
	    counter(i,j,k,e)=counter(i,j,k,e)+1
        end if
    end do 
    end do 
    end do 
    end do 
  end subroutine

!---------------------------------------------------------------------------------------
!movaverage  | Media  Movel                                                   | SHSF   |
!---------------------------------------------------------------------------------------
! Calculo da mÃ©dia movel 
!---------------------------------------------------------------------------------------

subroutine movaverage(value,i1,i2,di,fvalue)
!{Variaveis da interface
    real,dimension(:),intent(in)::value  !Valores para calculo da media
    integer,          intent(in)::i1     !indice do valor inicial
    integer,          intent(in)::i2     !indece do valor final
    integer,          intent(in)::di     !Numero de pontos (antes e depois) para o calculo da media movel) para media
    real,dimension(:),intent(out)::fvalue!Valores filtrados por mÃ©dia movel 
!}
!{Variaveis locais
    integer::ct ! Contador
    integer::i
    integer::j,j1,j2
!}

     do i=i1,i2
        j1=i-di
        j2=i+di
        if (j1<i1) j1=i1
        if (j1>i2) j2=i2
        ct=0
        do j=j1,j2
            if(value(j)/=undef) then
              fvalue(i)=(fvalue(i)*real(ct)+value(j))/real(ct+1)
              ct=ct+1
            end if
         end do
         if (ct==0)fvalue(i)=undef
      end do
   end subroutine

!---------------------------------------------------------------------------------------
!mult  | Retorna o resultado de  a * b  se ambos forem diferentes de undef    | SHSF   |
!---------------------------------------------------------------------------------------

function mult(a,b);real::mult
!{ variaveis de interface
	real,intent(in)::a
	real,intent(in)::b

!}
!{ Teste de transbordamento 
!      if (( abs(a)>abs(undef) ).or.( abs(a)>abs(undef) )) then 
!	print *,"a=",a
!	print *,"b=",b
!	stop
!    end if


  if ((a/=undef).and.(b/=undef)) then 
      mult=a*b
  else
      mult=undef
  end if
end function



!-------------------------------------------------
! Subrotina para teste local
!--------------------------------------------------
function overflow_err(v); logical::overflow_err
  !{ Variaveis da interface
  real,intent(in)::v

  !}
  !{Variaveis locais
    real::scalemax
    real::scalemin
   
  !}
	scalemax=10.0**(int(log(abs(undef))/log(10.0))-1)
	scalemin=-scalemax
	overflow_err=.false.
	if (v/=undef) then
	if ((v>scalemax).or.(v<scalemin))  then 
		overflow_err=.true.
		return
	end if	
	end if
	
end function

!-------------------------------------------------------------------------------
!derive !  derivative at point                                            | SHSF
!------------------------------------------------------------------------------
! Derivative of a function fx defined around the point i
!
! Important: This function presupposes that all points of fx were previously checked
!             for their existence
!------------------------------------------------------------------------------- 
!  index  and after of index i for finite diference shemes, where 
! i takes values ​​i = (1,2,...,imax-1,imax)
! i = 1 is the right edge of the grid
! i = imax is the left edge of the grid
!
!   The grid can be opened or circular
!   if circular 
!   imax+1 = 1
!   i-1 = imax
!
!   If opended 
!   i-1 = i
!   imax+1=imax
!
!   Cx = 0 indicates circular grid
!
!The derivation scheme fourth-order can be used, except at borders  of the domain
!-------------------------------------------------------------------------------
 function derive (fx,dx,i,imax,cx); real::derive
 !{Variaveis da interface 
     real,dimension(:),   intent(in) ::fx   ! Values ​​of the function to be derived
     real,                intent(in) ::dx   ! Delta X
     integer,             intent(in) ::i    ! Point 
     integer,             intent(in) ::imax ! I maximo (indice da borda esquerda) 
     integer,             intent(in) ::cx   ! =0 Grade circular, =1 Grade aberta
  !}
  !{ Local variables
     integer                ::i1,i2       ! Indexes after and before current index i  in the direction x
     real                   :: n
   !}

!{ caso meio da grade e derivada de quarta ordem  
 if(ordem==4) then 
 if ((i>2).and.(i<imax-1)) then 
     derive=((4.0/3.0)*(fx(i+1)-fx(i-1))/(2.0*Dx))-((1.0/3.0)*(fx(i+2)-fx(i-2))/(4.0*Dx))
    goto 10   
 end if
 end if
!{ caso meio da grade derivada de segunda ordem centrada

 if ((i>1).and.(i<imax)) then 
    derive=(fx(i+1)-fx(i-1))/(2.0*Dx)
    goto 10   
 end if

 if(cx==0) then 
   !{ caso bordas da grade circular (derivada segunda ordem centrada)
  	if (i==1) then  
                !{Resolve borda direita
                i1=imax
                i2=2    
                n=2.0
                !}
        elseif(i==imax) then  
                !{ Resolve borda esquerda
		i2=1
		i1=imax-1 
		n=2.0
		!}
	end if
       derive=(fx(i2)-fx(i1))/(n*Dx)
       goto 10      
  end if
 !}
  if(cx==1) then
    !{ caso bordas da grade aberta (derivadas de primeira ordem)
	if (i==1) then 
		!{Resolve borda direita
		i1=1
		i2=2
		n=1.0
	elseif(i==imax) then 
		!{ Resolve borda esquerda
		i2=imax
		i1=imax-1
		n=1.0
	end if
	derive=(fx(i2)-fx(i1))/(n*Dx)
        goto 10 
    !}
  end if
   
  10 continue    

end function 

 end module
