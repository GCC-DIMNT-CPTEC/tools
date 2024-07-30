!*******************************************************************************
!*                                 MGRADS                                      *
!*                                                                             *
!* Modulo de funcoes e subrotinas uteis para gravacao e leitura de dados       *
!*                    meteorologicos no formato do GRADS                       *
!*                                                                             *
!*                                                                             *
!*                      Sergio Henrique S. Ferreira                            *
!*                                                                             *
!*               MCTI-INPE- Sao Jose dos Campos ,   Brasil                     *
!*                                                                             *
!*                                  2002                                       *
!*-----------------------------------------------------------------------------*
!*Autores:                                                                     *
!*   SHSF : Sergio Henrique S. Ferreira <sergio.ferreira@cptec.inpe.br>        * 
!*                                                                             *
!*******************************************************************************
!* DEPENDENCIAS                                                                * 
!* MODULOS DATELIB E STRINGFLIB                                                * 
!*******************************************************************************
! HISTORICO / ATUALIZACOES
!  DEZ 2002 -SHSF-   Versao Inicial ( Prototipo )
!  MAR 2006 -SHSF-   Inclusao/revisao das rotinas para ler e gravar em formato 
!                    de ponto de grades (Load/save)(bin/ctl)_linear. 
!                    Adaptacao de funcoes de interpolacao (1998 - MAER-CTA-IAR)
!  2009      -SHSF-   Criacao do tipo ctldef para tratar a leitura da definicao do
!                    ctl em uma estrutura separada a da matriz de dados observacionais
!  2009/10/02 SHSF-  Acrescido funcao writectl. Modificado openr, e acrescido funcionalidade
!                    para tratamento adequado de TDEF, com conversao para o calendario juliano
!  2010/01/14 SHSF - Revisao das rotinas openw_mgrads e criancao de uma interface para estas rotinas
!  2010/03/06 SHSF - Tratamendo de dados que estao acima do valor missing 
!  2010/04/14 SHSF - Revisado writectl : YDEF
!  2013/11    SHSF - Acrescido novas rotinas readbin2 e writebin2 e feito revisao na forma de nomeancao 
!                    dos arquivos
!  2013/12/13 SHSF - Acrescentado defesa para erro de leitura de arquivo binario. Agora o programa nao para, mas atribui 
!                    a todos os pontos de grade o valor indefinido
!  2016/03/24 SHSF - Modificado calculo do valor de fundo de escala para tornar mais próximo do valor indefinido. Somente na rotina readbin1
!  2017/08/04 SHSF - Corrigido BUG na leitura de aquivos grads que nao sao templates. Ainda na verificado a gravacao
!  2017/10/18 SHSF - Incluido gravacao de CTL automatico para cada binario (independente ou nao de ser template)
!  2022/06/13 SHSF - Use of forecast time representation in the file name 
!  2022/07/19 SHSF - Use undefvar value ( not undef value from ctl) to calculate maximum and minimuns values 
!  2022/08/11 SHSF - the Error_Code_MGRADS public variable was created to indicate if an error ocurred when a file has been read 
!  2022/11/09 SHSF - New function: vertical_level_index
!  2022/11/30 SHSF - New function: Current_Filename.
!  2023/12/08 SHSF - bug fixed : getpath ctlname in openr_mgrads
!  2024/07/28 SHSF - improvement:  Force template options when reading multiple bin files with specific window and time range



MODULE MGRADS


 USE DATELIB
 USE stringflib 
 implicit none

 PRIVATE


!{ Tipo identificacao de estacao
type stidtype
   character(len=8):: cod
   real::lat
   real::lon
   integer::nlev
end type  
!}


!{ Tipo definicoes de arquivos binarios do grades
type bindef
   character(len=8),POINTER  ::code(:)   !Codigo/sigla da variavel 
   character(len=255),POINTER::name(:)   !Nome da variavel 
   character(len=1024)       ::ctlname   !Nome do arquivo descritor (CTL) 
   character(len=1024)       ::binname   !Nome do arquivo de dados binarios (BIN)
   character(len=80)         ::tlable           
   integer,pointer           ::varlevs(:)! Numero de niveis verticais de cada variavel
   integer                   ::imax      !Numero de pontos de grade na direcao x
   integer                   ::jmax      !Numero de pontos de grade na direcao y
   integer                   ::kmax      !Numero de niveis verticais (maximo)  
   integer                   ::tmax      !Numero de passos de tempo 
   integer                   ::fct       !Forecast period identification  (hours ?)
   integer                   ::nvars     !Numero de variaveis 
   integer                   ::nvarsmax  !Numero maximo de variaveis (para alocacao) 
   real                      ::dlat      !Tamanho do elemento de grade na direcao das latitudes
   real                      ::dlon      !Tamanho do elemento de grade na direcao das longitude
   real,pointer              ::lev(:)    !Valores dos Niveis verticais 
   real,pointer              ::lat(:)    !Valores de latitude
   real,pointer              ::lon(:)    !Valores de longitude
   real*8                    ::time_init !Data juliana inicial em dias e decimos de dias (start time ?)
   real*8                    ::time_step !Passo de tempo em dias e decimos de dias 
   real                      ::undef     !Valor indefinido
   integer*8                 ::nregtime  !Numero de registros por passo de tempo 
   integer*8                 ::NREG      !Numero total de registros
   integer*8                 ::recsize   !Tamanho dos resgistros (bytes)
   integer*8                 ::lof        !Tamanho do arquivo (bytes)
   integer                   ::horizontal ! Identificacao da tipo de coordenada  horizontal
                                          ! 1 = Lat e lon em graus 2 = Distancia em metros
   integer                   ::vertical   !Identificacao da variavel da coordenada  vertical
   integer                   ::mtype         !Tipo de modelo: 0=Modelo global 1 = modelo regional

   logical                   ::vcoords_from_ctl    !Se .true. = Coordenadas verticais provenientes do ctl 
   logical                   ::xrev                !Indica se o eixo x esta em ordem reversa
   logical                   ::yrev                !Indica se o eixo y esta em ordem reversa	
   logical                   ::optemplate          !Se .true. indica template 
   character(len=80)         ::title               !Titulo
end type 
integer                      :: error_code_mgrads=0
integer                      :: verbose=0
character(len=20)            :: version="2024-07-28"
!}

!{ Subrotinas PUBLICAS
   PUBLIC OPENR_mgrads    ! Abertura logica do arquivo para leitura
   
   PUBLIC READBIN_mgrads  ! Leitura do arquivo aberto por OPENR
   interface READBIN_mgrads
     module procedure readbin1
     module procedure readbin2
   end interface 

   PUBLIC writectl_mgrads ! Grava arquivo CTL
   PUBLIC ADDVAR_mgrads   ! Adciona uma variavel a um arquivo aberto

   PUBLIC WRITEBIN_mgrads ! Grava uma variavel ou todo um conjunto de dados para um tempo t
   interface WRITEBIN_mgrads
      module procedure writebin1 !(bf,varcode,t,xyz)
      module procedure writebin2
   end interface

   PUBLIC CLOSE_mgrads    ! Close file 
   PUBLIC varlevs_mgrads  ! Retorna o numero de niveis de uma variavel
   PUBLIC varindex        ! Retorna a posicao de uma variavel no ctl 
   PUBLIC BINDEF
   PUBLIC undefval
   public datetime_mgrads
   PUBLIC overflow_err
   PUBLIC conv_undef_mgrads
   PUBLIC newgrid_mgrads
   PUBLIC verbose_mgrads
   PUBLIC error_code_mgrads
   PUBLIC vertical_level_index
   public current_filename

   PUBLIC OPENW_mgrads    ! Abertura logica do arquivo para gravacao
   interface OPENW_mgrads
     module procedure  OPENW_mgrads1 !call OPENW_mgrads(bf,outfile,imax,jmax,kmax,nvarmax,optemplate)
     module procedure  OPENW_mgrads2 !call OPENW_mgrads(bf,outfile,imax,jmax,kmax,nvarmax)
     module procedure  OPENW_mgrads3 !call OPENW_mgrads(bf,outfile,bi,nvarmax,levs,nlevmax)
     module procedure  OPENW_mgrads4 !call openw_mgrads(bf,outfile,nvarmax)
   end interface
!}

   real,parameter::undefval=-8388607e31
   integer,parameter::record_size_unit = 4 ! Unidade de tamanho do registro [ 4 = bytes ou 1 = Word ] conforme compilador 
   integer*4::blk !  =0 -> unbloked   >0 : blocked 
 CONTAINS
 



!==============================================================================!
! openr_mgrads   | Le dados do arquivo ctl                                |SHSF!
!==============================================================================!
! Exemplo de uso
!   character(len=256)::ctlname
!   type(bindef)::bf
!   call openr_grads(bf,ctlname)
!
!------------------------------------------------------------------------------
!HISTORY
!   Com essa modificacao, um determinado modelo de ctl pode ser passado com argumento 
!   ao inves de um nome fixo de ctl. Isso e uma forma mais generalizadas de 
!   ler arquivos binarios utilizando controle de tempo 
!
!   20131118 - A terceira coluna na secao de variaveis do ctl nao e lida 

 subroutine verbose_mgrads(verb)
  integer,intent(in)::verb
  verbose=verb
 end subroutine
 
 SUBROUTINE OPENR_mgrads(bf,ctlname,d1,d2,ts,fct_in)

 !{ Interface
    character(len=*),         intent(in) ::ctlname
    type(bindef),             intent(out)::bf
    real*8,optional,          intent(in) ::d1,d2     ! period in days and fractions of days
    integer,optional,         intent(in) ::ts        ! time steps
    integer,optional,         intent(in) ::fct_in    !Forecast time 
 !}

!{ Local Variables
   character(len=255)               ::linha,linha0  ! Uma linha de texto
   character(len=255),dimension(200)::w             ! Palavras de uma linha de texto
   character(len=5)                 ::tstp          ! time step
   integer                          ::nw            ! Numero de palavras em w
   integer*2                        ::i,II,jj
   integer                          ::nlevs
   integer                          ::err
   REAL                             ::AUX
   integer                          ::iaux
   integer                          ::blkbytes
   integer                          ::FILE_SIZE !Size of binary file
   logical                          ::lexist
   character(len=2)                 ::mm,dd,hh  ! Ano, mes,dia e hora
   character(len=4)                 ::yy        ! Ano, mes,dia e hora
   character(len=1024)              ::ctlname_in  
   integer                          ::nts       ! Number of time stepsexample_namelist.nml
   real*8                           ::stime     ! start time
   character(len=3)                 ::fct
!}

   BF%YREV=.false.
   BF%XREV=.false.
   bf%optemplate=.false.
   ctlname_in=ctlname
   
   nts=0
   close(8)
 
   !--------------------------------------------------------
   ! Obter data inicial e timestep caso fornecido
   ! ( so considerar se ts > 0). 
   !--------------------------------------------------------   
    if (present(ts)) then 
       if (ts>0) then
         stime=d1  
	      nts=int((d2-d1)/(real(ts)/24.0))+1
       end if
    end if	 
    if (present(fct_in)) then 
	bf%fct=fct_in
    else
	bf%fct=0
    end if
    !---------------------------------------------------------
    ! Modificar nome do arquivo CTL caso tenha data informacao
    ! de data indicada por (%) no nome do arquivo 
    !---------------------------------------------------------   
    if (nts > 0 ) then  
         ctlname_in=current_filename(ctlname_in,stime,bf%fct);
         bf%optemplate=.true.
         print *,":MGRADS: New period from ",grdate(d1)," to ",grdate(d2)," inc=",ts,"hs  ", nts," time steps"
         print *,":MGRADS:ctlname_in=",trim(ctlname_in)
     elseif (index(ctlname_in,"%")>0) then 
	    ctlname_in=current_filename(ctlname_in,stime,bf%fct);
     end if
      
   
   inquire (file=ctlname_in,exist=lexist,size=file_size)
       if(.not.lexist) then
         print *, ":MGRADS: Error! File not found"
         print *, ":MGRADS: File=",TRIM(ctlname_in)
         stop
       else
         bf%ctlname=ctlname_in 
         print *, ":MGRADS: Openning ",TRIM(bf%ctlname)
       end if
!------------------
! reading ctl file 
!------------------
!{
   open (8,file=(bf%ctlname),status='old')
 
  
10 read(8,'(a)',end=999) linha
      linha0=linha
      linha=ucases(linha)
      !-------------------------------------------------
      ! Get binnary filename 
      !------------------------------------------------
      
        if (index(linha,"DSET")>0)then
         call sep_words(linha0,w,nw)
         bf%binname=w(nw)
         bf%binname=replace(bf%binname,"^",getpath(bf%ctlname))	

        if (nts>0) then
           print *,"x=",trim(bf%binname)
           iaux=index(ctlname,".ctl")
           bf%binname=ctlname(1:iaux)//"bin"
           print *,"xxxx=",trim(bf%binname)
           bf%optemplate=.true.
        end if

        end if


     !}

     !----------------------------------------------------
     ! Get time definition from TDEF and/or Privided TIME
     !----------------------------------------------------
     if (index(linha,"TDEF")>0)then
      
        call tdefconvert(linha,bf%time_init,bf%time_step,bf%tmax)
        
        if (nts>0)bf%time_init=stime
        if (nts>0)bf%tmax=nts
	if (nts>0)bf%time_step=ts/24.0
      
        bf%tlable=linha

        IF (bf%tmax<0) then
           print *,":MGRADS: Error close TDEF in", trim(bf%ctlname)
           stop
        end if 
        
     end if

    
      IF (index(linha,"YREV")>0) then
        bf%YREV=.true.
      end if
      if (index(linha,"XREV")>0)  then
        bf%XREV=.true.
      end if

      if ((index(linha,"TEMPLATE")>0).and.(index(linha,"OPTIONS")>0))  then
        bf%optemplate=.true.
      end if
    

      if (index(linha,"UNDEF")>0)then
       call sep_words(linha,w,nw)
       read(w(nw),*)bf%undef
      end if  
 
     !----------------------------------------
     ! obtaining the definitions of the axes
     !---------------------------------------
     !{
     !{XDEF Definicao do eixo X
      if (index(linha,"XDEF")>0)then
        call sep_words(linha,w,nw)
        bf%imax=val(w(2))
        allocate(bf%lon(bf%Imax),STAT=ERR)
        if (err>0) then 
          print *,":MGRADS: Error allocating lon"
          stop 
        end if 

        IF (INDEX(W(3),"LEVELS")>0) THEN
          CALL GETLEVELS(W,NW,bf%imax,bf%LON)
          bf%DLON=0
        ELSE
          AUX=VAL(W(5))
          bf%LON(1)=VAL(W(4))
          bf%DLON=val(W(5))
          DO II=2,bf%IMAX
            bf%LON(II)=bf%LON(II-1)+AUX
          END DO
        END IF
      end if
     !}

     !{ YDEF Definicao do eixo Y
      if (index(linha,"YDEF")>0)then 
       call sep_words(linha,w,nw)
       bf%jmax=val(w(2))
       allocate(bf%lat(bf%jmax),STAT=ERR)
       if (err>0)then 
         print *,":MGRADS: Error allocating lat"
         stop 
       end if 
           
        IF (INDEX(W(3),"LEVELS")>0) then 
          CALL GETLEVELS(W,NW,bf%jmax,bf%LAT)
          bf%DLAT=0
          else
          AUX=VAL(W(5))
          bf%LAT(1)=VAL(W(4))
          bf%DLAT=val(W(5))
          DO II=2,bf%JMAX
           bf%LAT(II)=bf%LAT(II-1)+AUX
          END DO
       END IF
     end if
     !}

    !{ ZDEF Definicao do exio Z
    if (index(linha,"ZDEF")>0)then 
      call sep_words(linha,w,nw)
      bf%kmax=val(w(2))
      !{ Obtendo niveis isobaricos 
       allocate(bf%lev(bf%kmax),STAT=ERR)
        if (err>0) then 
          print *,":MGRADS: Error allocating lev"
          stop 
        end if
        IF (INDEX(W(3),"LEVELS")>0) CALL GETLEVELS(W,NW,bf%Kmax,bf%LEV)
      !}
    end if
   !}

     !-------------------------------------------
     ! obtaining the definitions of the variables
     !-------------------------------------------
     !{
        if ((index(linha,"VARS")>0).and.(index(linha,"DSET")==0))then 
				call sep_words(linha,w,nw)
				bf%nvars=val(w(2))
				!if (associated(bf%varlevs)) deallocate(bf%varlevs)
				allocate(bf%varlevs(bf%nvars),STAT=ERR)
				!write(*,' (":MGRADS: Allocating bf%code[",i4.4,"]")')bf%nvars
				allocate(bf%code(bf%nvars),bf%name(bf%nvars),STAT=ERR)
				if (ERR>0) then 
				    print *,":MGRADS: Error allocating bf%code"
				    stop 
				end if
				do i=1,bf%nvars
						read(8,'(a)') linha
						call sep_words(linha,w,nw)
						bf%varlevs(i)=val(w(2))
						bf%code(i)=w(1)
						bf%name(i)=""
						do ii=4,nw
							bf%name(i)=trim(bf%name(i))//" "//trim(w(ii))
						end do
                                            !if (verbose==3) then 
                                           !   print *,i,bf%code(i),bf%varlevs(i),trim(bf%name(i))
                                            !end if 
					end do
					goto 999
				end if
			
		  !}
	  !{ 
	

		 goto 10
   999  continue 
		
		

      close(8) 
      !bf%binname=replace(bf%binname,"%y4",yy)
      !bf%binname=replace(bf%binname,"%Y4",yy)
      !bf%binname=replace(bf%binname,"%m2",mm)
      !bf%binname=replace(bf%binname,"%d2",dd)
      !bf%binname=replace(bf%binname,"%h2",hh) 
      print *,":MGRADS: binfile =",trim(bf%binname)


    !{ Determinacao do tamanho do registro 
	blk=0 ! 0 = unblocked, 1 = blocked
	bf%recsize=(bf%imax*bf%jmax+blk)*4 !record_size_unit
    !}
 	!{ Obter tamanho de um registro de tempo
	 
	bf%nregtime=0 
	DO II=1,bf%nvars
		IF (bf%VARLEVS(II)==0) THEN
			bf%nregtime=bf%nregtime+1
		ELSE
			bf%nregtime=bf%nregtime+bf%VARLEVS(II)
		END IF
	END DO
	bf%NREG=bf%nregtime*bf%tmax
	bf%LOF=bf%recsize*bf%NREG
	bf%ctlname=ctlname
	end subroutine
!==============================================================================!
! varlevs_mgrads | Fornece o numero de niveis de uma variavel              |SHSF!
!==============================================================================!
 function varlevs_mgrads(bf,varcode);integer::varlevs_mgrads

      
   !{  vaiaveis da interface 
	type(bindef),intent(inout)::bf
	character(len=*),intent(in)::varcode    
	!}
	
	 !{- variaveis locais	!
	  integer:: v                    ! Indice da variavel a ser lida (1 a xyz%nvars)
  
!{ Verificando existencia da variavel e tambem se instante de tempo e valido

	v=varindex(bf,varcode)

	if (v==0) then 
	    print *,":MGRADS: Error: Var='",trim(varcode),"' not found in the ctl =",trim(bf%ctlname);
            varlevs_mgrads=-1
	    return
	end if

	varlevs_mgrads=bf%varlevs(v)

	end function
	!} 

  
!==============================================================================!
! ReadBin1 | Carrega campos no formato do grads                           |SHSF!
!==============================================================================!
!     Valores missing = -9.99e33                                               !
! Esta subrotina carrega campos de modelos no formato binario do grads         !
! Os campos sao fornecidos em duas estruturas de dados:                        !
!  a) Com os campos de superficie (xy)                                         !
!  b) Com os campos de altitude (xyz)                                          !
!                                                                              !  
!  O programa principal deve conter as declaracoes                             !
!                                                                              !
!  use MGRADS                                                                  !
!  use datelib                                                                 !
!  type(xygrad):: xy         != Dados de superficie (x,y,nvar)                 !
!  type(xyzgrad) :: xyz     != Dados de altitude (x,y,z,nvars)                 !
!                                                                              !
!==============================================================================!
! HISTORICO                                                                    
!      DEZ 2002 -  S�gio H.S. Ferreira - prototipo                           
!      MAR 2006 -  Sergio H.S. Ferreira - Revisao e adaptacao                  
!      2009 - SHSF - LEITURA DESCLOCADA 
!==============================================================================
! Nota: Aparentemente existem arquivos binarios do grades que possuem ou
!       nao a blocagem do fortran. - Por conveniencia removemos a blocagem.
!       Esta é uma parte que precisa ser melhor vista. 
 subroutine readbin1(bf,varcode,t,xyz,err,pout)


   !{  vaiaveis da interface 
	type(bindef),         intent(inout)::bf
	real,dimension(:,:,:),intent(inout)::xyz  ! Uma variavel (x,y,z)
	integer,                 intent(in)::t    ! indice relativo ao tempo  t=1,2,.., tfinal
	character(len=*),        intent(in)::varcode   
	integer,optional,       intent(out)::err  ! 0 Leitura Normal 1 = Com erro de leitura
	logical,optional                   ::pout ! Se present imprime log de saida 
	!}
	   
	 !{- variaveis locais	!
	  integer            :: v       ! Indice da variavel a ser lida (1 a xyz%nvars)
          integer*8          ::recsize  ! Blocagem
	  integer            ::x,y,z
	  integer*8          ::irec2
	  integer            ::nvars,i,i1,i2,is,j1,j2,js,k1,k2
	  real               ::xyzmax,xyzmin,scalemax,scalemin
	  real*8             ::jdate    ! Data e horas correntes no calendario juliano
	  character(len=2)   ::mm,dd,hh ! Ano, mes,dia e hora
	  character(len=4)   ::yy       ! Ano, mes,dia e hora
	  character(len=1024)::binname
          logical            ::lexists
	  integer            ::ioerr  
	  integer*8          ::file_size,file_size2
	  !
  !{ para OMP (NECESSARIO ESTAR DECLARADO DENTRO DA SUBROTINA
       INTEGER            :: CHUNK             ! Tananho da peça de trabalho
       INTEGER            :: NTHREADS
       INTEGER            :: TID
       INTEGER            :: OMP_GET_NUM_THREADS  ! Funcao do omp que retorna o numero de processadores 
       INTEGER            :: OMP_GET_THREAD_NUM   ! fUNCAO DO OMP que retorna o numero do processador corrente
 !}
	  !{ Inciando Variaveis
	  !# SHSF Modificado formulara para calculo do valor maximo de fundo de escala para tornar mais justo. 
	if (present(err)) err=0  
	error_code_mgrads=0
	irec2=0;x=0;y=0;z=0;i=0;nvars=0
	scalemax=10.0**(int(log(abs(undefval)+1)/log(10.0)))-1
	scalemin=-scalemax
	xyzmin=scalemax
	xyzmax=scalemin
	xyz(:,:,:)=bf%undef
	

   


!{ Verificando existencia da variavel e tambem se instante de tempo e valido

	v=varindex(bf,varcode)

	if (v==0) then 
	    print *,":MGRADS: Error: Var='",trim(varcode),"' not found in ctl",trim(bf%ctlname);
	    stop
	end if

	if ((t>bf%tmax).or.(t<1)) then 
	    print *,":MGRADS: Error: Invalid time step"
	    stop
	end if
	!} 
!{ Verificando se e um arquivo TEMPLATE
    if (bf%optemplate) then
        
	jdate=bf%time_init+bf%time_step*real(t-1)
	binname=current_filename(bf%binname,jdate,bf%fct)
	
        if (verbose>0) print *,":MGRADS:Template t=",t,"var=",trim(varcode)," ",trim(binname)
    else
      binname=bf%binname
      if (verbose>0) print *,":MGRADS:Binname=",trim(binname)
      irec2=(t-1)*bf%nregtime
      if (verbose>0) print *,":MGRADS:t=",t," rec=",irec2,"recsize=",recsize
    end if
!}

     recsize=(bf%imax*bf%jmax+blk)*record_size_unit
    !{ Lendo arquivo binario
      inquire(FILE = binname, EXIST = lexists,size=file_size)
        file_size2=bf%nregtime*bf%recsize

	!bf%NREG=bf%nregtime*bf%tmax
	!bf%LOF=bf%recsize*bf%NREG
	!bf%ctlname=ctlname
	
        if (.not.lexists) then 
          print *,""
          print *,":MGRADS:ReadBin1: Error opening ",trim(binname)
          print *,":MGRADS:ReadBin1: File not found."
	  if (present(err)) err=1
	  error_code_MGRADS=1
          xyz(:,:,:)=bf%undef
          goto 444
       end if
       if (file_size<file_size2) then 
          print *,""
          print *,":MGRADS:ReadBin1: Error in ",trim(binname)
          print *,":MGRADS:ReadBin1: Incorrect size =",file_size," Expected Value=(",file_size2,")"
	   if (present(err)) err=1
	  xyz(:,:,:)=bf%undef
	  goto 444
       end if 
       open(8,file=binname,status='unknown',access='DIRECT',recl=recsize)
      
    !{ Caso o arquivo grads tena sido escrito em ordem reversa em x ou y, inverte a ordem dos indices 
       if (BF%xrev) then 
          i1=bf%imax;i2=1;is=-1
       else

          i1=1;i2=bf%imax;is=1
       end if	
         
       if (BF%yrev) then 
          j1=bf%jmax;j2=1;js=-1
       else
          j1=1;j2=bf%jmax;js=1
       end if	  
      !}     
	  nvars=0 
	  
	  
	!{ Loading  Up Air and surface DATA 
	if (bf%optemplate) then
	  IREC2=VARREG(bf,v,1) ! ser for template o tempo e sempre 1 para o calculo do registro 
	else
	  IREC2=VARREG(bf,V,t)
	end if

	if (bf%varlevs(v)>0) then 
		k1=1
		k2=bf%varlevs(v)
	elseIF (bf%varlevs(v)==0) then
		k1=1
		k2=1
	elseif(present(err)) then
		err=1
		return
	else
	      print *,"MGRADS.READBIN: ERROR: "//TRIM(VARCODE)//" NOT FOUND"
	end if
	
        if ((size(xyz,1)<i2).or.(size(xyz,2)<j2).or.(size(xyz,3)<k2)) then 
          print *,":MGRADS: ERROR! Upper bound error reading ",trim(bf%binname)
          write (*,44)v,trim(varcode),i2,j2,k2
          write (*,45)v,trim(varcode),size(xyz,1),size(xyz,2),size(xyz,3)
          stop
 44      format (" :MGRADS: File Dim:",i3,1x,a," imax,jmax,kmax=",3(i4))
 45      format (" :MGRADS: Expected:",i3,1x,a," xmax,ymax,zmax=",3(i4))
        end if 

	do z=k1,k2
	    irec2=irec2+1
	    read(8,rec=irec2,iostat=ioerr)((xyz(x,y,z),x=i1,i2,is),y=j1,j2,js)
	    if (ioerr/=0) then 
	      if (present(err)) err=1
	      print *,":MGRADS: Error reading ",trim(bf%binname)
	      return
	    end if
	end do
	
	      
	close(8)	
	!{Calculo do valor medio e redefinicao do valor indefinido
	 CHUNK=1 
	  !$OMP PARALLEL SHARED (xyz) PRIVATE(x,y,z)
	   !$OMP DO SCHEDULE(DYNAMIC,CHUNK)
	do x=i1,i2,is
		do y=j1,j2,js
			do z=k1,k2
			if ((xyz(x,y,z)<scalemax).and.(xyz(x,y,z)>scalemin))  then 
				if(xyz(x,y,z)>xyzmax) xyzmax=xyz(x,y,z)
				if(xyz(x,y,z)<xyzmin) xyzmin=xyz(x,y,z)
			else   
			      xyz(x,y,z)=bf%undef
			end if
			end do
		end do
	end do
     !$OMP END DO NOWAIT
     !$OMP END PARALLEL
	  !}
	
    444  if  (present(pout)) print *, ":MGRADS:",trim(bf%code(v))," TIME=",t," MIN=",xyzmin," MAX=",xyzmax 		
	end subroutine 
!==============================================================================!
! ReadBin2 | Carrega campos no formato do grads                           |SHSF!
!==============================================================================!

 subroutine readbin2(bf,t,rdata)


   !{  vaiaveis da interface 
	type(bindef),         intent(inout)::bf
	real,dimension(:),      intent(out)::rdata ! data from one register 
	integer,                 intent(in)::t     ! indice relativo ao tempo  t=1,2,.., tfinal
	
	!}
	   
	 !{- variaveis locais	!
          character(len=4)   ::yy
          character(len=2)   ::mm,dd,hh
          real*8             ::jdate
	  integer*8          ::recsize  ! Blocagem
	  integer*8          ::irec2
	  character(len=1024)::binname
          logical::lexists
          integer            ::z,nreg
	  integer            ::ioerr
	  integer*8          ::file_size,file_size2
        !{ Verificando existencia da variavel e tambem se instante de tempo e valido
        if ((t>bf%tmax).or.(t<1)) then 
	    print *,":MGRADS: Error: Invalid time step"
	    stop
	end if
	!} 

   !{ Verificando se e um arquivo TEMPLATE
    if (bf%optemplate) then
	jdate=bf%time_init+bf%time_step*real(t-1)
	write(yy,'(i4)')year(jdate)
	write(mm,'(i2.2)')month(jdate)
	write(dd,'(i2.2)')day(jdate)
	write(hh,'(i2.2)')hour(jdate)
	binname=replace(bf%binname,"%y4",yy)
	binname=replace(binname,"%m2",mm)
	binname=replace(binname,"%d2",dd)
	binname=replace(binname,"%h2",hh)
        print *,":MGRADS:Template t=",t," ",trim(binname)
    else
      binname=bf%binname
      print *,":MGRADS:Binname=",trim(binname)
    end if
!}
    

    !{ Lendo arquivo binario
      inquire(FILE = binname, EXIST = lexists,size=file_size)
      file_size2=bf%nregtime*bf%recsize
        if (.not.lexists) then 
          print *,""
          print *,":MGRADS:ReadBin2: Error opening ",trim(binname)
          print *,":MGRADS:ReadBin2: File not found."
          stop
       end if
        if (file_size<file_size2) then 
          print *,""
          print *,":MGRADS:ReadBin2: Error in ",trim(binname)
          print *,":MGRADS:ReadBin2: Incorrect size =",file_size," Expected Value=(",file_size2,")"
	  stop
       end if 
        nreg=bf%nregtime*bf%imax*bf%jmax
	!nreg=bf%recsize*bf%nregtime  ! Number of records per time block 
        print *,"Blksize=",nreg
        print *,"RecSzUni=",record_size_unit
	open(8,file=binname,status='unknown',access='DIRECT',recl=record_size_unit)  
	
	irec2=(t-1)*nreg   ! Initial record 
	do z=1,nreg
		irec2=irec2+1
		read(8,rec=irec2,iostat=ioerr)rdata(z)
		if (ioerr/=0) then 
	         print *,":MGRADS:readbin2: Error reading ",trim(bf%binname)
		 return
		end if
        end do
        print *,"Ok"
	      
	close(8)	
	end subroutine 

!------------------------------------------------------------------------------
! tdefconvert | converte  formato de tdef                              | SHSF |
!------------------------------------------------------------------------------ 
! Converte data no format do grads para data juliana e obtem demais parametros
!------------------------------------------------------------------------------

 subroutine tdefconvert(tdef,jdate,jstp,nj)
 !{variaveis de inteface
	character(len=*),intent(in)::tdef
	real*8,intent(out)::jdate ! Data inicial no calendario juliano
	real*8,intent(out)::jstp  ! passo de tempo em dias e fracoes de dias
	integer,intent(out)::nj   ! numero de passos de tempo

 !}
	character(len=10)::tstp
	character(len=15),dimension(15)::w
	integer::nw,l
	character(len=80)::linha
	character(len=15)::cdate
	jdate=0
	linha=tdef

	call sep_words(linha,w,nw)

	!{ obteecao do numero de passos de tempo
	nj=val(w(2))
	!}

	!{ Obtencao do passo de tempo
	tstp=ucases(trim(w(nw)))
	
	if (index(tstp,"HR")>0) then 
		jstp=val(tstp)/24.0
	elseif (index(tstp,"MN")>0) then 
		jstp=val(tstp)/24.0/60.0
        else
                jstp=val(tstp)
	end if
	!}
	!{ Obtendo data no calendario juliano
		
	  jdate=fjulian(w(4))
	
	!}  

 end subroutine

!------------------------------------------------------------------------------
! GETLEVELS |                                                         |SHSF   |
!-----------------------------------------------------------------------------
 ! Esta subrotina e chamada exclusivamente por LOADCTL_LINEAR, PARA
 ! fazer a leitura de XDEF, YDEF e ZDEF no caso destas nao serem 
 ! definidas como "LINEAR" e sim como "LEVES"
 !
 !  W e NW corresponde ao resto de linha anterior e poderam conter 
 !  alguns niveis caso NW > 3. Neste caso e feito o resto da leitura
 !  destas variaveis. Caso ainda existam variaveis a serem lidas entao 
 !  continua-se o processo 
 !
 !  
  SUBROUTINE GETLEVELS(W,NW,JMAX,LVAR)
	character(len=*),dimension(:),intent(inout)::W
	integer,intent(inout)::nw
	integer,intent(in)::jmax ! Numero maximo de niveis 
	real,dimension(:),intent(inout)::LVAR

	integer:: jj,II
	CHARACTER(LEN=255)::LINHA

	JJ=0

	
	IF (NW>3) THEN
		DO II=4,NW
			JJ=JJ+1
			LVAR(JJ)=VAL(W(II))
		END DO
	end if

	DO WHILE (JJ<JMAX)
		read(8,'(a)')LINHA
		call sep_words(linha,w,nw)
		do ii=1,nw
			JJ=JJ+1
			LVAR(JJ)=val(w(ii))
		end do
	END DO
		
END SUBROUTINE

!==============================================================================!
! VARREG | REGISTRO INICIAL DOS DADOS DE UMA VARIAVEL                     |SHSF!
!==============================================================================!
!  FUNCAO PRIVADA                                                              !
!                                                                              !
!  Calcula a posicao de uma variavel dentro de um arquivo grads, com base      !
!  nas informacoes lidas por LOADCTL_LINEAR ( ESTRUTURA xyz ) e indice         !
!  que identifica a variavel                                                   !
!                                                                              ! 
!                                                                              !
!==============================================================================! 
!{
 function VARREG(bf,V,t);INTEGER*4::VARREG

 !{ Variaveis de interface
   TYPE(bindef),INTENT(IN)::bf  ! Estrutura de dados lidas por LOADCTL_LINEAR
   INTEGER,INTENT(IN)::V	    ! Indice da variavel 
   integer,intent(in)::t       ! Indice do instante de tempo ( t = 1,2,.., tfinal)
 !}
 !{ Variaveis locais
   INTEGER*4::S
   integer :: II
 !}

   IF ( V>1 )THEN
      S=bf%nregtime*(t-1) 
      DO II=1,V-1
        IF (bf%VARLEVS(II)==0) THEN
          S=S+1
        ELSE
          S=S+bf%VARLEVS(II)
        END IF
      END DO
      VARREG=S
   ELSE
      VARREG=bf%nregtime*(t-1)
   END IF
END FUNCTION
!}
!==============================================================================!
! openw_mgrads2   | Abre arquivo para gravacao no formato do grads        |SHSF!
!==============================================================================!
! Como base nas definicoes de outro arquivo CTL do grads (bi)
!
! Este rotina aproveita todas as propriedades de bi para formar bf,
! com excessao do numero de niveis na coordenada principal 
!-----------------------------------------------------------------------------
 subroutine openw_mgrads2(bf,outfile,bi,nvarmax)
!{ Variaveis da interface
   type(bindef),intent(inout)::bf
   character(len=*),intent(in)::outfile
   type(bindef),intent(in)::bi
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
!} 
   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
   bf%fct=bi%fct
   call openw_mgrads(bf,outfile,bi%imax,bi%jmax,bi%kmax,nvarmax,bi%optemplate,bi%time_init,bi%fct)
   bf%lat=bi%lat
   bf%lon=bi%lon
   bf%lev=bi%lev
   bf%dlat=bi%dlat
   bf%dlon=bi%dlon
   bf%tlable=bi%tlable
   bf%undef=undefval
   bf%optemplate=bi%optemplate
   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
!}
end subroutine

!==============================================================================!
! openw_mgrads3   | Abre arquivo para gravacao no formato do grads        |SHSF!
!==============================================================================!
! Una como base as definicoes de outro arquivo CTL do grads (bi)
!
! Este rotina aproveita todas as propriedades de bi para formar bf,
! com excessao do numero de niveis na coordenada principal e do
! numero de variaveis maximas
!-----------------------------------------------------------------------------
!{
 subroutine openw_mgrads3(bf,outfile,bi,nvarmax,levs,nlevmax)

!{ Variaveis de interface
   type(bindef),intent(inout) ::bf       ! Parameters definion of outfile
   character(len=*),intent(in)::outfile  ! File name of outout file (without .ctl or .bin)
   type(bindef),intent(in)    ::bi       ! Parameter defintion from another ctlfile to be copy into bf 
   integer,intent(in)         ::nvarmax  ! Numero maximo de variaveis
   real,dimension(:)          ::levs     ! List of vertical levels [example: bi%lev(1:kmax)]
   integer,intent(in)         ::nlevmax  ! Maximum number of vertical levels 
!}
!{ Variaveis locais
   integer::k
!}

   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
   bf%fct=bi%fct
   call openw_mgrads(bf,outfile,bi%imax,bi%jmax,nlevmax,nvarmax)
   bf%lat=bi%lat
   bf%lon=bi%lon
   bf%kmax=nlevmax
   do k=1,bf%kmax 
      bf%lev(k)=levs(k)  
   end do
   bf%dlat=bi%dlat
   bf%dlon=bi%dlon
   bf%tlable=bi%tlable
   bf%undef=undefval
   bf%optemplate=bi%optemplate
   bf%time_init=bi%time_init
   bf%time_step=bi%time_step
   bf%fct=bi%fct
end subroutine
!}
!==============================================================================!
! openw_mgrads   | Abre arquivo para gravacao no formato do grads         |SHSF!
!==============================================================================!
!                                                                              !
! CTL file name cannot be formed with generic date.                            !
! If it is than replace the field (indicated with %) by the current date       !
!                                                                              !
! If it is a  template and the binary file don't have indication               !
! of date in the name  (%) than add theses indications                         !
!+-----------------------------------------------------------------------------!

 subroutine openw_mgrads1(bf,outfile,imax,jmax,kmax,nvarmax,optemplate,start_time,fct_in)
 !{ Variaveis da interface
   type(bindef),intent(inout)::bf 
   character(len=*),intent(in)::outfile
   integer,intent(in)::imax,jmax,kmax ! Dimensionamento da matriz em x,y e z
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
   logical,optional,intent(in)::optemplate ! True or false 
   real(8),optional,intent(in)::start_time
   integer,optional,intent(in)::fct_in
!}

!{ Variaveis locais
   logical::exists
   character(len=1024)::ctlname,binname
   character(len=4)::yy
   character(len=2)::mm,dd,hh
   character(len=3)::fct
   integer::ifct
!}


   bf%ctlname=trim(outfile)//".ctl"
   bf%binname=trim(outfile)//".bin"
   if (present(optemplate)) bf%optemplate=optemplate

   if (present(fct_in)) then
	ifct=fct_in
   else
	ifct=0
   end if

   !{ force the template option
   ! if (index(outfile,"%")>0) then
   !  bf%optemplate=.true.
   ! end if
   !}

   if ((bf%optemplate).and.(index(bf%binname,"%")==0)) then 
        bf%binname=trim(bf%binname)//"_"//char(37)//"Y4"//char(37)//"M2"//char(37)//"D2"//char(37)//"H2"
   end if
   !}



   bf%nvars=0
   bf%imax=imax
   bf%jmax=jmax
   bf%kmax=kmax
   bf%undef=undefval
   bf%recsize=bf%imax*bf%jmax
   bf%nvarsmax=nvarmax
   bf%nregtime=0
   bf%nreg=0
   bf%lof=0
   bf%fct=ifct
   call chk_ctlname(bf)
   
   allocate(bf%lon(bf%Imax))
   allocate(bf%lat(bf%jmax))
   allocate(bf%lev(bf%kmax))
   allocate(bf%varlevs(nvarmax))
   allocate(bf%code(nvarmax))
   allocate(bf%name(nvarmax))
   ctlname=trim(bf%ctlname)
   if (present(start_time)) then
       bf%time_init=start_time
       if( year(bf%time_init)<1900) then
          print *,":MGRADS:openw_mgrads1: Error on time definition"
          print *,":MGRADS:time=",grdate(bf%time_init)
          stop
       end if
   end if

    binname=bf%binname
    binname=current_filename(bf%binname,bf%time_init,ifct)

   inquire(FILE = binname, EXIST = exists)
   
     
   if (exists) then 
        call unlink(ctlname)
   else 
      print *,":MGRADS:OPENW: BINFILE=",trim(binname)
      print *,""
      open(8,file=binname,status='unknown')
      write(8,*)'teste'
      close(8)
    end if
    print *,":MGRADS:openw ctlfile=",trim(bf%ctlname)
    print *,":MGRADS:openw binfile=",trim(bf%binname)
   !}
end subroutine

!==============================================================================!
! openw_mgrads   | Abre arquivo para gravacao no formato do grads         |SHSF!
!==============================================================================!
! Exemplo de uso
 subroutine openw_mgrads4(bf,outfile,nvarmax)
 !{ Variaveis da interface
   type(bindef),    intent(inout)::bf 
   character(len=*),intent(in)   ::outfile
   integer,         intent(in)   ::nvarmax ! Numero maximo de variaveis
!}
!{ Variaveis locais
   logical::exists
   
!}

   bf%ctlname=trim(outfile)//".ctl"
   bf%binname=trim(outfile)//".bin"
   bf%nvars=0
   bf%undef=undefval
   bf%recsize=bf%imax*bf%jmax
   bf%nvarsmax=nvarmax
   bf%nregtime=0
   bf%nreg=0
   bf%lof=0
   allocate(bf%varlevs(nvarmax))
   allocate(bf%code(nvarmax))
   allocate(bf%name(nvarmax))
   
 
   inquire(FILE = bf%binname, EXIST = exists)
   
     
   if (exists) then 
     call unlink(bf%binname)
   else 
     print *,":MGRADS:OPENW: BINFILE=",trim(bf%binname)
     print *,""
     open(8,file=bf%binname,status='unknown')
     write(8,*)'teste'
     close(8)
   end if
   ! print *,":MGRADS:openw file=",trim(outfile)

  end subroutine
! 
!
!==============================================================================!
! addvar_mgrads   | Adiciona variavel a estrutura de gravacao             |SHSF!
!==============================================================================!
! Exemplo de uso
! 2014-01-20 Se uma mesma variavel for adicionada mais de uma vez, apenas
!            atualiza o numero de niveis da mesma
!
subroutine addvar_mgrads(bf,varcode,nlev,varname)
  !{ Variaveis da intertface
    type(bindef),intent(inout)   ::bf      ! Definicoes do arquivo binario
    character(len=*),intent(in)  ::varcode ! Codigo da variavel
    integer,intent(in)           ::nlev    ! Numero de niveis
    character(len=*),intent(in)  ::varname ! Descricao da Variavel
  !}
   integer::v
   logical::new
    new=.true.

    if (nlev>bf%kmax) then 
      print *,":MGRADS:ADDVAR: Error: nlev > kmax: (kmax,varcode,nlev)=",bf%kmax,trim(varcode),nlev
      stop
    end if
   !Varifica se a variavel ja foi adicionada
      do v=1,bf%nvars
        if (bf%code(v)==varcode) then
          new=.false.
          exit
        end if
      end do
   
    if ((bf%nvars<=bf%nvarsmax).and.new) then 
      bf%nvars=bf%nvars+1
      bf%code(bf%nvars)=varcode
      bf%varlevs(bf%nvars)=nlev
      bf%name(bf%nvars)=varname

      IF (bf%VARLEVS(bf%nvars)==0) THEN
        bf%nregtime=bf%nregtime+1
      ELSE
        bf%nregtime=bf%nregtime+bf%VARLEVS(bf%nvars)
      END IF

    elseif (bf%nvars<=bf%nvarsmax)then 
      bf%varlevs(v)=nlev
    else
      print *,":MGRADS: Error: Nvars=",bf%nvars, " > nvarmax = ",bf%nvarsmax
      print *,":MGRADS: Varname=",trim(varname)
      stop
    end if 

    !Descomente a linha a baixo para conferir os resultados 
    !print *,":ADDVARS_MGRADS:",bf%nvars,bf%code(bf%nvars)
    !
end subroutine
!==============================================================================!
! writebin1          | Gravar dados                                       |SHSF!
!==============================================================================!
! Exemplo de uso
subroutine writebin1(bf,varcode,t,xyz)
  type(bindef),intent(inout)         ::bf      ! binary file definitions
  character(len=*),intent(in)        ::varcode ! Variable code
  integer,intent(in)                 ::t       ! Time step
  real,dimension(:,:,:),intent(inout)::xyz     ! Data matriz for writing

  character(len=4)::yy
  character(len=2)::mm,dd,hh
  integer::v
  integer*4::irec2,k1,k2,recsize
  integer::x,y,z
  real::scalemax,scalemin
  character(len=1024)::binname
  character(len=10)::gdate ! Data gregoriana yyyymmddhh
  real*8::jdate
  character(len=3)::fct

! start
!{ Verificar se a variavel existe
	v=varindex(bf,varcode)
	if (v==0) then 
	    print *,":MGRADS: Error: Var='",trim(varcode),"' not found in Binfile=", trim(bf%binname);
	    call close_mgrads(bf)
	    stop
	end if
   !}
   !{ Verifica dimensoes de xyz
     if (size(xyz,1)<bf%imax) then 
		print *,":MGRADS_WRITEBIN: Erro: size(xyz,1)=",size(xyz,1),"< bf%imax=",bf%imax
		call close_mgrads(bf)
		stop
	endif
     if (size(xyz,2)<bf%jmax) then 
		print *,":MGRADS_WRITEBIN: Erro: size(xyz,2)=",size(xyz,2),"< bf%jmax=",bf%jmax
		call close_mgrads(bf)
		stop
	endif
     if (size(xyz,3)<bf%varlevs(v)) then 
	      print *,":MGRADS_WRITEBIN: Erro: size(xyz,3)=",size(xyz,3),"< bf%varlevs=",bf%varlevs(v),"Varcode=",varcode
	      call close_mgrads(bf)
	     stop
	endif
     if (t<1) then 
	      print *,":MGRADS_WRITEBIN: Erro: Time must be positive "
	      call close_mgrads(bf)
	      stop
	endif

    !}

 
	scalemax=10.0**(int(log(abs(undefval))/log(10.0))-1)
	scalemin=-scalemax


 
	if (bf%tmax<t) bf%tmax=t ! atualiza tempo maximo
	recsize=(bf%imax*bf%jmax)*record_size_unit
	bf%recsize=(bf%imax*bf%jmax)*record_size_unit

    if (index(bf%binname,"%")>0) then
        jdate=bf%time_init+bf%time_step*real(t-1)
	    binname=current_filename(bf%binname,jdate,bf%fct)
	    IREC2=VARREG(bf,V,1)
	else
	    bf%optemplate=.false.
	    binname=trim(bf%binname)
	    IREC2=VARREG(bf,V,t)
	end if
       
        if (index(binname,".")==0) binname=trim(binname)//".bin"
 
  !{ Saving arquivo binario 
    if(verbose>1) print *,":MGRADS:Saving ",trim(varcode),' in ',trim(binname)
    open(8,file=binname,status='unknown',access='DIRECT',recl=recsize)
   
    !print *,":MGRADS: t=",t,"v=",v,"IREC=",IREC2
    if (bf%varlevs(v)>0) then 
      k1=1
      k2=bf%varlevs(v)
    else
      k1=1
      k2=1
    end if
    if (verbose>2) print *,"k1,k2=",k1,k2

    do z=k1,k2
      do x=1,bf%imax
        do y=1,bf%jmax
          if (xyz(x,y,z)/=bf%undef) then 
            if ((xyz(x,y,z)>scalemax).or.(xyz(x,y,z)<scalemin))  then 
             if (verbose>2) then 
              print *,":MGRADS:Writebin: Overflow Error"
              print *," Var=",trim(varcode),"(",x,y,z,")=",xyz(x,y,z)
              end if
              xyz(x,y,z)=bf%undef
              !close(8)
              !stop
            end if	
          end if
        end do
      end do
     
      irec2=irec2+1
      write(8,rec=irec2)((xyz(x,y,z),x=1,bf%imax,1),y=1,bf%jmax,1)
      if (verbose>2) print *,"irec=",irec2
    end do 
    close(8)	
    
    call writectl_mgrads(bf,binname)
  !}
end subroutine
!==============================================================================!
! writebin2          | Gravar dados                                       |SHSF!
!==============================================================================!
! Exemplo de uso

subroutine writebin2(bf,t,rdata)


   !{  vaiaveis da interface 
	type(bindef),         intent(inout)::bf
	real,dimension(:),       intent(in)::rdata ! data from one register 
	integer,                 intent(in)::t     ! indice relativo ao tempo  t=1,2,.., tfinal
	
	!}
  !{ Variaveis locais
	
	integer            ::z,nreg,irec2
	
  !}
 
     if (t<1) then 
	      print *,":MGRADS_WRITEBIN: Erro: Time must be positive "
	      call close_mgrads(bf)
	      stop
	endif

     
	if (bf%tmax<t) bf%tmax=t ! atualiza tempo maximo

        nreg=bf%nregtime*bf%imax*bf%jmax
	!nreg=bf%recsize*bf%nregtime  ! Number of records per time block 
        print *,":MGRADS:Saving data in ",trim(bf%binname)
        print *,":MGRADS:Blksize=",nreg
        print *,":MGRADS:RecSzUni=",record_size_unit
	open(8,file=bf%binname,status='unknown',access='DIRECT',recl=record_size_unit)  
	
	irec2=(t-1)*nreg   ! Initial record 
	do z=1,nreg
		irec2=irec2+1
		write(8,rec=irec2)rdata(z)
        end do
        print *,":MGRADS: Done"
	      
	close(8)	

end subroutine
!==============================================================================!
! writectl_mgrads   | Gravar ctl                                         |SHSF !
!==============================================================================!
! Exemplo de uso
!
!   call writectl_mgrads(bf)          <- grava ctl conforme especficado anterioremente 
!                                        no bf, durante a abertura do arquivo 
!   call writectl_mgrads(bf,binname)  <- grava ctl, forcando o nome do ctl com base 
!                                        no binname. Utiliza o mesmo path do binname
!                                        so muda aterminacao (.bin para .ctl)
subroutine writectl_mgrads(bf,binname_in)
!{ Variaveis da interface
	type(bindef),intent(in)::bf
        character(len=*),optional,intent(in):: binname_in
!} 

!{ Variaveis locais
   real::xmin ! Longitude minima
   real::ymin ! 
   real::xmax ! 
   real::ymax !
   real::tmax
   character(len=3)::fct
   real*8::stime,ftime ! Data em dias e fracoes de dias a partir do ano zero (vide datelib)
   character(len=1024),dimension(100)::lines
   character(len=1024)::binname,ctlname
   character(len=4)::yy
   character(len=2)::mm,dd,hh
   integer*2::z,i,v
   real :: stpx
   real :: stpy 
   real :: stpt
   integer ::p,r,rt
   integer::rz
   logical usebinname
!}
   xmin=bf%lon(1);stpx=bf%dlon
   ymin=bf%lat(1);stpy=bf%dlat
   tmax=bf%tmax;stpt=bf%time_step*24
   stime=bf%time_init
   ftime=stime+real(bf%fct)/24.0
   usebinname=.false.
   
   
   ! Mudar padrao de 0:360 para -180:180
   if (xmin>180) xmin=xmin-360.0
   
   if (present(binname_in)) usebinname=.true. 
   if (verbose>2) then 
     print *,":MGRADS:writectl:optemplate=",bf%optemplate
     if (usebinname) print *,":MGRADS:writectl:binname=",trim(binname_in)
   end if
   p=0
  !{ Writing CTL file

    !{ BINARY FILENAME
    if (.not.usebinname) then !{CASE of the  use preexisting CTL filename

        if (bf%optemplate) then
       
             ! filename need to have "%" . If not,  include it
             if(index(bf%binname,"%")==0) then 
               binname=trim(bf%binname)//"_"//char(37)//"y4"//char(37)//"m2"//char(37)//"d2"//char(37)//"h2"
             else 
               binname=trim(bf%binname) 
             end if
	   
           else  ! Not a template
             binname=trim(bf%binname)
          end if

    else ! Case binary file name has been provided

       binname=binname_in

    end if
!-----------------------------------------------------------------
    if (index(binname,".bin")==0) binname=trim(bf%binname)//".bin"

!------------------------------------------------------------
! Remove path from filename
!  Caso o path inicie com data generica (%) entao mantem o path
!-----------------------------------------------------------
!{
      r=rinstr(binname,"/")
      if (usebinname) then
        rt=0
      else
        rt=index(binname,"%")
      end if

      if ((rt>0).and.(rt<r)) r=rt-1
      

      if (r>0) then
          binname="^"//binname(r+1:len_trim(binname))
      else
          binname="^./"//binname(r+1:len_trim(binname))
      end if
!}
!{ Substituicao pela data inicial caso nao seja template
    if ((.not.bf%optemplate).and.(index(binname,"%")>0)) then 
        write(yy,'(i4)')year(bf%time_init)
        write(mm,'(i2.2)')month(bf%time_init)
        write(dd,'(i2.2)')day(bf%time_init)
        write(hh,'(i2.2)')hour(bf%time_init)
        write(fct,'(i3.3)')bf%fct
        binname=replace(binname,"%y4",yy)
        binname=replace(binname,"%Y4",yy)
        binname=replace(binname,"%m2",mm)
        binname=replace(binname,"%d2",dd)
        binname=replace(binname,"%h2",hh)
        binname=replace(binname,"%f3",fct)
    end if
!}

    lines(1)='DSET    '//trim(binname)
    
!{ OPTION TEMPLATE
    if ((bf%optemplate).and.(.not.usebinname)) then
      lines(2)='OPTIONS TEMPLATE               '
    else
      lines(2)='                               '
    end if
!}

   lines(3)='TITLE '//trim(bf%title)
   lines(4)='UNDEF   '//trim(strs(bf%undef))
   lines(5)='XDEF    '//trim(strs(bf%imax))//' LINEAR '//trim(strs(xmin))//' '//trim(strs(stpx))

!{ YDEF pode ser LINEAR or LEVEL  caso stpy == 0, isto indica que o YDEF e LEVELS
      if (stpy==0.0) then
        lines(6)='YDEF    '//trim(strs(bf%jmax))//' LEVELS'
        rz=3
        do z=1,bf%jmax
          rz=rz+1
          if (rz>=10) then
            p=p+1
            lines(6+p)=""
            rz=0
          end if
          Lines(6+p)=trim(lines(6+p))//' '//trim(strS(bf%lat(z)))
        end do
      else
        lines(6+p)='YDEF    '//trim(strs(bf%jmax))//' LINEAR '//trim(strs(ymin))//' '//trim(strs(stpy))
      end if
 !}

    lines(7+p)='ZDEF    '//trim(strs(bf%kmax))//' LEVELS'
    do z=1,bf%kmax
      Lines(7+p)=trim(lines(7+p))//' '//trim(strS(bf%lev(z)))
    end do


!{ Caso nao seja fornecido jdate, entao utiliza o valor em bf%tlable como data inicial
    if (ftime<1950.0) then
        lines(8+p)=bf%tlable
    else
        if (stpt>=1) then
            lines(8+p)='TDEF  '//trim(strs(tmax))//" linear "//grdate(ftime)//" "//trim(strs(stpt))//"hr"
        else
            lines(8+p)='TDEF  '//trim(strs(tmax))//" linear "//grdate(ftime)//" "//trim(strs(stpt*60.0))//"mn"
        end if
    end if
!}

    lines(9+p)='VARS   '//trim(strS(bf%nvars))

!---------------------
! GETTING  CLT FILENAME
!---------------------
    close(8)
    if (usebinname) then ! Caso fornecido nome do binario forca nome do ctl baseado no nome do binario
       
        if (index(binname_in,".bin")>0) then
            ctlname=replace(binname_in,".bin",".ctl")
        else
            print *,":MGRADS:AQUI1",trim(binname_in)
            ctlname=trim(binname_in)//".ctl"
        end if

     else !{ Caso nao fornecido nome do binario, usa ctl pre-estabelecido
          ctlname=bf%ctlname
     end if
!----------------
! WRITE CTL FILE
! ---------------
!{
    if(verbose>2)  print *,":MGRADS: CTL=",trim(ctlname)
     open (8,file=(ctlname),status='replace')

    do i=1,9+p
        write(8,'(a)')trim(lines(i))
    end do

    do v=1,bf%nvars
        write(8,881)bf%code(v),bf%varlevs(v),trim(bf%name(v))
 881    format(a8,i3,' 0 ',a)
    end do
    write(8,'(a)')'ENDVARS                            '
    close(8)
!}

end subroutine


!------------------------------------------------------------------------------
!
!------------------------------------------------------------------------------
subroutine close_mgrads(bf)
!{ Variaveis da interface
   type(bindef),intent(in)::bf
!} 
    !deallocate(bf%lon)
    !deallocate(bf%lat)
    !deallocate(bf%lev)
    !deallocate(bf%varlevs)
    !deallocate(bf%code)
    !deallocate(bf%name)

end subroutine


!==============================================================================!
! varindex   | Retorna a posicao de uma variavel na estrutura             |SHSF!
!==============================================================================!

 function varindex (bf,varcode);integer::varindex
!{ Variaveis de entrada
    type(bindef),intent(in)::bf
    character(len=*),intent(in)::varcode
!}
!{ Variaveis locais
      integer::i
	i=0
	varindex=0
	do i=1,bf%nvars
		if (trim(ucases(bf%code(i)))==trim(ucases(varcode))) varindex=i
	end do
end function


!==============================================================================!
! datetime   | Retornar a data relativa a um instante de tempo t         |SHSF!
!==============================================================================!
 function datetime_mgrads(bf,t);real*8::datetime_mgrads
 !{ Cariavel da interface
	  type(bindef),intent(in)::bf ! Definicao do arquivo 
	  integer,intent(in)::t      ! Passo de tempo desejado	  
	  datetime_mgrads=bf%time_init+(bf%time_step*real(t-1))
	  
 end function

!==============================================================================!
! overflow_err   | Retornar true ou false para teste de transbordamento     |SHSF!
!==============================================================================!

function overflow_err(v,bl); logical::overflow_err
  !{ Variaveis da interface
  real,dimension(:,:,:),intent(inout)::v
  type(bindef),intent(in)::bl
  !}
  !{Variaveis locais
    real::scalemax
    real::scalemin
    integer::i,j,k
  !}
	scalemax=10.0**(int(log(abs(undefval))/log(10.0))-1)
	scalemin=-scalemax
	overflow_err=.false.
	do i=1,bl%imax
	do j=1,bl%jmax
	do k=1,bl%kmax
	if (v(i,j,k)/=bl%undef) then
	if ((v(i,j,k)>scalemax).or.(v(i,j,K)<scalemin))  then 
		overflow_err=.true.
                if (verbose > 1) then 
		  print *,":MGRADS:Overflow error at v(",i,j,k,")=",v(i,j,k)
		  print *,"        scalemax,bl%undef=",scalemax,bl%undef
                end if
		return 
		!v(i,j,k)=bl%undef
	end if	
	end if
	end do
	end do
	end do

end function

!---------------------------------------------------------------------
! conv_undef | Converte valores indefinidos
!-------------------------------------------------------------------
subroutine conv_undef_mgrads(bi,bf,v)
!{ variaveis de interface
	type(bindef),intent(in)::bi
	type(bindef),intent(in)::bf
	real,dimension(:,:,:),intent(inout)::v
!}
!{ Variaveis locais
	integer::i,j,k,kmax
!}
     kmax=size(v,3)
     if (bi%kmax<kmax) kmax=bi%kmax

	if (bi%undef/=bf%undef) then 
	do i=1,bi%imax
	do j=1,bi%jmax
	do k=1,kmax
	if (v(i,j,k)==bi%undef) v(i,j,k)=bf%undef
	end do
	end do
	end do
	end if
!}


end subroutine 


!--------------------------------------------------------------
! 
!
!==============================================================================!
! openw_mgrads   | Abre arquivo para gravacao no formato do grads         |SHSF!
!==============================================================================!
! Exemplo de uso
 subroutine newgrid_mgrads(bf,imax,jmax,kmax,nvarmax)
 !{ Variaveis da interface
   type(bindef),intent(inout)::bf 
   integer,intent(in)::imax,jmax,kmax ! Dimensionamento da matriz em x,y e z
   integer,intent(in)::nvarmax ! Numero maximo de variaveis
!}
!{ Variaveis locais
   logical::exists
   character(len=1024)::binname
!}
   bf%nvars=0
   bf%imax=imax
   bf%jmax=jmax
   bf%kmax=kmax
   bf%undef=undefval
   bf%recsize=bf%imax*bf%jmax
   bf%nvarsmax=nvarmax
   bf%nregtime=0
   bf%nreg=0
   bf%lof=0
   allocate(bf%lon(bf%Imax))
   allocate(bf%lat(bf%jmax))
   allocate(bf%lev(bf%kmax))
   allocate(bf%varlevs(nvarmax))
   allocate(bf%code(nvarmax))
   allocate(bf%name(nvarmax))
  
  end subroutine
  
  
!==============================================================================!
! chk_ctlname   | check and fix the CTL FILENAME                          |SHSF!
!==============================================================================!
 subroutine chk_ctlname(bf)
    type(bindef),intent(inout)::bf
    
   character(len=1024)::ctlname,ctl1,ctl2,binname
   integer            ::IO,ii
   character(len=4)::yy
   character(len=2)::mm,dd,hh
   character(len=3)::fct
     
   if (index(bf%ctlname,"%")>0) then 
     !{O path com a data e removido
       ctl1=getpath(bf%ctlname)
  
       ii=index(ctl1,"%")
        if (ii==1) then 
           ctl1=""
        elseif(ii>1) then
           ctl1=ctl1(1:ii-1)
        end if
        bf%ctlname=trim(ctl1)//"/"//trim(getfname(bf%ctlname))
     !}
     
  
     !{ O nome com a data, tem a data substituida
     !  pela data inicial
        !write(yy,'(i4)')year(bf%time_init)
        !write(mm,'(i2.2)')month(bf%time_init)
        !write(dd,'(i2.2)')day(bf%time_init)
        !write(hh,'(i2.2)')hour(bf%time_init)
        write(fct,'(i3.3)')bf%fct
	
        bf%ctlname=replace(bf%ctlname,"%y4",'')
        bf%ctlname=replace(bf%ctlname,"%m2",'')
        bf%ctlname=replace(bf%ctlname,"%d2",'')
        bf%ctlname=replace(bf%ctlname,"%h2",'')
        bf%ctlname=replace(bf%ctlname,"%Y4",'')
        bf%ctlname=replace(bf%ctlname,"%M2",'')
        bf%ctlname=replace(bf%ctlname,"%D2",'')
        bf%ctlname=replace(bf%ctlname,"%H2",'')

	    bf%ctlname=replace(bf%ctlname,"%f3",fct)
     !}
   end if
   end subroutine 
   
   !
   !
   !
    function vertical_level_index(bi,pres); integer::vertical_level_index
        type(bindef),intent(in)   ::bi
        real,        intent(in)   ::pres 
        integer:: i,l
        l=0
        do i=1,bi%kmax
            if (bi%lev(i)==pres) then 
                l=i
                exit
            end if
        end do
        vertical_level_index=i
    end function

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
	integer,optional, intent(in)::ifct       ! Forecast time indicator (optional) (hours)

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
END MODULE
