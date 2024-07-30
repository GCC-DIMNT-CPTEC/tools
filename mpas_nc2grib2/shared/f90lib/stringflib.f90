
!******************************************************************************
!*                                STRINGFLIB                                  *
!*                                                                            *
!*     Biblioteca de funcoes e rotinas para manipulacao de variaveis-texto    *
!*                                                                            *
!*              Copyright (C) 2005, Sergio Henrique Soares Ferreira           *
!*                                                                            *
!*                   MCT-INPE-CPTEC, Cachoeira Paulista, Brasil               * 
!* SHSF: Sergio Henrique Soares Ferreira                                      *
!* ALTF: Ana Lucia Travezani Ferreira                                         *
!* SMJ : Saulo Magnum de Jesus                                                * 
!******************************************************************************
!*                                                                            *
!*  Esta biblioteca fornece um conjunto de subrotinas e funcoes uteis para    *
!*  manipulacao de variaveis-texto (Character) em FORTRAN                     *
!*                                                                            *
!******************************************************************************
!* DEPENDENCIAS: GetArgs (Sistema Operacional)                                *
!******************************************************************************
! 
! HISTORICO
!
!2005-03-10 - SHSF - Corrigido "bug" na funcao str_real que ocorre quando passado 
!                    o valor 0.0000E+00
!
!2007-01-20 - SHSF - Acrescido Subrotina GETARGS2 
!2007-03-22 - SHSF - Modificacao da rotina VAL- Antiga VAL agora esta como VAL2 
!2009-10-02 - SHSF - Acrescido funcao replace 
!2009-10-24 - SHSF - Corrigindo str_realS : Erro no truncamento de zeros a esquerda 
!                    quanto utiliza notacao cientifica
!2010-05-21 -SHSF/ALTF - Corrigido leitura de parametros em getarg2
!2010-09-07 -SHSF  - Corrigido funcao VAL para aceitar os codigos ascII<32, incluindo o TAB 
!2010-09-26 -SHSF  - Criado funcao isVal para verificar se uma linha tem apenas numeros ou nao
!2010-10-11 -SHSF e SMJ - Corrigido funcao split com relacao a separacao do ultimo caracter
!2014-11-26 -SHSF  - Subrotina getargs2 foi substituida por get_parameter, baseado no comando get_command
!2024-02-28 -SHSF  -Add str_clean function

 module stringflib
        
   implicit none
   !USE MSFLIB  ! Para compilacao em Windows ( Microsoft Power Station )

  !-----------------------------------------------------------------------------
  ! Subroutinas deste modulo
  !----------------------------------------------------------------------------
  ! get_parameter | Obtem lista de argumentos passados por paramentro
  ! getpath
  ! IsVal    |Verifica se um texto contem apenas valores numericos
  ! lcases   |Converte texto para letras menusculas  (lowercase)
  ! replace 
  ! Sep_NatNum
  ! sep_words
  ! split    |Separacao de "Strings" em "Sub-String"  
  ! strs     |Converte uma valiavel numerica em variavel texto (string) 
  ! ucases   |Converte um texto para letras maiuhsculas (upercase)
  ! val      |Converte texto em numeros
  !
  


  Real,parameter        :: Null=-340282300      !valor nulo ou indefinido
  !integer               :: verbose              !Verbosidade 0,1,2,3
  
	
!******************************************************************************
!  STRS| Converte uma valiavel numerica em variavel texto (string)     | SHSF |
!******************************************************************************
!                                                                             |
! Esta e uma interface para converter variaveis numericas (INTERGER ou REAL)  |
! em texto                                                                    |
!******************************************************************************
!{
	private str_intS	! Converte INTEGER em CHARACTER
	private str_realS	! Converte REAL em CHARACTER 
    private str_str         ! Converte character em character sem codigos especias

	interface STRS
		module procedure str_intS
		module procedure str_realS
        module procedure str_str
        module procedure str_clean   !(a,b) Return string without character "b" occorrences
	end interface

       interface getarg2
		module procedure get_parameter
       end interface
	

 !}
 !===========================================================================
	contains

!******************************************************************************
!  Sep_NatNum | Separacao de Numeros Naturais                          | SHSF |
!******************************************************************************
!                                                                             !
!   Esta subrotina separa numeros naturais contidos em um texto uma variavel  !
!                                                                             |
!******************************************************************************

Subroutine Sep_NatNum(string, substrings, nelements)

!{ Variaveis da Interface
     character(len=*), intent (in) :: string !.................. Texto de entrada contendo Letras e Numeros          
	 character(len=*), dimension(:), intent (out)::substrings  !.Matriz contendo apenas os numeros que foram separados
	 integer , intent (out) :: nelements !.....................  Numero de elementos em "substrings" 

!}
!{ Variaveis Locais
	 integer :: i,l,maxl,F 
	 character(len=1) ::DS
	 character(len=256) :: SS
!}

	F=0
	SS=""
	substrings=""
	maxl=size(substrings,1)	  
	l = Len_trim(string)
	
	if (l==0) goto 100
		do i = 1,l
		
		dS = string(i:i)
		If (index("0123456789", dS)== 0) Then
			If (Len_trim(sS) > 0) Then
				F = F + 1
				substrings(F) = trim(sS)
				sS = ""
			End If
		Else
			sS = trim(sS) // dS
		End If
		if (F==maxl+1) exit 
		end do !i

		If (sS /="") then ;F = F + 1; substrings(F) = sS; sS = "";end if

100	  nelements=F


End Subroutine Sep_NatNum


	
!******************************************************************************
!  Sep_NatNum | Separacao de Numeros Naturais                          | SHSF |
!******************************************************************************
! 																			  !
!   Esta subrotina separa numeros naturais contidos em um texto uma variavel  !
!																			  |
!******************************************************************************

Subroutine Sep_Num(string, substrings, nelements)
    
!{ Variaveis da Interface
     character(len=*), intent (in) :: string !.................. Texto de entrada contendo Letras e Numeros          
	 character(len=*), dimension(:), intent (out)::substrings  !.Matriz contendo apenas os numeros que foram separados
	 integer , intent (out) :: nelements !.....................  Numero de elementos em "substrings" 

!}
!{ Variaveis Locais
	 integer :: i,l,maxl,F 
	 character(len=1) ::DS
	 character(len=256) :: SS
!}

	F=0
	SS=""
	substrings=""
	maxl=size(substrings,1)	  
	l = Len_trim(string)
	
	if (l==0) goto 100
	    
		do i = 1,l
        
		  dS = string(i:i)
		  If (index("-0123456789.+", dS)== 0) Then
          
		     If (Len_trim(sS) > 0) Then
                            
				 F = F + 1
                 substrings(F) = trim(sS)
                 sS = ""

			  End If
           
		   Else
     	    
			  sS = trim(sS) // dS
           
		   End If
       	 
		 if (F==maxl+1) exit 

	   end do !i
      
	  If (sS /="") then ;F = F + 1; substrings(F) = sS; sS = "";end if
100	  nelements=F


End Subroutine Sep_Num

 
!******************************************************************************
!  split| Separacao de "Strings" em "Sub-String"                       | SHSF |
!******************************************************************************
!                                                                             |
!  Decompoe um string em um conjunto e sub-strings segundo um caracter de     |
!  separacao.                                                                 |
!                                                                             |
!******************************************************************************
 subroutine split(string,sep,substrings,nelements)
!{ Variaveis da Interface
	character (len=*),               intent(in)::string     !Texto  de entrada
	character(len=*),                intent(in)::sep        !Caracteres de seperacao
	character (len=*),dimension(:), intent(out)::substrings !Palavras separadas do texto
	integer,                        intent(out)::nelements  !Numero de palavras
 !}

 !{ Variaveis Locais 
         integer :: i,l,maxl,F, j
         character(len=1) ::D
         character(len=256) :: S
         character(len=256) ::auxline
 !}
         F=0
         S=""
	 substrings=""
         maxl=size(substrings,1)
         if (maxl<=2) goto 100
         l=len_trim(string)
         if (l==0) goto 100
         
         !{ Procura por texto entre aspas
                
         i=1
         do while ( i<=l)
         
            D = string(i:i)
            j=0
            if (D=='"') then 
              auxline=string(i+1:len_trim(string))
              j=index(auxline,'"')
            end if

            if (j>0) then
              F=F+1
              auxline=string(i+1:i+j-1)
              substrings(F)=auxline
              i=i+j
            else    
                If (index(sep,D)>0) Then
                        If (Len(trim(S)) > 0) Then
                                F = F + 1 
				substrings(F) = S
				S = ""
			End If
                 Else
                    S = TRIM(S) // D
                 End If

              end if
                 if (F==maxl+1) exit 
           i=i+1
          end do !i

          If (S /= "") then 
            F = F + 1
            substrings(F) = S
          end if
 
 100  nelements=F
end subroutine
!******************************************************************************
!  split2| Separacao de "Strings" em "Sub-String"                       | SHSF |
!******************************************************************************
!                                                                             |
!  Decompoe um string em um conjunto e sub-strings segundo um caracter de     |
!  separacao.                                                                 |
! Versao 2                                                                             |
!******************************************************************************
 subroutine split2(string,sep,substrings,nelements)
!{ Variaveis da Interface
	character (len=*),               intent(in)::string     !Texto  de entrada
	character(len=*),                intent(in)::sep        !Caracteres de seperacao
	character (len=*),dimension(:), intent(out)::substrings !Palavras separadas do texto
	integer,                        intent(out)::nelements  !Numero de palavras
 !}

 !{ Variaveis Locais 
         integer :: i,l,maxl,F, j
         character(len=1) ::D
         character(len=256) :: S
         character(len=256) ::auxline
         integer            ::ns
 !}
         F=0
         S=""
         ns=0
	 substrings=""
         maxl=size(substrings,1)
         if (maxl<=2) goto 100
         l=len_trim(string)
         if (l==0) goto 100
         
         !{ Procura por texto entre aspas
                
         i=1
         do while ( i<=l)
         
            D = string(i:i)
            j=0
            if (D=='"') then 
              auxline=string(i+1:len_trim(string))
              j=index(auxline,'"')
            end if

            if (j>0) then
              F=F+1
              auxline=string(i+1:i+j-1)
              substrings(F)=auxline
              i=i+j
            else    
                If (index(sep,D)>0) Then
                        If (ns > 0) Then
                                F = F + 1 
				substrings(F) = S
				S = ""
				ns=0
			End If
                 Else
                    S = TRIM(S) // D
                    ns=ns+1
                 End If

              end if
                 if (F==maxl+1) exit 
           i=i+1
          end do !i

          if (ns>0) then 
           
            F = F + 1
            substrings(F) = S
          end if
 
 100  nelements=F
end subroutine

!******************************************************************************
! sep_words	| Separa as palavras que compoe uma linha de texto     | SHSF |
!******************************************************************************
!                                                                             |
!  Separa as pavras de uma linha de texto em uma matriz de palavras,          |
!                                                                             |
!******************************************************************************

 subroutine sep_words(string,words,nwords)

 !{ Variaveis da Interface
	 character (len=*), intent (in) :: string !..................Texto  de entrada
	 character (len=*), dimension(:), intent (out)::words !..... Palavras separadas do texto
	 integer , intent (out) :: nwords !..........................Numero de palavras
 !}

 !{ Variaveis Locais 
	 integer :: i,l,maxl,F 
	 character(len=1) ::D
	 character(len=256) :: S
 !}
	 F=0
	 S=""
	 words=""
	 maxl=size(words,1)
	 if (maxl<=2) goto 100
	 l=len_trim(string)
	 if (l==0) goto 100
	 
	 do i=1,l
	 
	    D = string(i:i)
		If (index(" ,;",D)>0) Then
			If (Len(trim(S)) > 0) Then
				F = F + 1 
				words(F) = S
				S = ""
			End If
		 Else
		    S = TRIM(S) // D
		
         End If
		 if (F==maxl+1) exit 
      
	  end do !i
      	
	  If (S /= "")  F = F + 1; words(F) = S; S = ""
 
 100  nwords=F
 	 
	End Subroutine

!******************************************************************************
! str_ints  | Converte uma variavel inteira em uma variavel-texto      | SHSF |
!******************************************************************************
!  Funcao tipo Character que converte o valor de uma variavel inteira nos     | 
!  caracteres correspondentes                                                 |
!******************************************************************************
function str_intS(a); character(len=256) ::str_intS

!{ Variavel de Interface	
	 integer , intent (in) :: a
!}

!{ Variavel Local

	character(len=256)::b
!}
	 b=""
	 write(b,*)a
	 str_intS=adjustl(b)

	end function


!******************************************************************************
! str_reals	| Converte uma variavel REAL em uma variavel-texto     | SHSF |
!******************************************************************************
!  Funcao tipo Character que converte o valor de uma variavel REAL nos        | 
!  caracteres correspondentes                                                 |
!******************************************************************************

 function str_realS(a);character(len=256)   ::str_realS,b
 
 !{ Variaveis da Interface
  real , intent (in) :: a
 !}

 !{ Variaveis Locais
	 integer ::p,r
 !}

	 b=""
	 write(b,*)a
	 b=adjustl(b)

	 p=index(b,".")

!{ Eliminando E+00 e E-00

	r=INDEX(B,"E+00")
	IF (R>0) B=B(1:R-1)

	R=INDEX(B,"E-00")
	IF (R>0) B=B(1:R-1)
!}
  
     
!{Eliminando zeros a esquerda se nao for notacao cientifica 
	if (index(B,"E") ==0) then 
11	 r=len_trim(b)

	 if ( (b(r:r)=="0").and.(r>p).and.(p/=0))then 
		 r=r-1
		 b=b(1:r)
		 goto 11
	 end if
	end if 
!} 	 

	 if ((r==p).and.(p>0)) then 
	    r=r-1
		b=b(1:r)
	end if

	str_realS=adjustl(b)
	 
	end function
		

!******************************************************************************
! VAL	| Converte CHARACTER em REAL                                   | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
 function VAL2(AS);real VAL2

 !{ Variaveis da interface
	 character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
	 real            ::b
	 character(len=1):: cS
	 character(len=255)::numS,aaS
	 integer i,l,chknum
 !}

	 chknum=1
	 numS=""
	 aaS=adjustl(aS)
	 l=len_trim(aaS)
	 
	 if (trim(aas)=="-") chknum=0
	 
	 
	 if (l>0) then 
	   
		do i=1,l
	 
                        cS=aaS(i:i)
                        if (ichar(CS)<32) CS=" "
         
                        if (index(" 0123456789.+-E",cS)>0) then
                                numS=trim(numS)//cS
                        else
                                chknum=0         
			end if
		end do


		if (chknum==1) then 
			read(numS,*)b
		else 
			b=NULL
		end if

     else 
		b=NULL
	 end if

         val2=b
        end function    
!******************************************************************************
! IsVAL | Verify if a text has only numerical characters               | SHSF |
!******************************************************************************
!  Funcao que ferifica se um texto possui apenas caracteres numericos  
!  If .true.  there are only numerical characters 
!                                                                             |
!******************************************************************************
 function IsVAL(AS);logical  IsVAL

 !{ Variaveis da interface
   character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
    character(len=1):: cS
    character(len=255)::numS,aaS
    integer i,l
 !}
  IsVal=.false.
  aaS=adjustl(trim(aS))
  l=len_trim(aaS)
  
  if (l>0) then 
    do i=1,l
      cS=aaS(i:i)
      if (ichar(CS)<32) CS=" "
      if (index("0123456789.+- ",cS)>0) then
        isVal=.true.
      else
        IsVal=.false. 
        exit
      end if
    end do
  end if

end function
  

!******************************************************************************
! VAL   | Converte CHARACTER em REAL                                   | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
!HISTORICO
!  20110904 : Acrescentado eliminacao do caracter @ quando junto ao numero ou
!            de eliminacao de caracteres abaixo de ASC 32
 function VAL(AS);real VAL

 !{ Variaveis da interface
   character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
    real            ::b
    character(len=1):: cS
    character(len=255)::numS,aaS
    integer i,l,chknum
    integer::n1,n2
    integer::IE
 !}
  chknum=0
  numS=""
  aaS=adjustl(trim(aS))
  l=len_trim(aaS)

  if (l>0) then 
    do i=1,l
      cS=aaS(i:i)
      if ((ichar(CS)>=32).and.(CS /="@")) then
      if (index("0123456789.+-E",cS)>0) then
        numS=trim(numS)//cS
        chknum=1
      else
        exit
      end if
      end if
    end do
   IE=index(numS,"E")
   if (index(numS,"-")>(IE+1)) chknum=0
   if (trim(aas)=="-") chknum=0
    if ((chknum==1).and.(len_trim(numS)>0)) then 
      read(numS,*)b
    else 
      b=NULL
    end if
  else 
    b=NULL
  end if
  val=b
end function            
!******************************************************************************
! IVAL  | Converte CHARACTER em Inteiro                                | SHSF |
!******************************************************************************
!  Funcao para converter texto com caracteres numericos em uma variavel       |
!  REAL.                                                                      |
!                                                                             |
!  Caso a texto contiver caracteres invalidos VAL retornara o valor "NULL"    |
!  (Veja declaracao da variavel NULL)                                         |
!                                                                             |
!******************************************************************************
 function IVAL(AS);integer:: IVAL

 !{ Variaveis da interface
	 character(LEN=*),intent (in) :: aS 
 !}
 !{ Variaveis locais
	 real            ::b
	 character(len=1):: cS
	 character(len=255)::numS,aaS
	 integer i,l,chknum
 !}
	 chknum=0
	 numS=""
	 aaS=adjustl(trim(aS))
	 l=len_trim(aaS)


	 if (l>0) then 
	   
		do i=1,l
	 
			cS=aaS(i:i)
			if (ichar(CS)<32) CS=" "
	 
			if (index("0123456789.+-",cS)>0) then
				chknum=1
				numS=trim(numS)//cS
			else
				exit
			end if
		end do
	
		if ((chknum==1).and.(len_trim(numS)>0)) then 
			read(numS,*)b
		else 
			b=NULL
		end if
			

     else 
		b=NULL
	 end if
	 ival=b
	end function
!******************************************************************************
! UCASES| Todos as letras MAIUSCULAS                                   | SHSF |
!******************************************************************************
!  Funcao que retorna um texto em "CAIXA ALTA" de um texto                    |
!                                                                             |
!  Exemplos:                                                                  |
!                  char(97)="a" --> char(65)="A"                              |
!                  char(122)="z" --> char(90)="Z"                             |
!******************************************************************************     
function UCASES(str);character(len=255)::UCASES

	!{ Variaveis da interface	 
		 character(len=*)::str
	!}
	!{ Variaveis locais
		 character(len=255)::b
		integer :: i,l,a
	!}

	 
	 b=str
	 l=len(trim(str))
	 if (l>0) then 
	 do i=1,l
	 
	     a=iachar(str(i:i))
	     
		 if ((a>=97).and.(a<=122)) then 
		    a=a-32
	   	    b(i:i)=achar(a)
		 end if
	 
	 end do
	 end if
	 ucaseS=b
	 
	 end function   
	 
!******************************************************************************
! LCASE| Todas as letras MENUSCULAS                                    | SHSF |
!******************************************************************************
!  Funcao que retorna um texto em "CAIXA BAIXA" de um texto                   |
!                                                                             |
!  Exemplos:                                                                  |
!                char(65)="A" --> char(97)="a"                                |
!                char(90)="Z" --> char(122)="z"                               |
!*****************************************************************************|
 
function LCASES(str); character(len=255)::LCASES

	 !{ Variaveis de Interface
	 character(len=*)::str
	 !}

	 !{ Variaveis internas
	 character(len=255)::b
	 integer :: i,l,a
	 !}


	 b=str
	 l=len(trim(str))
	 
	 do i=1,l
	 
	   a=iachar(str(i:i))

	   if ((a>=65).and.(a<=90)) then 
	      
		  a=a+32
	   	  b(i:i)=achar(a)
	   
	   end if

	 end do
	 
	 lcaseS=b
	 
	 end function  lcaseS


!******************************************************************************
! RIGHTS| Obtem os caracteres a direita de um texto                   | SHSF |
!******************************************************************************
	function rightS(char,lenth); character  :: rightS

	!{ Variaveis da interface
		character (len=*),intent (in) :: char !....................Texto original 
         integer,intent (in):: lenth !..........comprimento do texto a ser obtido 
	!}
	
	 !{ Variaveis locais	
	 character (len=len(trim(char))):: a
	 integer 			:: l,i
	 !}

	 a=trim(char)
	 l=len(a)
	 i=l-lenth+1
	 rightS=a(i:l)	
	end function rightS 


!******************************************************************************
! LEFTS| Obtem os caracteres a esquerda de um texto                   | SHSF |
!******************************************************************************

	function leftS(char,lenth);	 character  :: leftS

	!{ Variaveis de interface
	 character (len=*),intent (in) :: char	 !....................Texto original
	 integer, intent (in) :: lenth	!..........comprimento do texto a ser obtido

	!}
		 
	 leftS=char(1:lenth)
	end function leftS	  	  


!******************************************************************************
! CUTSTRING| Corta um texto                                            | SHSF |
!******************************************************************************
!  Corta uma linha de texto a partir da primeira ocorrecia de um caracter
!  especificado                                                               |
!*****************************************************************************|


subroutine CutString(line,char)
	!{ Variaveis de interface
		character(len=255),intent(inout)::line ! Linha de texto 
		character(len=1),intent(in)::char 	   ! Caracter para corte
	!}
	!{Variaveis locais
		integer :: cp
	!}

		cp=index(line,char)-1

		if (cp>0) then 

			line=line(1:cp)

		elseif(cp==0) then 
			line=""
		end if

		
	end subroutine 

!******************************************************************************
!BETWEEN_INVCOMMAS|                                                    | SHSF |
!******************************************************************************
!* Obtem a primeira ocorrencia de um texto delimitado por aspas duplas        *
!*                                                                            *
!******************************************************************************
function  between_invdcommas (line); character(len=255)::between_invdcommas

!{ Variaveis de interface
	character(len=*),intent(in)::line
!}

!{ Variaveis locais	 
	integer::i
	character(len=255)::auxline
 !}
	auxline=line
	between_invdcommas=""

	i=index(auxline,'"')

	if (i==0) return
	 
	auxline=auxline(i+1:len_trim(auxline))
	
	i=index(auxline,'"')
	
	if (i==0) return 
		
	between_invdcommas='"'//auxline(1:i+1)
end function

!------------------------------------------------------------------------------
!GET_PARAMETER ! OBTEM ARGUMENTOS PASSADOS EM LINHA DE COMANDOS        !SHSF !
! -----------------------------------------------------------------------------
!  ESTA SUBROTINA E BASEADA NO COMANDO GET_COMMAND                            !
!                                                                             !
!  Ao inves de obter os argumentos tal como sao digitados,  esta              !
!  sub-rotina interpreta as letras que precedidadas por "-" como indicativo   !
!  do tipo de argumento que esta sendo passado                                !
!                                                                             !
!     ex.:   programa -d 20APR2007                                            !
!                                                                             !
!     Neste exemplo "d" e o nome do argumento  e "20apt2007" eÃ¯Â¿Â½ o valor do  !
!     argumento                                                               !
!                                                                             !
!   Programa Exemplo                                                          !
!     integer,paramenter::x=<Numero de agumentos esperados>                   !
!     character(len=1),dimension(100)::namearg !.......... Nome dos argumentos!
!     character(len=256),dimension(100)::arg  !.................... argumentos!
!     integer::nargs      !........ numero de argumentos efetivamente passados!
!     integer::i          !................................. Variavel auxiliar!
!                                                                             !
!     call get_parameter(namearg,arg,nargs)                                         !
!                                                                             !
!     do i=1, nargs                                                           !
!        print *,namearg(i)," = ",trim(arg(i))                                !
!     end do                                                                  !
!  Nota: a Dimensao de namearg e arg estabelece o numero maximo de argumentos !
!        que podem ser passados. Caso sejam passados mais argumentos do que   !
!        esta dimensao, os ultimos argumentos serao ignorados                 !
!-----------------------------------------------------------------------------!
!  DEPENDEDIAS: NAO HA                                 !
!-----------------------------------------------------------------------------!

  subroutine get_parameter(argnames,args,nargs)
!{ Variaveis da interface	
   character(len=*),dimension(:),intent(out)::argnames
   character(len=*),dimension(:),intent(out)::args
   integer,intent(out)::nargs
!}
!{ Variaveis locais
 integer*2::argc
 character(len=1024)::indate
 integer::i,j ,k
!}

   i=ubound(argnames,1)
   argc =  command_argument_count()
   if (argc>i) argc=i
   argnames(1:argc)=" "
   args(1:argc)="                    "
   i=0
   j=0
   k=0
    DO while (i<=argc)
      j=j+1
      10 i=i+1
      call get_command_argument(i,indate)
      if (len_trim(indate)>0) then
      if ((index(indate,"-")==1).and.(len_trim(indate)==2)) then 
       
        if(k==1) j=j+1
        argnames(j)=indate(2:2)
        k=1
        goto 10
      else
        if (argnames(j)=="") argnames(j)="?"
        args(j)=indate
        k=0
      end if 
      end if 
    end DO
    nargs=j
end subroutine get_parameter


!------------------------------------------------------------------------------
!GETVARS ! OBTEM ATRIBUICAO DE VARIAVEIS                                !SHSF !
!-----------------------------------------------------------------------------!

  subroutine getvars(string, varname,values,nvars,nvarmax)
  !{ Variaveis de interface
	character(len=*),intent(in)::string
	character(len=*),dimension(:),intent(out)::varname
	character(len=*),dimension(:),intent(out)::values
	integer,intent(out)::nvars  
	integer,intent(in)::nvarmax 
   !}
   !{Variaveis locais
     integer::nelements
	 character(len=255),dimension(nvarmax)::substring
	 integer::i
   !}
   
	call sep_words(string,substring,nelements)
	i=0
	nvars=0
	do while (i<nelements)
		i=i+1
		if (trim(substring(i))=="=") then 
			i=i+1
			values(nvars)=substring(i)
		else
			nvars=nvars+1
			varname(nvars)=substring(i)
		end if
	end do

end subroutine	getvars

! |----------------------------------------------------------------------------|
!|getpath | Obtem o caminho de um arquivo                                |SHSF|
!'|-----------------------------------------------------------------------------
!'|
Function getpath(filename);character(len=1024)::getpath
	character(len=*),intent(in)::filename    
	
	character(len=1024)::f
	integer::p,p1,p2

	p1 = rinstr(filename, char(92)) !92="\"
	p2 = rinstr(filename, "/")
	if (p1>p2) then
		p=p1
	else
		p=p2
	end if


    If (p > 0) Then
        f = filename(1:p)
    Else
        f = ""
    End If
    getpath = f
End Function
 ! |----------------------------------------------------------------------------|
!|getfname | Obtem o nome do arquivo sem o PATH                            |SHSF|
!'|-----------------------------------------------------------------------------
!'|
Function getfname(filename);character(len=1024)::getfname
	character(len=*),intent(in)::filename    
	
	integer::r
       
       r=rinstr(filename,"/") 
       

      if (r>0) then
          getfname=filename(r+1:len_trim(filename))
      else
          getfname=filename
      end if
End Function
!------------------------------------------------------------------------------
! rinst
!----------------------------------------------------------------------------
! Retorna a ultima ocorrencia de um substring 
!---------------------------------------------------------------------------
 Function rinstr(A, B);integer::rinstr
  !{ Variaveis de interface 
   character(len=*),intent(in)::A !String
   character(len=*),intent(in)::B !Substring
  !}
  !{ Variaveis localis
    integer::l1
    integer::l2 
    integer::k,i 
  !} 
   l1 = Len_trim(A)
   l2 = Len_trim(b)
   k = 0
   do i = 1,l1 - l2   
     If (A(i:i+l2-1) == B) k = i
   end do
  rinstr = k
End Function

!---------------------------------------------------------------------------------
! replace 
!---------------------------------------------------------------------------------
! Subrititui os caracteres a por b em uma linha texto

function replace(line,a,b);character(len=1024)::replace
  !Variaveis da interface
	character(len=*),intent(in)::line
	character(len=*),intent(in)::a
	character(len=*),intent(in)::b
  !}
	integer::llmax,almax,l
	character(len=1024)::linha
	character(len=1024)::c
	linha=line
	almax=len_trim(a)-1
	llmax=len_trim(linha)
	l=0
	do while (l<(llmax-almax))
	  l=l+1
	  llmax=len_trim(linha)
	  c=linha(l:l+almax)
	
	  if(trim(c)==trim(a)) then 
	      if (l==1) then 
		linha=trim(b)//linha(l+almax+1:llmax)
	      else
		linha=linha(1:l-1)//trim(b)//linha(l+almax+1:llmax)
	      end if
	  end if
	end do

	replace=linha

end function
!---------------------------------------------------------------------------
!prox  | Verifica se dois numeros reais sao muito proximos | SHSF 
!----------------------------------------------------------------------
!  Se rval1 ~ rval2  prox = .true.
!  se rval1 /= rval2 prox = .false. 

function near(rval1,rval2);logical::near
  real,intent(in)::rval1
  real,intent(in)::rval2
  real::dif
  dif=abs(rval1-rval2)

  if (dif<0.0000001) then
      near=.true.
  else
      near=.false.
  end if
end function


!------------------------------------------------------------------------------
!rrnamelist  | Remove Remarks from namelist vector                      | SHSF 
!------------------------------------------------------------------------------
! Remove comentarios  do vetor de lista de nomes(namelist)
! Nota: Este subrotina foi escrita para resolver problemas de compatibilidade
! com a leitura de namelist que contenham comentarios, ao lado do namelist
! No caso do programa compilado com GFORTRAN, quando a comentarios "!" no lado 
! direito, a leitura de um vetor ï¿½ interrompida, o que nao ocorre quando o 
! programa e compilado com outros compiladores
! Para lidar com esta incompatibilidade, pode se usar o comentario entre aspas (" ")
! Assim o comentario entra como parte do vetor. 
! Esta rotina elimina os comentarios lidos desta forma verificando se o elemento da
! lista inicial com o sinal "!" indicando que ï¿½ um comentario.
! ---------------------------------------------------------------------------------
subroutine rrnamelist(nlist,ne)
!{ Interface
   character(len=*),dimension(:),intent(inout)::nlist !namelist elements 
   integer,intent(in)                         ::ne    !Number of elemens in namelist (without remarks
!}

!{ local 
   integer::i,j
   character(len=256),dimension(ne)::auxnlist
!}

  i=0
  j=0
  do while (i<ne)
    j=j+1
    if (index(nlist(j),"!")/=1) then 
      i=i+1
      auxnlist(i)=nlist(j)
    end if
  end do  
    
  do i=1,ne
    nlist(i)=auxnlist(i)
  end do

end subroutine

!------------------------------------------------------------------------------
! read_config | Ler arquivo de listas de parametros de configuracao. | SHSF
!-----------------------------------------------------------------------------
! Esta subrotina trata de uma forma alternativa ao uso dos namelist do fortran
! para passagem de parametros de configuracao do programa.
! Como existe diferentes versos de fortran com estrutura diferentes de namelist
! o uso de namelist tradicional pode gerar problemas de compatibilidade quando
! se migra de uma versao para outra do compilador fortran. Assim uma forma
! independente de se ler parametros de configuracao e implementada nesta rotina
!
! Modelo de arquivo de
! :list_name:
!    valor 1 ! Comentarios
!    valor 2 ! comentarios
!   ...
!   valor n  ! Comentarios
! ::
!-----------------------------------------------------------------------------
! Programa exemplo
!program example
! use stringflib
! character(len=255),dimension(1:100)::values
! integer ::nelements,i
!
!   call read_config(1,"extractor.cfg","list_name",values,nelements)
!   do i=1,nelements
!    print *,trim(values(i))
!   end do
!end program
!------------------------------------------------------------------------------
! Historico
  
subroutine read_config(un,filename,list_name,values,nelements)
  !{ Interface
     integer,                      intent(in) ::un         ! Unidade de leitura
     character(len=*),             intent(in) ::filename   !Nome do arquivo de parametros
     character(len=*),             intent(in) ::list_name  !Nome da lista de parametros
     character(len=*),dimension(:),intent(out)::values     !Lista de valores lidos (numericos ou caracteres)
     integer,                      intent(out):: nelements ! Numero de elementos lidos na lista 
  !}
  !{ local
  character(len=255)::linha,listname
   integer::i,j,ncc
   character(len=255),dimension(2000)::cc
   integer::maxe
  !}

    listname=":"//trim(ucases(list_name))//":"
    nelements=0
    maxe=size(values,1)
    open (un,file=filename,status='unknown')
      200 read(un,'(a)',end=999)linha
          call cutstring(linha,"!")
          linha=ucases(linha)
          if (index(trim(linha),trim(listname))==0) goto 200
          i=0
         do while (i==0)

            read(un,'(a)',end=999) linha
            call cutstring(linha,"!")
            i=index(linha,"::") 
            if (i==0) then
              call split(linha,', []',cc,ncc)
              do j=1,ncc
                nelements=nelements+1
                if (nelements>maxe) then 
                  print *,":STRINGFLIB:READ_CONFIG:Error: Number of readed element exceded the maximum values=",maxe
                  stop
                end if
                values(nelements)=cc(j)
              end do
            else 
            end if
          end do
      999 close(un)
          if (nelements==0) then 
            print *,":STRINGFLIB:READ_CONF: ERROR ",trim(listname)," not found in ",trim(filename)
            stop
          end if 
end subroutine read_config

function ilen( A) ; integer::ilen
  character(len=*),intent(in)::A
  integer ::l,i,k
  l=len_trim(a)
  k=0
  do i=1,l
   if (ichar(a(i:i))>13) k=k+1  
  end do
  ilen=k
end function

function str_str(A);character(len=256)::str_str
  character(len=*),intent(in)::A
   integer ::l,i
   
   l=len_trim(a)
   str_str=""
   do i=1,l
     if (ichar(a(i:I))<14) then 
       str_str(I:I)=" "
     else
       str_str(I:I)=a(I:i)
     end if
   end do
end function

function str_clean(a,b);character(len=256)::str_clean
  character(len=*),intent(in)::A
  character(len=1),intent(in)::B
   integer ::l,i

   l=len_trim(a)
   str_clean=""
   do i=1,l
     if ((a(i:I)/=b).and.(ichar(a(i:i))>32)) then
       str_clean=trim(str_clean)//a(i:i)
     end if
   end do

end function

end module stringflib
