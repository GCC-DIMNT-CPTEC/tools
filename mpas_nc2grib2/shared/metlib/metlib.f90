!******************************************************************************
!*                                   METLIB                                   *
!*                                                                            *
!*              FUNCOES MATEMATICAS USUAIS EM METEOROLOGIA                    *
!*                                                                            *
!*              Copyright (C) 2007 Sergio Henrique Soares Ferreira            *
!*                                                                            *
!****************************************************************************** 
!* Este modulo contem uma colecao de funcoes e sub-rotinas que processam      *
!* calculos comuns em meteorologias, tais como conversao de direcao e velo-   *
!* cidade de vento em suas componente zonais e meridionais e calculo de       *
!* variaveis termodinamicas                                                   *
!*                                                                            *
!* Estas funcoes e sub-rotinas levam em consideracao os intervalos em que as  *
!* calculos tem representatividade fisica, retornando valores nulos  quando   *
!* um valor espurio e fornecido                                               *
!******************************************************************************
! HISTORICO 

MODULE METLIB 

      
  
	real, parameter :: pi=3.141596 
	real, parameter :: Null=-340282300
	private Null
	private ITPL
	public


	interface Tvirt
	 module procedure Tvirt_Ceusius
	 module procedure Tvirtw_Kelvin
	end interface

      
       
        


	 CONTAINS

!-----------------------------------------------------------------------------------------
!  Thickness 
!       Retorna a espessura em metros entre dois niveis isobariocos (Press1 e Press2) com 
!       uma temperatura virtural media da camada (TV) 
!
!---------------------------------------------------------------------------------------

 Function thickness(press1, press2, tv_Kelvin);real:: thickness
	
	!{ Variaveis de entrada
		real,intent(in)::press1,press2  ! Pressao da base e topo da camada em (hPa)
		real,intent(in)::tv_kelvin	!Temperatura virtual media da camada em Craus kelvin)
	!}	
	
	!{ Variaveis locais	
		
		real :: p0,p
	!}
	
	p0=press1
	p=press2
  	If (p0 == 0)  p0 = 0.00001
  	If (p == 0 )  p = 0.00001
  	dz = tv_kelvin * 29.3 * Log(p0 / p)
  	thickness = dz
 End Function

!-----------------------------------------------------------------------------------------
!  c2omega 
!       Converte verlocidade vertical w (m/s) para omega (pa/2)
!       Entrada w (m/s), p (hPa),tv (k)
!       saida: omega (Pa/s)
!---------------------------------------------------------------------------------------
function w2omega(w,p,tv); real::w2omega ! omega (Pa/s)
	!{Variaveis de entrada
	real,intent(in)::w !Velocidade vertical em m/s
	real,intent(in)::p ! Pressao atmosferica em Pa !Quidado!
	real,intent(in)::Tv ! Temperatura virtual em Kelvin
	!}
	real,parameter::Rd=287.05
	real,parameter::g=9.8
        
        if ((Tv>275).and.(P<50000)) then
          print *,":METLIB: w2omega: Warning: Inconsistent Virtual Temperature"
          print *,":METLIB: Pressure (Pa) =",P, "  Virt.Temp (K).=",Tv

        end if
        !w2omega=w*P*(exp(-g/(Rd*Tv))-1)
        w2omega=w*P*(-g/(Rd*Tv))
      

end function
!-----------------------------------------------------------------------------------------
!  c2omega 
!       Convert omega (pa/s) into  vertical velocity w(m/s)
!       Entre  p (Pa),w (m/s),tv (k)
!       saida: omega (Pa/s)
!---------------------------------------------------------------------------------------
function omega2w(omega,p,tv); real::omega2w ! vertical velocity (Pa/s)
	!{Variaveis de entrada
	real,intent(in)::omega !Velocidade vertical em pa/2
	real,intent(in)::p ! Pressao atmosferica em Pa !Cuidado!
	real,intent(in)::Tv ! Temperatura virtual em Kelvin
	!}
	real,parameter::Rd=287.05
	real,parameter::g=9.8
        
        if ((Tv>273).and.(P<50000)) then
          print *,":METLIB: omega2w: Error: Pressure (Pa) =",P, "  Virt.Temp (K).=",Tv
          stop
        end if
        !w2omega=w*P*(exp(-g/(Rd*Tv))-1)
        omega2w=-omega*((Rd*Tv)/g)/P
      

end function

 Function Tvirt_Ceusius(tt, TD, p);real :: Tvirt_Ceusius
!__________________________________________________________________
! Tvirt 
!     Dado a temperatura do ar (TT) ,a temperatura do ponto de orvalho (td),
!     e a pressao atmosferica (p), Rerona a Temperatura virtual em graus Ceusius 
!------------------------------------------------------------------    
      !{ Variaveis de entrada
       	real,intent(in)::tt,td   ! Temperaturas do ar e pto orvalho Graus Ceusius 
       	real,intent(in):: p ! Pressao atmosferica em hPa 
      !}
 
	If ((TD < 99).And.(p < 1100)) Then
		UE = 0.622 * PVAPOR(TD) / p ! Fun. umidade especifica
		Tvirt_Ceusius = (tt + 273.16) * (1 + 0.61 * UE) - 273.16
	Else
		Tvirt_Ceusius = tt
	End If

 End Function


 Function Tvirtw_Kelvin(tt, w);real :: Tvirtw_Kelvin
 !-----------------------------------------------------------------------------
 !|  Esta funcao retorna a Temperatura Virtual em Kelvin                      |
 !|  em funcao da temperatura do ar seco e da Razao de Mistura                |
 !|                                                                           |
 !|  Caso seja fornecido valor negativo para umidade especidica               |
 !|  a umidade sera considerada zero e a temperatura virtual igual a          |
 !|  a temperatura do ar                                                      |
 !|  Caso seja fornecido temperatura do ar inferior a 0 Kelvin  a funcao      |
 !|  retorna o valor null                                                     |
 !-----------------------------------------------------------------------------
      !{ Variaveis de entrada
       	real,intent(in)::tt ! Temperaturas do ar (kelvin) 
       	real,intent(in)::w ! Umidade especifica Kg/Kg
      !}
        rm=w 
	if (w<0) rm=0
	
	if (tt>0) then		
		Tvirtw_Kelvin = tt  * (1 + 0.60 * rm) 
	else
		Tvirtw_Kelvin=Null
	end if
	
 End Function



 Function pressure_isa(z);real::pressure_isa ! (hPa)
!------------------------------------------------------------------------------
!pressure_isa ! `Pressao da Atmosfera Padrao da OACI                    ! SHSF| 
!------------------------------------------------------------------------------
! -----------------------------------------------------------------------------
!{Variaveis de interface
 real,intent(in)::z ! Altitude (metros)
!}
!{ Variaveis locais
 real,Dimension(22,3):: np
 real::p,x1,x2
 !}
   Np(1,1)=1013.25  ; np(1,2)=0      ; np(1,3)=  15.0 
   Np(2,1)=1000     ; Np(2,2)=111    ; Np(2,3)=  14.3 
   Np(3,1)=950      ; Np(3,2)=540    ; Np(3,3)=  11.5 
   Np(4,1)=900      ; Np(4,2)=988    ; Np(4,3)=   8.6 
   Np(5,1)=850      ; Np(5,2)=1457   ; Np(5,3)=   5.5 
   Np(6,1)=800      ; Np(6,2)=1949   ; Np(6,3)=   2.3 
   Np(7,1)=700      ; Np(7,2)=3012   ; Np(7,3)=  -4.6 
   Np(8,1)=600      ; Np(8,2)=4206   ; Np(8,3)= -12.3 
   Np(9,1)=500      ; Np(9,2)=5574   ; Np(9,3)= -21.2 
   Np(10,1)=400     ; Np(10,2)=7185  ; Np(10,3)=-31.7 
   Np(11,1)=300     ; Np(11,2)=9164  ; Np(11,3)=-44.5 
   Np(12,1)=250     ; Np(12,2)=10363 ; Np(12,3)=-52.3 
   Np(13,1)=226     ; Np(13,2)=11000 ; Np(13,3)=-56.5 
   Np(14,1)=200     ; Np(14,2)=11784 ; Np(14,3)=-56.5 
   Np(15,1)=150     ; Np(15,2)=13608 ; Np(15,3)=-56.5 
   Np(16,1)=100     ; Np(16,2)=16180 ; Np(16,3)=-56.5 
   Np(17,1)=70      ; Np(17,2)=18442 ; Np(17,3)=-56.5 
   Np(18,1)=50      ; Np(18,2)=20576 ; Np(18,3)=-55.9 
   Np(19,1)=40      ; Np(19,2)=22000 ; Np(19,3)=-54.5 
   Np(20,1)=30      ; Np(20,2)=23849 ; Np(20,3)=-52.7 
   Np(21,1)=20      ; Np(21,2)=26481 ; Np(21,3)=-50.0 
   Np(22,1)=10      ; Np(22,2)=31055 ; Np(22,3)=-45.4 


   !{ Valor Invalido de PRessao
    if (z>100000) then
      pressure_isa=null
      return
    end if
    if (z <= 0) then 
      pressure_isa=1013.25
      return
    end if
    
    If ((z >= np(1,2)).And.(p <= np(22,2))) Then
      do  n = 1,21
        If ((z >= np(n, 2)).And.(z < np(n + 1, 2))) Then
          if (z==np(n,2)) then 
            p=np(n,1)
            exit
          else
            x1 = log(np(n, 1))
            x2 = log(np(n + 1, 1))
            p = exp(ITPL(np(n, 2),x1,np(n + 1, 2),x2, z))
            exit !
          end if
        End If
      end do
    Elseif (z<np(1,2)) then ! Altitude menor que o nivel do mar 
      x1 = log(np(n, 1))
      x2 = log(np(n + 1, 1))
      p = exp(ITPL(np(n, 2),x1, np(n + 1, 2),x2, z))
    elseif (z>np(22,2)) then ! Pressao acima de 10 hPa 
      x2 = log(np(n + 1, 1))
      p = exp(ITPL(np(n, 2),x1,np(n + 1, 2),x2, z))
    End If 
    pressure_isa = p
End Function pressure_isa


Function zpadrao(p);real::zpadrao  ! (metros)
	real,intent(in)::p ! Pressao em hPa
	zpadrao=altitude_isa(p)
end function


!------------------------------------------------------------------------------
!altitude_isa ! Altitude da Atmosfera Padrao da OACI                         ! SHSF| 
!------------------------------------------------------------------------------
! -----------------------------------------------------------------------------



Function altitude_isa(p);real::altitude_isa  ! (metros)

!{ Variaveis de interface
 real,intent(in)::p	 ! Pressao atmosferica (hPa)
!}

!{ Variaveis locais
 real,Dimension(22,3):: np
 real::x1,x2,z
!}

	Np(1,1)=1013.25  ; np(1,2)=0      ; np(1,3)=  15.0 
	Np(2,1)=1000     ; Np(2,2)=111    ; Np(2,3)=  14.3 
	Np(3,1)=950      ; Np(3,2)=540    ; Np(3,3)=  11.5 
	Np(4,1)=900      ; Np(4,2)=988    ; Np(4,3)=   8.6 
	Np(5,1)=850      ; Np(5,2)=1457   ; Np(5,3)=   5.5 
	Np(6,1)=800      ; Np(6,2)=1949   ; Np(6,3)=   2.3 
	Np(7,1)=700      ; Np(7,2)=3012   ; Np(7,3)=  -4.6 
	Np(8,1)=600      ; Np(8,2)=4206   ; Np(8,3)= -12.3 
	Np(9,1)=500      ; Np(9,2)=5574   ; Np(9,3)= -21.2 
	Np(10,1)=400     ; Np(10,2)=7185  ; Np(10,3)=-31.7 
	Np(11,1)=300     ; Np(11,2)=9164  ; Np(11,3)=-44.5 
	Np(12,1)=250     ; Np(12,2)=10363 ; Np(12,3)=-52.3 
	Np(13,1)=226     ; Np(13,2)=11000 ; Np(13,3)=-56.5 
	Np(14,1)=200     ; Np(14,2)=11784 ; Np(14,3)=-56.5 
	Np(15,1)=150     ; Np(15,2)=13608 ; Np(15,3)=-56.5 
	Np(16,1)=100     ; Np(16,2)=16180 ; Np(16,3)=-56.5 
	Np(17,1)=70      ; Np(17,2)=18442 ; Np(17,3)=-56.5 
	Np(18,1)=50      ; Np(18,2)=20576 ; Np(18,3)=-55.9 
	Np(19,1)=40      ; Np(19,2)=22000 ; Np(19,3)=-54.5 
	Np(20,1)=30      ; Np(20,2)=23849 ; Np(20,3)=-52.7 
	Np(21,1)=20      ; Np(21,2)=26481 ; Np(21,3)=-50.0 
	Np(22,1)=10      ; Np(22,2)=31055 ; Np(22,3)=-45.4 


   !{ Valor Invalido de PRessao
   
	if ((p<0).or.(p>1100)) then
		altitude_isa=null
		return
	end if

	If ((p <= np(1,1)).And.(p >= np(22,1))) Then
	
		do  n = 1,21
			If ((p <= np(n, 1)).And.(p > np(n + 1, 1))) Then
				if(p==np(n,1)) then
					z=np(n,2)
					exit
				else
					x1 = Log(np(n, 1))
					x2 = Log(np(n + 1, 1))
					z = ITPL(x1, np(n, 2), x2, np(n + 1, 2), Log(p))
					exit !
				end if
			End If
		end do
	
	Elseif (p>np(1,1)) then ! Pressao abaixo do nivel do mar 
				x1 = Log(np(1, 1))
				x2 = Log(np(2, 1))
				z = ITPL(x1, np(n, 2), x2, np(n + 1, 2), Log(p))
	
	elseif (p<np(22,1)) then ! Pressao acima de 10 hPa 	
				x1 = Log(np(21, 1))
				x2 = Log(np(22, 1))
				z = ITPL(x1, np(n, 2), x2, np(n + 1, 2), Log(p))
    End If
    
	altitude_isa = z

End Function altitude_isa


!_______________________________________________________________________________
! round | Arredondamento  (rounding)                                    | SHSF                
!-------------------------------------------------------------------------------
! Obtem o valor aproximado de um numero X com um numero de casas decimais C
!------------------------------------------------------------------------------ 
Function ROUND(x, C);real:: ROUND
   round = Int(x * 10 ** C + sign(0.5,x)) / 10 ** C!: !: Def. fun. de arredondamento
End Function
!-------------------------------------------------------------------------------

 
Function ITLINEAR(x1, Y1, x2, Y2, IT, MAXY, NULY);real::ITLINEAR
    real,intent(in)::x1,y1,x2,y2,IT,maxy,nuly
    If ((Y2 >= MAXY).Or.(Y1 >= MAXY)) Then
      ITLINEAR = NULY
    Else
      ITLINEAR = (Y2 * (IT - x1) + Y1 * (x2 - IT)) / (x2 - x1)
    End If
End Function


!-------------------------------------------------------------------
!PVAPOR | Saturated vapor pressure     (Tetens)                | SHSF
!-------------------------------------------------------------------
Function PVAPOR(tt);real::PVAPOR
  !{ Variaveis da interface
   real,intent(in)::tt ! Temperatura em Celsius
  !}
    if (tt > 0) then
       PVAPOR = 6.1078 * 10 ** (7.5 * tt / (237.3 + tt)) ! tensao do vapor sobre AGUA
    elseIf (tt > -100) Then
      PVAPOR = 6.1078 * 10 ** (9.5 * tt / (265.5 + tt))! tensao do vapor sobre GELO
    Else
      PVAPOR = 0
    End If
End Function


!------------------------------------------------------------------
!RMIST | Razao de Mistura Saturada em g/kg                   | SHSF 
!------------------------------------------------------------------
Function RMIST(E, p);real::RMIST  
  !{ interface
    real,intent(in)::E ! Tensao do Valor (hPa)
    real,intent(in)::P ! Pressao Atmosferica (hPa)
  !}
  RMIST = 622 * E / (p - E) 
End Function
!__________________________________________________________________
!------------------------------------------------------------------
Function q_humid1(E, P);real::q_humid1 !Umidade Especifica em  g/Kg

	real,intent(in)::E ! Actual vapor pressure (hPa)
	real,intent(in)::P ! Atmospheric pressure  (hPa)

	q_humid1 = 622 * E / (P - 0.378*E)

end Function
!__________________________________________________________________
! water vapor mixing ratio
!------------------------------------------------------------------
Function RMIST2(RH,TT,PP);real ::RMIST2 
 	real,intent(in)::RH   ! Relative Humidith (%)
        real,intent(in)::TT   ! Temperature (C)
        real,intent(in)::PP   ! Pressure (hPa)

       real:: e  ! Actual vapor pressure 
       e=pvapor(TT)*RH/100.0
       RMIST2 = 622 * E / (pp - E) 
end function
!-------------------------------------------------------------------------------
!Rseca | Draw Dry Adiabatic Rate or Potencial Temperature isoline        | SHSF                           
!-------------------------------------------------------------------------------
!       Esta funcao retorna temperaturas em Ceusius obtidas atraves
!       da razao adiabatica seca
!       This function returns temperatures in Ceusius obteined throuth 
!       the Dry Adicapatic Rate 
!------------------------------------------------------------------------------
Function Rseca_c(tt, PP1, PP2);REAL::Rseca_c
  !{ Interface 
   real,intent(in)::tt   ! Air temperature in Ceusius at level PP1
   real,intent(in)::pp1  ! Pressure Level 1
   real,intent(in)::pp2  ! Pressure Level 2 
   !}
   Rseca_c = (273.16 + tt) * ((PP2 / PP1) ** (2.0 / 7.0)) - 273.16 
End Function
!-------------------------------------------------------------------------------
!Rseca | Draw Dry Adiabatic Rate or Potencial Temperature isoline        | SHSF                           
!-------------------------------------------------------------------------------
!       Esta funcao retorna temperaturas em Kelvin obtidas atraves
!       da razao adiabatica seca
!       This function returns temperatures in Kelvin obteined throuth 
!       the Dry Adicapatic Rate 
!------------------------------------------------------------------------------
Function Rseca(tt, PP1, PP2);REAL::Rseca
  !{ Interface 
   real,intent(in)::tt   ! Air temperature in Kelvin at level PP1
   real,intent(in)::pp1  ! Pressure Level 1
   real,intent(in)::pp2  ! Pressure Level 2 
   !}
    if ((tt>0).and.(PP2>0).AND.(PP1>0)) then 
     Rseca = tt * ((PP2 / PP1) ** (2.0 / 7.0))
    else
     rseca=0
    end if
End Function


!------------------------------------------------------------------------------
!Rumida | Draw Wet Adiabatic Rate                                      | SHSF
!-------------------------------------------------------------------------------
!     Esta funcao retornat temperaturas em Ceusius obtidas atraves da
!     razao adiabatica umida
!     This function returns temperatures in Ceusius though the
!     wet adiabatic rate
!------------------------------------------------------------------------------
 Function Rumida_c(pl,plt,tc); real::Rumida_c
  real,intent(inout)::pl
  real,intent(inout)::plt
  real,intent(in)::tc

  Rumida_c=Saturation_Adiabatic(pl,tc+273.16,plt)-273.16


 end function
!------------------------------------------------------------------------------
!Rumida | Draw Wet Adiabatic Rate                                      | SHSF 
!-------------------------------------------------------------------------------
!     Dada um nivel isobatico de referencia e uma temperatura no mesmo
!     nivel, esta funcao retorna o valor de temperatura equivalente ao longo da
!     linha adiabatica saturada.
!
!     Nota:
!      Valor maximo de Temperatura Potencial Equivale = 500K
!      Valor maximo de temperatura a 1000 hPa 40 (C)
!      Acima destes valores assumido lapse_rate de 6.5 C/Km
!
!     Given an isobaric reference level and a temperature at the same
!     level, this function returns the equivalent temperature value along the
!     saturated adiabatic line
!
!     Note:
!      Maximum value of Equivalent Potencial Temperature = 500 K
!      Maximum value of Air Tenmperatture at 1000 hPa = 40 (c)
!      Abouve this value the lapse rate of 6.5 C/Km  is adopted
!---------------------------------------------------------------------------
! 20240309 -SHSF the 3nd guess (by proportionality) has been revided.  A defese has been included
Function Saturation_Adiabatic( pl, tt, plt);real :: Saturation_Adiabatic
!{ Interface
    REAL,INTENT(IN):: PL  ! Pressure at reference level ZR (hPa)
    REAL,INTENT(IN):: TT  ! Temperature (K) at  reference level
    REAL,INTENT(IN):: PLT ! Pressure (hPa) at the desired level for calculation

!{ local variables 
	REAL,DIMENSION(3)::T,TEC,DTE! T=TEMP CALCULADA
    real:: RCP,CP,ALCP,DT2,DT3,dz,es,r,TE
    real::lapse_rate
 !}
	
!{init variables **
        RCP = 287.0 / 1004.0
        CP = 0.238
        alcp = 2500000.0 / 1004.0
        DT2 = 0.0   ! old increment
        DT3 = 2.0   ! Actual increment
!}    
        dz = (altitude_isa(plt) - altitude_isa(pl)) / 1000.0 !thickness estimation of the providade layer
       

        TR = tt  ! Tempearature at reference level
        !--------------------------------------------------
        ! CALCULO OF EQUIAVALENTE POTENCIAL TEMPERATURE AT FIRT LEVEL (TE)
        !----------------------------------------------------
     
        TTO = 273.16 / TR
        ES = 6.11 * (TTO ** 5.31) * Exp(25.22 * (1 - TTO))
        r = (622.0 * ES / (pl - ES)) / 1000.0
        TE = TR * ((1000.0 / (pl - ES)) ** RCP) * Exp(alcp * r / TR)

        if (TE >500.0) then
          T(3)=TR - DZ * 6.5
          goto 622
        end if
        !----------------------------------------------------
        ! FIRST AND SECOND GUESS  (Lapse rate of  6.5 GRAUS/KM)
        !-----------------------------------------------------
    
        T(1) = TR - DZ * 6.5
        T(2) = T(1) + sign(1.0,dz)
        DO i = 1,2
          TTO = 273.16 / T(i)
          ES = 6.11 * TTO ** 5.31 * Exp(25.22 * (1 - TTO))
          !Es=pvapor(T(I)-273.16)
          r = (622.0 * ES / (plt - ES)) / 1000.0

          TEC(i) = T(i) * ((1000.0 / (plt - ES)) ** RCP) * Exp(alcp * r / T(i))
          DTE(i) = TEC(i) - TE
        END DO
     
        !------------------------------------------------
        ! TERCEIRA ESTIMATIVA - POR PROPORCIONALIDADE
        !------------------------------------------------
        !T(3) = (DTE(1) * T(2) - DTE(2) * T(1)) / (DTE(1) - DTE(2))
        T(3) = ((TE-TEC(1))*(T(2)-T(1))/(TEC(2)-TEC(1)))+T(1)

621     TTO = 273.16 / T(3)
        ES = 6.11 * TTO ** 5.31 * Exp(25.22 * (1 - TTO))
        r = (622.0 * ES / (plt - ES)) / 1000.0
        TEC(3) = T(3) * ((1000.0 / (plt - ES)) ** RCP) * Exp(alcp * r / T(3))
        DTE(3) = TEC(3) - TE

        if (abs(TEC(3))>500.0) then
          T(3)=T(1)
          goto 622
        end if
        !--------------------------------------------
        ! FIM DA TERCEIRA ESTIMATIVA T(3)
        ! VERIFCACAO E APURACAO DA PRECISAO DE CALCULO
        !---------------------------------------------
    If (DTE(3) * DT2 < 0)  DT3 = DT3 + 1.0
        DT2 = DTE(3)

        DIFER = Int(Abs(DTE(3) * 10) + 0.5)
        If (DIFER > 0) Then
            T(3) = T(3) - (DTE(3) / DT3)
           GoTo 621
        End If
 622   Continue
        lapse_rate=(T(3)-tt)/dz
       ! print *,"lapse_rate,T1,DZ,T3=",lapse_rate,tt,dz,T(3)
         Saturation_Adiabatic = T(3)

End Function

function temperature_lapse_rate(t1,p1,p2); real::temperature_lapse_rate
    real,intent(in)::t1 ! Temperature (Celsiuns, or Kelvin) at level 1
    real,intent(in)::p1 ! Atmospheric pressure (hPa) at level 1
    real,intent(in)::p2 ! Atmospheric Pressure (hPa) at level 2

     real::dz,dt,t2
     dz = (altitude_isa(p2) - altitude_isa(p1))/1000.0  !Estimation of layer Thickness (km)
     dt=6.5*dz
     t2=t1-dt
     temperature_lapse_rate=t2
end function

!-------------------------------------------------------
! Dewpoint 
!-----------------------------------------------------
! Calcula a temperatura do ponto de orvalho em (C)
! 
     function dewpoint(q,p); real::dewpoint
!{
         real,intent(in)::q ! Razao de mistura (g/kg)
         real,intent(in)::p !pressao reduzida (hPa)
!}
         real           ::e

         e=q*p/(622+0.378*q)
         dewpoint=(186.4905-237.3*log(e)/log(10.0))/(log(e)/log(10.0)-8.2859)

     end function dewpoint


!----------------------------------------------------
!      TDVUV E TUVDV  TRANSFORMACAO DE DIRECAO E VELOCIDADE 
!               PARA COMPONENTES X E Y E VICE-VERSA
!---------------------------------------------------
	SUBROUTINE TDVUV(DIR,VEL,CX,CY)
	 real,intent(in)::DIR,VEL
	 real,intent(out)::CX,CY
	   if ((Dir>=0.0).and.(Dir<=360.0).and.(vel>=0)) then 
	        CX = -VEL * SIN(DIR*pi/180.0)
        	CY = -VEL * COS(DIR*pi/180.0)
           else
                CX=null
                CY=null
           end if
	END subroutine
!______________________________________________________	
	SUBROUTINE TUVDV(U,V,DIR,VEL)
	  real,intent(in)::U,V ! componentes zonal e meridional
          real,intent(out)::DIR,VEL ! Direcao e velecidade

      	  VEL = SQRT(U ** 2 + V ** 2)
	 
	  if (vel/=0.0) then 
	    cddd=-V/VEL
	    IF(CDDD==1.0)  DIR=0.0
	    IF(CDDD==-1.0) DIR=180.0
	    IF((CDDD/=1.0).AND.(CDDD/=-1.0)) THEN
	      DIR=ACOS(CDDD)*180.0/pi 
              IF (DIR<=0) DIR=360.0+DIR
	      IF (-U<0) DIR=360.0-DIR
	     
	    END IF
	   ELSE
	     DIR=0.0
	   END IF
	
           DIR=INT((DIR*1000)+0.5)/1000.0
	  END subroutine
!------------------------------------------------------------------------------
! Cart-Met-Trans2 | Transposição do direção do plano cartesiano-meteorologico  
!-----------------------------------------------------------------------------
! Esta função transpoe valores de direção em graus do plano de eixos cartesianos
! para o plano de eixos de convenção meterológica e vice e versa.
!
!  Metodo  de permuta de eixos   
!
!     dir2=acos(-sin(dir1))
!
!    que realiza a conversão da direção do plano cartesiano para o meteorológico, 
!    e vice-versa, baseado na permura de eixos.
!     Dir1 é a direção em um plano, dir2 a direção equivalente no outro plano.  
!
!     Considerando-se tambem que a funcao  acos resulta em angulo reduzidos ao 
!     primeiro e quarto quadrates,sao feitos testes adicionais de analise do
!     quadrante e correcao do valor 
!--------------------------------------------------------------------------------
!     Dados de entrada - Direção na convensao cartesiana (ou meteorologica)  (graus)
!     Resultado        - Direção na convensao meteorologica (ou cartesiana)  (graus)
!______________________________________________________	
	function cart_met_trans2(dir1);real :: cart_met_trans2
	  real,intent(in)::dir1   ! Direcao no plano cartesiano ou meteorológico em graus

          real*8 :: y2,x2  ! Componentes nos eixos permutados
          real*8 :: rad1   ! Dir1 em radianos 
          real*8 :: dir2   ! Direcao convertida em grais  
 

         ! Calculo da transformação da direção do referencial cartesiano-meteorologico ou vice versa
           rad1=dir1*pi/180.0
           dir2=acos(-sin(rad1))*180.0/pi
  
         ! Verifica se acos esta retornado valores entre {-90 e 90} 
         ! Se sim, convenrte para valores entre  {0 e 180}
          
                 if (dir2<=0)  dir2=360.0+dir2

         ! Analise de redução de quadrantes 
              y2=-cos(rad1)
              if (y2<0) dir2=360.0-dir2
              
         !Aproximacao para terceira casa decimal e conversao 360.0 = 0.0

              dir2=int((dir2*1000)+0.5)/1000 
              if (int(dir2)==360) dir2=0.0

         cart_met_trans2=dir2
	 
        end function
!------------------------------------------------------------------------------
! Cart-Met-Trans | Transposição do direção do plano cartesiano-meteorologico  
!-----------------------------------------------------------------------------
! Esta função transpoe valores de direção em graus do plano de eixos cartesianos
! para o plano de eixos de convenção meterológica e vice e versa.
!
!   Metodo de rotacao   
!--------------------------------------------------------------------------------
!     Dados de entrada - Direção na convensao cartesiana (ou meteorologica)  (graus)
!     Resultado        - Direção na convensao meteorologica (ou cartesiana)  (graus)
!______________________________________________________	
 function cart_met_trans(dir1);real :: cart_met_trans
     real,intent(in)::dir1
      real :: d
      d=-(90+dir1)
      if (d<0) d=360+d
      if (d<0) d=360+d
      cart_met_trans=d
  end function
!-----------------------------------------------
!  Obtem direcao do vento na forma de 16 pontos 
!   Cardial, colateral e subcolateral
!----------------------------------------------
        function winddir16(dir);character(len=3)::winddir16
         real,intent(in)::dir
         integer::i
         character(len=3),dimension(0:16)::d

         if ((dir>360.0).or.(dir<0)) then 
            print *,"Error: Value out of range in the windir 16_metlib" 
            i=0 
         else

         d(0)="N"
         d(1)="NNE"
         d(2)="NE"
         d(3)="ENE"
         d(4)="E"
         d(5)="ESE"
         d(6)="SE"
         d(7)="SSE"
         d(8)="S"
         d(9)="SSW"
         d(10)="SW"
         d(11)="WSW"
         d(12)="W"
         d(13)="WNW"
         d(14)="NW"
         d(15)="NNW"
         d(16)="N"
         i=int((dir/360.0*16.0)+0.5)
         end if
   
         write(winddir16,'(A3)')d(i)
   
         return
          
        end function
!'---------------------------------------------------------------------------
!'ITPL| INTERPOLACAO LINEAR ENTRE DOIS PONTOS                         |SHSF |
!'---------------------------------------------------------------------------
!' Esta funcao retorna o valor interpolado de x em funcao de y, onde 
!' sao dados 2 pontos p1(x1,y1) e p2(x2,y2) e o valor de X em 
!  um ponto P qualquer do espaco.  A partir deste 5 informacoes, 
!  calculoa e retorna o valor interpolado Y  
!'---------------------------------------------------------------------------
 Function ITPL(x1, y1, x2, y2, X); real::ITPL
!{ Variaveis da interface
	real,intent(in)::x1,y1 !Coordenadas do primeiro ponto 
	real,intent(in)::x2,y2 !Coordenadas do segundo ponto 
	real,intent(in)::X 	   !Valor da abissica para interpolcao 			   ! 
!}  
!{ Variaveis Locais
	real :: ddd
!}  

	
	ddd = x2 - x1
	If (ddd /= 0.0) Then
	  ITPL = (Y2 * (x - x1) + Y1 * (x2 - x)) / (x2 - x1) 
	Else
 	   ITPL = Y1
	End If
	
  End Function ITPL
!______________________________________________________

!'---------------------------------------------------------------------------
!'gravity| Gravidade local aproximada                                 |SHSF |
!'---------------------------------------------------------------------------
!' Gravidade local aproximada 
!' Para pontos proximos a superficie da Terra usar alt=0 
!'---------------------------------------------------------------------------

FUNCTION gravity(lat,alt); real::gravity
    !{Variaveis da interface
    real,intent(in)::lat ! Latitude ( graus e decimos)
    real,intent(in)::alt ! Altitude em relacao ao nive do mar (metros) :: considerar = 0  para uso meteorologico
    !}
    
    real::rlat
    rlat=lat*pi/180
    gravity=9.780318*(1+0.0053024*(sin(rlat)**2)-0.0000058*(sin(2*rlat)**2)) - 0.000003086*alt
end function

!------------------------------------------------------------------------------
!Tpe ! Temperatura potencial equivalente
!-------------------------------------------------------------------------------
function tpe(t,q,p);real::tpe
  !{ Variaveis de interface
    real,intent(in)::t
    real,intent(in)::q
    real,intent(in)::p ! Pressao em hectoPascal !cuitado!
  !}
  !{ Variaveis locais
    real::raz,tp,ee,tl
  !}
    
      raz=0.2854*(1.0-0.28*1e-3*q)
      tp=t*(1000./p)**raz
      ee=p*q/(622.0+q)
      if(ee.le.0.)then
       ee=0.01
      endif

      tl=(2840./(3.5*log(t)-log(ee)-4.805))+55.0
  
      tpe=tp*exp(((3.376/tl)-0.00254)*q*(1.0+0.81*1e-3*q))
end function


!------------------------------------------------------------------------------
!lintep ! Interpolacao da pressao segundo altura geopotencial 
!------------------------------------------------------------------------------
! Esta funcao obtem a pressao atmosferica entre duas alturas distintas.
! Os vetores de altura devem estar em ordem crescente.
!
function linterp(z_in,p_in,z_ref,undef);real::linterp
  !{ Variaveis de interface 
   real,dimension(:),intent(in)::z_in ! Vetor das alturas
   real,dimension(:),intent(in)::p_in ! Vetor dos valores de pressao
   real,intent(in)             ::Z_ref ! Altura de referencia para interpolacao
   real,intent(in)             ::Undef !Valor indefinido 
  !}
  !{variaveis locais
    integer::kmax,k
    real::aux
  !}
   aux=undef
   if (z_ref/=undef) then 
      kmax=ubound(z_in,1)
      !{ Interpolando pressao de superficie  entre dois niveis isobaricos
      do k=1,kmax-1
        if ((z_in(k)/=undef).and.(z_in(k+1)/=undef)) then 
          if ((z_ref>z_in(k)).and.(Z_ref<=(z_in(k+1)))) then  
            aux=exp(ITPL(Z_in(k),log(P_in(k)),Z_in(k+1),log(P_in(k+1)), Z_ref))
            exit
          end if
        end if
      end do
      !}
      !{ Caso nao tenha ocorrindo a interpolacao, procede a extrapolacao 
      if(aux==undef) then 
        do k=1,kmax-1
          if ((z_in(k)/=undef).and.(z_in(k+1)/=undef)) then 
            if (z_ref<z_in(k)) then  
              aux=exp(ITPL(Z_in(k),log(P_in(k)),Z_in(k+1),log(P_in(k+1)), Z_ref)) 
              exit
            end if
          end if
        end do
      end if
      !}
    !  if (aux/=undef) print *,P_in(k),aux,p_in(k+1)
    end if
    linterp=aux
end function
end module


