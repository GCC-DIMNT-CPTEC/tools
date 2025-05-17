module msort
!-------------------------------------------------------------------------------
!   MSORT: MODULO PARA ORDENAÇÃO DE DADOS 
!-------------------------------------------------------------------------------
implicit none
public

 contains

!------------------------------------------------------------------------------
! sortr8 | Odernacão de valores reais de 8 bytes (Bubble sort)          |     |
!------------------------------------------------------------------------------
! Esta subrotina ordena uma lista de valores reais de 8 bytes utilizando o 
! algoritmo bubble sort. Ao invez de retornar os valores ordenados, retorna
! um vetor de indices (ponteiros) correspondente a posicao dos valores ordenados
!--------------------------------------------------------------------------------

  subroutine sortr8(V,nv,iv)
   !{variaveis de entrada
      real*8,dimension(:), intent(inout)::V  !Lista dos valores a serem ordenados 
      integer,             intent(in)  ::nv !Numero de valores da lista
      integer,dimension(:),intent(out) ::iv !Guarda as posicoes anteriores a ordenacao

   !}

    integer i,j
    logical::p
    real*8:: raux,iaux

 
    do i=1,nv
     iv(i)=i
     end do

    do j = 1, nv
    print *,"sort=",j
      p=.true.
      do i=1, nv-1
          if (v(i)>v(i+1)) then
            p=.false. 
            raux=v(i+1)
            v(i+1)=v(i)
            v(i)=raux
            iaux=iv(i+1)
            iv(i+1)=iv(i)
            iv(i)=iaux
          end if
        end do
       if (p) exit
     end do
   
 end subroutine sortr8
 
 !------------------------------------------------------------------------------
! sort4 | Odernacão de valores reais de 4 bytes (Bubble sort)          |     |
!------------------------------------------------------------------------------
! Esta subrotina ordena uma lista de valores reais de 4 bytes utilizando o 
! algoritmo bubble sort. 
!--------------------------------------------------------------------------------

  subroutine sortr4(V,nv)
   !{variaveis de entrada
      real,dimension(:), intent(inout)::V  !Lista dos valores a serem ordenados 
      integer,             intent(in)  ::nv !Numero de valores da lista

   !}

    integer i,j
    logical::p
    real:: raux,iaux

 
    
    do j = 1, nv
    !print *,"sort=",j
      p=.true.
      do i=1, nv-1
          if (v(i)>v(i+1)) then
            p=.false. 
            raux=v(i+1)
            v(i+1)=v(i)
            v(i)=raux
          end if
        end do
       if (p) exit
     end do
   
 end subroutine sortr4

end module

