   module mcllgr
!------------------------------------------------------------------------------
!                                mcllgr
!
!         Circular linked list of binary files of grads  
!-----------------------------------------------------------------------------
!This module creates a data storage structure in the form of circular linked list.
! Each element in this list correspond to a time that is added at the end of the list.
! When it reaches the list limit, the oldest times are cleared for insertion 
! of the new time. 
! Each time contains all the variables and levels of a binary of the grads
!---------------------------------------------------------------------------------
!SHSF 20130925- Incial version 

  use mgrads

!------------------------------------------------------------------
! new | Create a new CCL file
!-----------------------------------------------------------------
 subroutine new_cllgr(ctlfile,cllfile,nt)

!{ Variaveis de interface
   character(len=*),intent(in)::ctlfile  !A ctl grads file used as model for criate de cll
   character(len=*),intent(in)::cclfile  !The CCL filename  
   integer                ::nt           !Number of times in CCL file
!}

 !{ variavelis locais

 type(bindef)::bi    
 real*8      ::d1,d2 !Initial and final time (days and fractions) 
 integer     ::ts    ! Times steps
!}

!{Openning input file   
   call openr_mgrads(bi,ctlfile,d1,d2,ts)   ! Arquivo da primeira grade 
                                            ! Insere o ultimo tempo para criar o arquivo
                                            ! Configura os pontoiros Head and tail
                                            ! grava um ccl

end subroutine 

end module
