!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Programa principal:
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
program main
use :: zmod_const
use :: zmod_sim
use :: zmod_mat
use :: zmod_test
use :: zmod_write
implicit none

!++ Declaração de variáveis:

!++ Inicialização:



!++ Execução:
call fix_const()
! print*, v0
call time_evolution()
! call sub_test()

end program


!##################################################################################################################################!
! Exemplos de rotinas, módulos, etc.
!##################################################################################################################################!

! !**********************************************************************************************************************************!
! subroutine sub_test()
! !! Subrotina de teste.

! !++ Variáveis e definições
! ! IN

! ! INOUT/OUT

! ! Local

! !++ Inicialização

! !++ Execução
! 
! end subroutine 
! !**********************************************************************************************************************************!

! !**********************************************************************************************************************************!
! function func_test()
! !! Função de teste.

! !++ Variáveis e definições
! ! IN

! ! INOUT/OUT
! real(dp) :: 

! ! Local

! !++ Inicialização
! 

! !++ Execução

! end function 
! !**********************************************************************************************************************************!

! !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! ! Módulo de teste:
! !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! module zmod_test
! implicit none
!
! !++ Declaração de variáveis do módulo:
!
! !++ Declaração de rotinas do módulo:
! contains
!
! end module