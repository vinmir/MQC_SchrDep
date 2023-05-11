!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Módulo de rotinas de teste
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
module zmod_test
use :: zmod_const, only: sp, dp
use :: zmod_sim
implicit none

!++ Declaração de variáveis do módulo:

!++ Declaração de rotinas do módulo:
contains
!**********************************************************************************************************************************!
subroutine sub_test()
!! Subrotina de teste.

!++ Variáveis e definições
! real(dp), dimension(:), allocatable :: x
! integer :: N=1000
! real(dp) :: dx
! x = linspace(0._dp,1._dp,N+1)
! dx=1._dp/N
! print*, maxval(abs(second_derivative(exp(2._dp*x),dx)-4._dp*exp(2._dp*x)))
! print*, second_derivative(x**2,dx)
! print*, 4*exp(2*1._dp)


end subroutine 
!**********************************************************************************************************************************!


end module