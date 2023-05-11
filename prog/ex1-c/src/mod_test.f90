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
! IN

! INOUT/OUT

! Local
! real(dp), dimension(:),allocatable :: x,y
! integer :: i

!++ Inicialização

!++ Execução
! x=linspace(-R_max,R_max,2001)
! open(10,file="./temp/arr.dat",action="write")
! y=Hermite(x,10)
! print*,(2._dp**10*factorial(10))
! do i=1,2001
!     write(10,*) x(i),y(i)
! end do

end subroutine 
!**********************************************************************************************************************************!


end module