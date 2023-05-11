!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Módulo para salvar arquivos:
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
module zmod_write
use :: zmod_const
use :: zmod_sim

implicit none

!++ Declaração de variáveis do módulo:

!++ Declaração de rotinas do módulo:
contains
!**********************************************************************************************************************************!
subroutine write_program_info()
!! Subrotina que salva os parâmetros de entrada do programa.

!++ Variáveis e definições

! Local
character(len=*),parameter :: path="./out/program_info.txt"
integer :: io_unit

!++ Inicialização
open(newunit=io_unit,file=path,action="write",status="replace")

!++ Execução
close(io_unit)

end subroutine 
!**********************************************************************************************************************************!


end module