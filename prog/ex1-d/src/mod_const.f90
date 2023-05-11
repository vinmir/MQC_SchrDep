!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Módulo de constantes globais
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
module zmod_const
! use :: iso_fortran_env, only: sp=>real32, dp=>real64
implicit none

!++ Declaração de tipos de precisão:
integer, parameter :: sp = selected_real_kind(6,37)
integer, parameter :: dp = selected_real_kind(15,307)

!++ Declaração de variáveis do módulo:
real(dp), parameter :: pi=3.141592654_dp 
real(dp), parameter :: R_max=10._dp ! O "espaço" é definido de [-R_max,+R_max]
real(dp), protected :: med_n ! Entrada do programa. É o <n>.

!++ Declaração de rotinas do módulo (leitura das constantes, etc.):
contains
!**********************************************************************************************************************************!
subroutine fix_const()
!! Subrotina que fixa as constantes de simulação.

!++ Variáveis e definições
! Local
integer :: io_unit ! Unidade do arquivo de entrada.
character(len=*),parameter :: path="./in/input.txt" ! Caminho do arquivo de entrada.
integer :: i ! Loop

!++ Inicialização
! Abre o 'input file':
open(newunit=io_unit,file=path,action="read",status="old")

!++ Execução
! Pula as linhas de comentário:
do i=1,1
    read(io_unit,*)
end do
! Em ordem, faz a leitura dos parâmetros de simulação:
read(io_unit,*) med_n
! Fecha o arquivo:
close(io_unit)

end subroutine 
!**********************************************************************************************************************************!


end module
