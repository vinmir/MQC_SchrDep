!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Módulo específico para rotinas gerais de simulação
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
module zmod_sim
use :: zmod_const
use :: zmod_mat
! use :: zmod_energy, only: find_eigenstates
implicit none

!++ Declaração de variáveis do módulo:

!++ Declaração de procedures do módulo:
contains
!**********************************************************************************************************************************!
elemental pure function psi_t0(x)
!! Estado psi(x,t=0). É `elemental`, ou seja, pode ser atuada com arrays 1D.

!++ Variáveis e definições
! IN
real(dp), intent(in) :: x

! INOUT/OUT
complex(dp) :: psi_t0

! Local

!++ Execução
psi_t0 = (2*pi*w**2)**(-0.25_dp)*exp(imag*k0*(x-x0))*exp(-(x-x0)**2/4._dp/w**2)
! psi_t0 = (2*pi*w**2)**(-0.25)*exp(imag*k0*x)*exp(-(x-x0)**2/4._dp/w**2)

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
pure elemental function potential(x)
!! Função que retorna V(x).

!++ Variáveis e definições
! IN
real(dp), intent(in) :: x

! INOUT/OUT
real(dp) :: potential

!++ Execução
if (0._dp<=x .and. x<=a) then
    potential=V0
else
    potential=0._dp
end if

! if (x>=a) then
!     potential=V0
! else
!     potential=0._dp
! end if

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
subroutine time_evolution()
!! Subrotina que evolui numericamente psi(x,t) utilizando o método de half-steps.

!++ Variáveis e definições
! IN

! INOUT/OUT

! Local
integer :: N ! x_i=-R_max + i*dx, i=0,...,N
real(dp),parameter :: dx=0.1_dp, dt=1e-4_dp
integer :: i,j ! Loop
real(dp), allocatable, dimension(:) :: x ! Determina os pontos da rede.
real(dp), allocatable, dimension(:) :: V ! Determina o potencial em cada sítio espacial da rede.
real(dp), dimension(:), allocatable :: prob, R_arr, I_arr, I_old ! Arrays de evolução; formato: (x). Utiliza o método do Giordano.
integer :: io_anim
character(len=1) :: sair

!++ Inicialização
N = ceiling(2*R_max/dx/4._dp)*4._dp ! Redefine o x_N; deixa de ser R_max e se torna (-R_max) + N*dx;
                        ! Também deve forçar N a ser múltiplo de 4 para a rotina de Bode.
allocate(x(0:N), V(0:N))
x = linspace(-R_max, -R_max + N*dx,N+1)
V = potential(x)
    ! V=0._dp
allocate(prob(0:N), R_arr(0:N), I_arr(0:N), I_old(0:N))
open(newunit=io_anim,file="out/psi_x_t.dat",action="write",status="replace")
write(io_anim,*) "# Primeira linha: x; demais linhas: prob(x)_t"
write(io_anim,*) x(:)

! Inicializa os arrays R, I e Prob:
R_arr = 0._dp; I_arr=0._dp; I_old=0._dp; prob=0._dp

prob(:) = abs(psi_t0(x))
write(io_anim,*) prob(:)

R_arr(:) = real(psi_t0(x),dp)
I_old(:) = aimag(psi_t0(x))

R_arr(0)=0._dp
R_arr(N)=0._dp
I_old(0)=0._dp
I_old(N)=0._dp


! Avança half-step de I:
do i=1,N-1
    I_arr(i) = I_old(i) + dt/(2*dx**2) * (R_arr(i+1) - 2*R_arr(i) + R_arr(i-1)) &
                -0.5_dp*V(i)*R_arr(i)*dt
end do
I_arr(0)=0._dp
I_arr(N)=0._dp
! Verifica a forma de I:
    ! call quick_save(I_old(:))
    ! read(*,*)
    ! call quick_save(R_arr(:))
    ! read(*,*)
    ! call quick_save(I_arr(:))
    ! read(*,*)

! !++ Execução
! ! O loop de evolução varia de j=1,M, contendo M. O array de probabilidade poderá ser avaliado de j=1,M.
j=1
do 
    do i=1,N-1
        ! Iteração primeiro por R ...
        R_arr(i) = R_arr(i) - dt/dx**2 * (I_arr(i+1)-2*I_arr(i)+I_arr(i-1)) &
                        + V(i)*I_arr(i)*dt
    end do
    I_old(:) = I_arr(:)
    do i=1,N-1
        ! ... depois por I
        I_arr(i) = I_arr(i) + dt/dx**2 * (R_arr(i+1)-2*R_arr(i)+R_arr(i-1)) &
                        - V(i)*R_arr(i)*dt
    end do
    prob(:) = R_arr(:)*R_arr(:) + I_arr(:)*I_old(:)
    
    if (mod(j,500)==0) then
        write(io_anim,*) prob(:)
    end if

    ! O bloco abaixo é necessário para cortar a animação.
    if (mod(j,4000)==0) then
        call quick_save(prob(:))
        print*,j, bode_array_int(prob(:),dx)
        print*, "sair? [s/n]"
        read(*,*) sair
        if (sair=="s") exit
    end if
    j=j+1
    ! if (j>M) exit

end do

! Salva o formato final da função de onda para economizar tempo na animação do gif:
call quick_save(prob(:))

!++ Rotinas internas
contains
    subroutine quick_save(arr)
    !! Rotina que rapidamente salva um array em um arquivo temporário.
    ! IN
    real(dp), intent(in), dimension(0:) :: arr
    ! Local
    integer :: io_unit
    integer :: ii ! Índice interno
    open(newunit=io_unit, file="./temp/arr.txt", status="replace",action="write")
    do ii=lbound(arr,1),ubound(arr,1)
        write(io_unit,*) x(ii), arr(ii)
    end do
    close(io_unit)
    end subroutine
end subroutine 
!**********************************************************************************************************************************!

! !**********************************************************************************************************************************!
! pure function Hamiltonian(arr,x,dx) result(new_arr)
! !! Operador Hamiltoniano que atua em arr.

! !++ Variáveis e definições
! ! IN
! real(dp), dimension(:), intent(in) :: arr
! real(dp), dimension(:), intent(in) :: x
! real(dp), intent(in) :: dx

! ! INOUT/OUT
! real(dp), dimension(:), allocatable :: new_arr

! ! Local

! !++ Execução

! end function 
! !**********************************************************************************************************************************!


end module
