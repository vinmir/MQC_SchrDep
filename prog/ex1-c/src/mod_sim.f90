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
elemental pure function phi_0(x)
!! Auto-função phi_0(x). É `elemental`, ou seja, pode ser atuada com arrays 1D.

!++ Variáveis e definições
! IN
real(dp), intent(in) :: x

! INOUT/OUT
real(dp) :: phi_0

! Local

!++ Execução
phi_0 = pi**(-0.25_dp)*exp(-x**2/2._dp)

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
elemental pure function psi_t0(x)
!! Estado psi(x,t=0). É `elemental`, ou seja, pode ser atuada com arrays 1D.

!++ Variáveis e definições
! IN
real(dp), intent(in) :: x

! INOUT/OUT
real(dp) :: psi_t0

! Local

!++ Execução
psi_t0 = 2**(0.5_dp)*pi**(-0.25)* Exp(-2*x**2)

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
elemental pure function Hermite(x,n)
!! Função de teste.

!++ Variáveis e definições
! IN
integer, intent(in) :: n ! Ordem do polinômio
real(dp), intent(in) :: x

! INOUT/OUT
real(dp) :: Hermite

! Local

!++ Inicialização
! if (modulo(n,2)==1) then
!     Hermite = 0._dp
! end if

!++ Execução
if (n==0) then
    Hermite=1._dp
else if (n==1) then
    Hermite=2*x
else if (n==2) then
    Hermite=4*x**2-2
else if (n==3) then
    Hermite=8*x**3-12*x
else if (n==4) then
    Hermite=16*x**4-48*x**2+12
else if (n==5) then
    Hermite=32*x**5-160*x**3+120*x
else if (n==6) then
    Hermite=64*x**6-480*x**4+720*x**2-120
else if (n==7) then
    Hermite=128*x**7-1344*x**5+3360*x**3-1680*x
else if (n==8) then
    Hermite=256*x**8-3584*x**6+13440*x**4-13440*x**2+1680
else if (n==9) then
    Hermite=512*x**9-9216*x**7+48384*x**5-80640*x**3+30240*x
else if (n==10) then
    Hermite=1024*x**10-23040*x**8+161280*x**6-403200*x**4&
        +302400*x**2-30240
else
    Hermite=0._dp
end if

end function 
!**********************************************************************************************************************************!


!**********************************************************************************************************************************!
subroutine calculate_coefficients(phi,cn,x)
!! Subrotina que calcula os coeficientes c_n até uma quantidade máxima n<=N.
!! Também retorna todos os phi_n associados a cada coeficiente, assim como os pontos da rede.

!++ Variáveis e definições
! IN

! INOUT/OUT
real(dp), allocatable, dimension(:,:),intent(out) :: phi ! phi(n,x), resp.
real(dp), allocatable, dimension(:),intent(out) :: cn ! Coeficientes cn(n)
real(dp), allocatable, dimension(:),intent(out) :: x ! x_j, posições da rede

! Local
integer :: M ! M+1: total de pontos da rede.
real(dp) :: dx ! Discretização da rede. Será atualizada com M.
real(dp), allocatable, dimension(:) :: psi ! psi(x,t=0), resp.
integer,parameter :: N=10 ! c_n: n=0,1,...,N
integer :: i ! Loop

!++ Inicialização
! x_j = -R_max + dx*j, j=0,...,M
! dx = 2*R_max/M e M=2*R_max/dx
! É melhor definir dx manualmente primeiro. Depois, deve-se "capar" M. Se M for arredondado, dx claramente mudará.
! Bastará atualizar o novo dx com este valor de M.
dx=1e-2_dp
M=floor(2._dp*R_max/dx)

! Inicialização de x e correção de dx:
allocate(x(0:M))
x(:) = linspace(-R_max,R_max,M+1)
dx = x(1)-x(0)

! Em seguida, inicialize phi
    ! Primeiro idx: n=0,...,N
    ! Segundo idx: referente a x_j, j=0,...,M
allocate(phi(0:N,0:M))
phi=0._dp
phi(0,:)=phi_0(x)
allocate(cn(0:N))
cn(:)=0._dp

! Inicialização de psi(x,t=0)
allocate(psi(0:M))
psi(:) = psi_t0(x)

!++ Execução
! Cálculo do primeiro coeficiente cn(0):
cn(0) = bode_array_int(phi(0,:)*psi(:),dx)

! phi_n, n>=1
do i=1,N
    ! ATENÇÃO: por padrão, os inteiros são de 32-bits, então haverá overflow em 2**10*factorial(10).
    ! Faça questão de calcular um destes valores como real(dp).
    phi(i,:) = pi**(-0.25_dp)*(2._dp**i*factorial(i))**(-0.5_dp)*Hermite(x,i)*exp(-x**2/2._dp)
    cn(i) = bode_array_int(phi(i,:)*psi(:),dx)
end do

do i=0,N
    print*, i, cn(i)
end do

!++ Rotinas internas
contains
    !**********************************************************************************************************************************!
    subroutine save_arr(arr)
    !! Subrotina de teste que salva o formato da função de onda

    !++ Variáveis e definições
    ! IN
    real(dp), intent(in), dimension(0:) :: arr

    ! INOUT/OUT

    ! Local
    integer :: ii, io_unit
    character(len=*),parameter :: path="./temp/arr.dat"

    !++ Inicialização
    open(newunit=io_unit, file=path, status="replace", action="write")

    !++ Execução
    do ii=0,M
        write(io_unit,*) x(ii), arr(ii)
    end do

    end subroutine 
    !**********************************************************************************************************************************!


end subroutine 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
subroutine time_evolution()
!! Subrotina que evolui temporalmente a função de onda psi_t0.

!++ Variáveis e definições
! IN

! INOUT/OUT

! Local
real(dp), allocatable, dimension(:,:) :: phi ! phi(n,x), resp.
real(dp), allocatable, dimension(:) :: cn ! Coeficientes cn(n)
real(dp), allocatable, dimension(:) :: En ! Autoenergias
real(dp), allocatable, dimension(:) :: x ! Posições da rede
complex(dp), allocatable, dimension(:,:) :: psi ! phi(x,t), resp.

integer, parameter :: Q=200 ! Q+1: total de elementos no tempo. 
real(dp), parameter :: t_max = 3*pi
real(dp) :: dx, dt, t ! dt = t_max/(Q); t_j = dt*j, j=0,...,Q
integer :: i,j,n ! Loop
complex(dp),parameter :: img=(0._dp,1._dp) ! Unidade imaginária.

integer :: iounit ! File

!++ Inicialização
call calculate_coefficients(phi,cn,x)
allocate(psi(0:ubound(x,1),0:Q)) ! Ubound(x,1): total de elementos do array x. Esta linha simplesmente inicializa um
                                 ! array com a mesma quantidade de elementos e indexação do array 1D x.
psi=0._dp
dt = t_max/Q
dx = x(1)-x(0)
allocate(En(0:ubound(cn,1))) ! Inicializa um array com a mesma qtd de elementos e idx do arr 1D cn.
En = [(0.5_dp+j, j=0,ubound(cn,1))] ! Implicit do para inicializar En rapidamente.

!++ Execução
do j=0,Q
    psi(:,j)=0
    t = j*dt
    do n=0,ubound(cn,1)
        psi(:,j) = psi(:,j) + cn(n)*phi(n,:)*exp(-img*En(n)*t)
    end do
end do

! Resta salvar os dados de psi^2:
open(newunit=iounit,file="out/psi_x_t.dat",status="replace",action="write")
write(iounit,*) "t; x; |psi(x,t)|^2"
do j=0,ubound(psi,2)
    do i=0,ubound(psi,1)
        write(iounit,*)  j, x(i), abs(psi(i,j))**2
    end do
end do
close(iounit)

! Puramente complementar; esta etapa determina se o produto escalar <psi|psi> é conservado no tempo.
open(newunit=iounit,file="out/psi_quad.dat",status="replace",action="write")
write(iounit,*) "t, <psi(t)|psi(t)>"
do j=0,Q
    ! Lembre-se: psi = psi(i,j), com i: x; j: t
    write(iounit,*) j*dt, bode_array_int(abs(psi(:,j))**2,dx)
end do

end subroutine 
!**********************************************************************************************************************************!

end module
