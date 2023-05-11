!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
! Módulo de rotinas matemáticas
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
module zmod_mat
use :: zmod_const, only: sp,dp
implicit none

!++ Declaração de variáveis do módulo:

!++ Declaração de rotinas do módulo:
contains
!**********************************************************************************************************************************!
pure function linspace(a,b,N) result(lin_arr)
!! Equivalente ao linspace do Numpy. Gera um array de N pontos linearmente espaçados entre a e b.
!! OBS: inclui a e b.
! Conceitos preliminares:
!   N pontos => N-1 seções; dx = (b-a)/(N-1).
!   arr = [x_i] = [a + i dx], i=0,1,...,N-1
!   arr = a + dx*[i]
!   arr = a + dx*[0,1,...,N-1]

!++ Variáveis e definições
! IN
real(dp), intent(in) :: a,b ! Limites do array.
integer, intent(in) :: N ! Total de pontos.

! INOUT/OUT
real(dp),dimension(N) :: lin_arr

! Local
integer :: i ! Loop
real(dp) :: dx ! Espaçamento

!++ Inicialização
dx = (b-a)/(N-1)

!++ Execução
lin_arr = a + dx*[(i, i=0,N-1)]

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
recursive function root_bisection(func, a, b) result(root)
!! Rotina que determina a raiz de 'func' pelo método da bisseção, na faixa inicial [a,b].
!! OBS.: 'a' não necessariamente deve ser menor que 'b'. Se a função for func = (x-2)*(x+3),
!! [-5,+5] fornecerá a raiz -3 e [+5,-5] fornecerá a raiz +2. E retornada a primeira raiz mapeada no intervalo.
! O algoritmo é "inteligente". Ao invés de simplesmente usar [a,b], a rotina determina dez valores de x
! linearmente espaçados entre [a,b]. Em seguida, testa o valor da função em cada um desses valores de x.
! Caso existam xi, xj nesse conjunto de valores de x tais que func(xi)*func(xj)<0, então obrigatoriamente
! haverá uma raiz em [xi,xj].
! "OK, qual é a utilidade disso?". Simples: suponha f(x) = (x-2)*(x+3), que tem raízes +2 e -3.
! Considere a faixa [-5,5]. No algoritmo convencional, como f(-5)*f(5)>0, o programa seria interrompido
! no primeiro loop de iteração e acusaria que não há raiz no intervalo.
! Pelo método que implementei, a função eventualmente encontraria um valor xj que tem f(xj)<0, ou seja,
! encontraria uma nova faixa a partir de [a,b] que de fato contém uma raiz.
! Óbvio, eu poderia usar 1000 valores de x entre [a,b] no teste inicial, mas isso reduziria o desempenho
! do programa final. 10 são suficientes.

!++ Variáveis e definições
! IN
real(dp), external :: func
real(dp), intent(in) :: a,b

! INOUT/OUT
real(dp) :: root

! Local
integer,parameter :: N_trials=10
real(dp),dimension(N_trials) :: initial_trials, initial_func_vals
                                ! Serão feitas N_trials tentativas de encontrar uma região [xi, xj] 
                                ! de [a,b] tal que f(xi)*f(xj) < 0. Cada xi é um elemento de initial_trials
integer,dimension(N_trials) :: trial_check ! Se o elemento fi>0, o elemento de trial_check será 1.
                                           ! Se fi<0, trial_check_i = -1
real(dp) :: x0,x1,xm ! Variáveis no loop iterativo.
real(dp),parameter :: error=1e-8 ! Tolerância de erro na convergência da raiz.
integer :: i,j ! Loop

!++ Inicialização
root=0._dp
! Determina os elementos iniciais xi e fi de tentativa
initial_trials = linspace(a,b,N_trials) 
do i=1,N_trials
    initial_func_vals(i) = func(initial_trials(i))
end do
! Determina os elementos de trial_check:
    ! +1 se o valor da função é >0
    ! -1 se o valor da função é <0
    ! Lembre-se: `merge` é o equivalente ao `np.where`
trial_check = merge(1,-1,initial_func_vals>0._dp)

!++ Execução
! Se houver uma região válida, o algoritmo continuará.
! A região será válida se nem todos os elementos do array forem iguais:
    ! Condição abaixo: constata se todos os elementos do array são iguais ao primeiro elemento, ou seja,
    ! verifica se todos os elementos do array são iguais
if (all(trial_check==trial_check(1))) then
    print*, "[root_bisection]: impossível continuar; não foi detectada uma raiz entre [a,b]"
    stop
end if
! A região [a,b] é válida e tem pelo menos uma raiz. Tomemos os índices da região mais próxima:
i = minloc(trial_check, 1)
j = maxloc(trial_check, 1)

! Há uma raiz. Redefina x0 e x1:
x0 = initial_trials(i)
x1 = initial_trials(j)

! Inicia o loop da bisseção:
do
    ! Determina o ponto médio:
    xm = (x0+x1)/2._dp
    if (abs(func(xm)) < error) exit

    ! Determina a próxima divisão:
    if (func(x0)*func(xm)<0._dp) then
        ! a raiz está na faixa [x0,xm]
        x1 = xm
    else if (func(xm)*func(x1)<0._dp) then
        ! a raiz está na faixa [xm,x1]
        x0=xm
    else
        ! Pela maneira com que construí o algoritmo, o programa nunca deverá chegar neste bloco.
        ! Como ainda não encontrei um problema, manterei isto aqui.
        print*, "[root_bisection]: impossível continuar; não foi detectada uma raiz entre [x0,x1];&
                 & erro algorítmico, deveria sim ter raiz. Reveja o código-fonte"
        stop
    end if

end do

! Finalmente, retorna o valor da raiz final:
root = xm

end function 
!**********************************************************************************************************************************!


!**********************************************************************************************************************************!
recursive function root_secant(func, a, b) result(root)
!! Rotina que determina a raiz de uma função 'func' pelo método das secantes, com chutes iniciais
!! 'a' e 'b', tais que (a<b)
! Nota: esta função teve de ser colocada na forma "recursive" para funcionar com o método de localização
! de autovalor. Do contrário, haveria um crash no programa.
! Encontrei o motivo de haver um crash.
! O método de localização dos pontos de retorno usa a root_secant, ou seja, a função g_func usa a root_secant.
! No entanto, a raiz de g(E) é determinada também pela root_secant. O debugger deixa isto muito explícito:
! a root_secant é acionada primeiro. Quando g(E) é acionada, a root_secant é acionada. Ou seja, a root_secant
! chama a si própria.
! Descobri que não havia um problema intrínseco ao método de "wrapping" quando reparei que, após consertar
! o problema de NaN em sqrt(E-V(r)), o resultado convergia ao usar root_bisection em find_eigenstates, sem
! crash no GDB ou no terminal.

!++ Variáveis e definições
! IN
! Atenção: este é método "atual" para passar funções como argumentos. Usa um bloco de interface diretamente.
! O Chapman recomenda usar um simples "external", ao passo que o Metcalf e certos tópicos no SO sugerem
! o bloco de interface. Ambos os métodos funcionam bem, então continuarei com o simples "external".
! Este bloco de interface é requerido em situações mais complicadas, como discutido na seção 13.5 do Chapman.
! interface
!     function func(x)
!         use :: zmod_const, only: dp
!         implicit none
!         ! IN
!         real(dp), intent(in) :: x
!         ! OUT
!         real(dp) :: func
!     end function
! end interface
real(dp), external :: func
real(dp), intent(in) :: a,b
! INOUT/OUT
real(dp) :: root

! Local
real(dp) :: x0, x1, x2 ! Variáveis x_(i-1), x_i e x_(i+1), vide Meredith.
real(dp),parameter :: error=1.e-8_dp ! Tolerância de erro na convergência da raiz; também utilizado em f(a)=f(b)
! integer :: i

!++ Inicialização
root=0._dp
! Inicializa as posições iniciais, x0=a, x1=b:
x0=a; x1=b

!++ Execução
! Primeiro, deve-se determinar se será possível implementar o algoritmo em primeiro lugar.
! Se f(x0) = f(x1), o loop de iteração será +inf em todas as vezes.
if (  abs(func(x0)-func(x1)) < error ) then
    print*, "[root_secant]: impossível continuar; f(x0)=f(x1), levando a x2 = +inf"
    stop
end if

! Loop de determinação algorítmica da raiz:
! x_(i+1) = x_i - f(x_i)*( x_i - x_(i-1) )/( f(x_i) - f(x_(i-1) ))
do
    ! Determina o x_(i+1)
    x2 = x1 - func(x1)*(x1-x0)/(func(x1)-func(x0))
    ! Verifica se a precisão foi atingida
    if (abs(x2-x1)<error) exit
    ! Reinicia o algoritmo:
        ! x0 <- x1 e x1 <- x2
    x0 = x1
    x1 = x2

end do
! Retorna o valor da raiz na última iteração.
root = x2

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
function bode_integral(func,a,b) result(integral)
!! Rotina de integração de uma função 'func' pelo método de Bode no domínio [a,b].
!! Nota: [a,b] não tem restrição a<b, vide integral definida (orientação de [a,b] influi em sinal + ou -).
! Esta função é um wrapper para o [bode_array_int].
! Atenção: GDB provou ser extremamente útil neste passo. Não tem comparação, o código final fica limpo,
! sem a necessidade de introduzir `print`s constantemente. Aparenta funcionar perfeitamente bem. 

!++ Variáveis e definições
! IN
real(dp), external :: func
real(dp), intent(in) :: a,b

! INOUT/OUT
real(dp) :: integral

! Local
integer,parameter :: N=200 ! Total de seções. ATENÇÃO: N **deve** ser múltiplo de 4
real(dp), dimension(0:N) :: f_vec ! Será o f_vec mandado para o bode_array_int
real(dp) :: dx ! Separação da malha
integer :: i ! Loop

!++ Inicialização
! Construção de f_vec:
    ! Primeiro, tratarei f_vec como se fosse um x_vec = [x0,x1,...,xN]
f_vec = linspace(a,b,N+1) ! Motivo de N+1: tenho N seções, o total de pontos é N+1
    ! Em seguida, usarei que f_i = func(x_i):
do i=0,N
    f_vec(i) = func(f_vec(i))
end do
! Resta inicializar dx:
dx = (b-a)/N

!++ Execução
integral = bode_array_int(f_vec, dx)


end function 
!**********************************************************************************************************************************!


!**********************************************************************************************************************************!
function bode_array_int(f_vec,dx) result(integral)
!! Rotina de integração de um array pelo método de Bode.
!! f_vec: [f0,f1,...,fN]
!! dx: separação da malha; fator de integração.

!++ Variáveis e definições
! IN
real(dp), dimension(0:), intent(in) :: f_vec
real(dp), intent(in) :: dx

! INOUT/OUT
real(dp) :: integral

! Local
integer :: N

!++ Inicialização
! Como o array é da forma [f0,f1,...,fN], basta tomar N como o bound superior de f_vec
N = ubound(f_vec,1)
! Em seguida, encerre o programa caso N (= número de seções) não seja múltiplo de 4:
if (mod(N,4)/=0) then
    print*, "[bode_array_int]: array com seções não-múltiplas de 4 detectado."
    stop
end if

!++ Execução
! Consulte as anotações acerca do algoritmo numérico. Basta separar em seções f_j, com j uma PA, para
! cada um dos pontos na integral de x_0 a x_4.
integral = 7*(f_vec(0)+f_vec(N))   +&
           14*sum(f_vec(4:N-4:4))  +&
           32*sum(f_vec(1:N-1:2))  +&
           12*sum(f_vec(2:N-2:4))

integral = integral*2*dx/45
end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
pure function simpson_array_int(f_vec,dx) result(integral)
!! Rotina de integração de um array pela regra de Simpson.
!! f_vec: [f0,f1,...,fN]
!! dx: separação da malha; fator de integração.
!! Atenção: evite usar esta rotina. É, no geral, consideravelmente inferior a Bode.

!++ Variáveis e definições
! IN
real(dp), dimension(0:), intent(in) :: f_vec
real(dp), intent(in) :: dx

! INOUT/OUT
real(dp) :: integral

! Local
integer :: N

!++ Inicialização
! Como o array é da forma [f0,f1,...,fN], basta tomar N como o bound superior de f_vec
N = ubound(f_vec,1)
! Em seguida, encerre o programa caso N não seja múltiplo de 4:
if (mod(N,2)/=0) stop

!++ Execução
! Consulte as anotações acerca do algoritmo numérico. Basta separar em seções f_j, com j uma PA, para
! cada um dos pontos na integral de x_0 a x_2.
integral = 1*(f_vec(0)+f_vec(N))  +&
           4*sum(f_vec(1:N-1:2))  +&
           2*sum(f_vec(2:N-2:2))  


integral = integral*(dx/3)
end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
function desv_am(array)
!! Função que calcula o desvio padrão amostral de um array.

!++ Variáveis e definições
! IN
real(dp), intent(in), dimension(:) :: array ! Array de entrada.

! INOUT/OUT
real(dp) :: desv_am ! Desvio padrão amostral

! Local
integer :: n_elementos ! Elementos do array
real(dp) :: med, med_quad ! Médias <x> e <x^2>, resp.

!++ Inicialização
desv_am=0._dp
n_elementos = size(array)

!++ Execução
med = media_arr(array)
med_quad = media_arr(array*array)

! Atenção: devido a erros numéricos, é capaz de o termo (med_quad-med*med) ser <0. Portanto, basta forçar o uso de
! abs.
desv_am = sqrt(real(n_elementos,dp)/(n_elementos-1) * abs(med_quad - med*med))


end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
function media_arr(array) result(med)
!! Função que calcula a média dos elementos do array.

!++ Variáveis e definições
! IN
real(dp), intent(in), dimension(:) :: array

! INOUT/OUT
real(dp) :: med

! Local

!++ Execução
med = sum(array)/real(size(array),dp)

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
pure function first_derivative(arr,dx) result(derivative)
!! Função que calcula a derivada primeira de um array `arr` com espaçamento dx. Retorna a derivada `derivative`.

!++ Variáveis e definições
! IN
real(dp), intent(in), dimension(0:) :: arr ! array de entrada; total de pontos: N+1
real(dp), intent(in) :: dx ! espaçamento da rede

! INOUT/OUT
real(dp), dimension(:), allocatable :: derivative

! Local
integer :: N ! N+1: total de pontos de arr
integer :: i ! Loop.

!++ Inicialização
N = ubound(arr,1) ! arr mapeado de 0,...,N

allocate(derivative(0:N))
derivative=0._dp

!++ Execução
! Para esta etapa, diferenças de segunda ordem são duficientes.:
derivative(0) = (arr(1)-arr(0))/dx
derivative(N) = (arr(N)-arr(N-1))/dx
    ! Nota: o erro nas extremidades é grande: O(dx).
    ! Em diferenças centrais, o erro é O(dx^2). Não tem o que fazer, tem falta de pontos na extremidade.

! Pontos onde é possível aplicar diferenças centrais:
do i = 1,N-1
    derivative(i) = (arr(i+1)-arr(i-1))/(2._dp*dx)
end do

end function 
!**********************************************************************************************************************************!

!**********************************************************************************************************************************!
function factorial(n) result(fact)
!! Função que calcula o fatorial n!

!++ Variáveis e definições
! IN
integer, intent(in) :: n

! INOUT/OUT
integer :: fact

! Local
integer :: i

!++ Inicialização
if (n==0) then
    fact=1
else
    fact=1
    do i=1,n
        fact=fact*i
    end do
end if

!++ Execução

end function 
!**********************************************************************************************************************************!




end module