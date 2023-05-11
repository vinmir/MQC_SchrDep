# MQC_SchrDep
Projeto de Mecânica Quântica Computacional em evolução temporal de funções de onda.

# Discussões-chave
## Potencial harmônico, estado coerente
A equação de Schrödinger independente do tempo pode ser utilizada para mapear numerica ou analiticamente os autoestados associados ao potecial harmônico (constantes $m,w,\hbar$ normalizadas):
$$-\frac{1}{2}\frac{d^2\psi_j}{dx^2} + \frac{x^2}{2}\psi_j = E_j\ \psi_j$$

Em seguida, dadas as condições iniciais da função de onda, pode-se aplicar a evolução temporal:
$$\Psi(x,t) = \sum_j c_j \psi_j(x) \exp(-\textrm{i}E_jt)$$

Neste subitem, estudou-se o chamado *estado inicial coerente*:
$$c_j = \sqrt{\frac{\langle n\rangle^j}{j!}\exp(-\langle n \rangle)},$$
onde $\langle n \rangle$ é um parâmetro de entrada que fixa a configuração inicial do oscilador. Mapeando os $j=0,\ldots,10$ primeiros autoestados para evoluir temporalmente a equação de Schrödinger, obtém-se, para $\langle n\rangle=1$:

![coerente_1](https://github.com/vinmir/MQC_SchrDep/assets/133194350/c55d0a4d-8169-4221-af83-48f5fd3cd235)

Numericamente, constata-se que o período de oscilação desta onda é de $2\pi$, ou seja, coerente com o resultado clássico analítico de $2\pi/\omega$ (recordando que $\omega=1$).

## Pacote gaussiano livre
Casoa a partícula esteja livre, a sua função de onda será uma interposição de ondas planas (i.e. onda gaussiana). A animação abaixo simula uma onda da forma
$$\Psi(x,t=0) = (2\pi)^{-1/4}\ \exp(5\textrm{i}(x+10))\ \exp\left(-\frac{(x+10)^2}{4}\right),$$
isto é, totalmente normalizada ($\int |\Psi(x,0)|^2\ \text{d}x$ = 1) e com momento inicial $p_0 = k_0 \hbar = 5$, em unidades normalizadas. Há barreiras físicas infinitas em $x=\pm 30$ para que as reflexões nas paredes sejam totalmente conservativas.
![ex2_livre](https://github.com/vinmir/MQC_SchrDep/assets/133194350/093727bb-18ea-4cb9-93d5-7766ffc77d46)

A evolução temporal da função de onda "alarga" o pacote gaussiano, aumentando a incerteza da localização da partícula ($\Delta x$ aumenta) enquanto concomitantemente reduz a rapidez de deslocamento da onda. Tal redução é gradualmente menor com o tempo ($\Delta p$ diminui). As relações descritas entre $\Delta x$ e $\Delta p$ são previstas pelo Princípio da Incerteza.

## Potencial barreira
Suponha a mesma situação de partícula livre, com as mesmas condições iniciais, mas agora com uma barreira potencial negativa $V_0 = -50$ de $0\le x \le 2$. Ao contrário do que se espera classicamente, haverá uma parcela refletida pela barreira negativa:
![2-b](https://github.com/vinmir/MQC_SchrDep/assets/133194350/b26a757a-8c73-49c2-8087-790fc3a0168d)

Esta situação é perfeitamente explicada pela teoria de reflexões e refrações de funções de onda unidimensionais (consulte Quantum Mechanics, de McIntyre).


# Referências
+ An Introduction to Computer Simulation Methods (Harvey Gould et. al.)
+ Computational Physics (Nicholas J. Giordano et. al.)
