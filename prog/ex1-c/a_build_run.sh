#!/usr/bin/env bash

# Primeiro, garantirei que o caminho utilizado no script é o caminho do script, não o caminho de onde foi chamado:
script_dir=$(dirname "$0")
cd "$script_dir"

# Em seguida, bastará trocar o caminho para a pasta dos códigos-fonte
cd ./src

# Compilarei todos os arquivos .f90 ali presentes e farei a transferência do executável para a pasta raiz:
gfortran -Wall -Wextra -pedantic -fcheck=all *.f90 -o "../exec"

# Finalmente, basta retornar à pasta raiz e executar o programa
cd ..
echo -e "\n******************************************"
echo -e   "O programa foi compilado e será executado."
echo -e   "******************************************\n\n"
./exec

