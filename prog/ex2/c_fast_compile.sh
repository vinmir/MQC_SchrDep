#!/usr/bin/env bash

# Este .sh será útil para verificar as mensagens de compilação do gfortran. Não tem muito uso além disto.
# Ocasionalmente, perceberá que o VS Code não gerará o .mod automaticamente. Isto acontecerá por causa de algum
# erro grave no script, algo como `call func(x)` (nunca use `call` contra uma `function`: talvez
# o `call` esteja remanescente após converter uma procedure de `subroutine` para `function`).

# Primeiro, garantirei que o caminho utilizado no script é o caminho do script, não o caminho de onde foi chamado:
script_dir=$(dirname "$0")
cd "$script_dir"

# Em seguida, bastará trocar o caminho para a pasta dos códigos-fonte
cd ./src

# Compilarei todos os arquivos .f90 ali presentes e farei a transferência do executável para a pasta raiz:
gfortran -O1 *.f90 -o "../exec"

