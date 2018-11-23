#!/bin/bash
# ---------------------------------------------------------------------------- #

set -e

pdflatex -shell-escape proposta.tex
pdflatex -shell-escape proposta.tex
pdflatex -shell-escape proposta.tex

xdg-open proposta.pdf

# ---------------------------------------------------------------------------- #
