#!/bin/bash

# bash script loop through files
for f in sol.* ; do echo "plot [:][:] '$f' w l "; done | gnuplot -persist
#....ma tanto non funziona, mi rimane solo l'ultimo!

# Rimuove i files sol.*
#rm sol.*
