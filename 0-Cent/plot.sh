#!/bin/bash

#  bash script loop through files
#echo "set output 'esempio.png';  plot [:][:] 'DatoIniziale' w l "  | gnuplot -persist
echo "set output 'esempio.png'; plot [:][:] 'DatoIniziale' w l,'sol.0150' w l, 'sol.0300' w l, 'sol.0450' w l, 'sol.0600' w l, 'sol.0750' w l, 'sol.0900' w l, 'sol.1050' w l, 'sol.1200' w l, 'sol.1350' w l, 'sol.1500' w l" | gnuplot -persist
#echo "set output 'esempio.png'; plot [:][:] 'DatoIniziale' w l,'sol.0020' w l, 'sol.0040' w l, 'sol.0060' w l, 'sol.0080' w l, 'sol.0100' w l, 'sol.0120' w l, 'sol.0140' w l, 'sol.0160' w l, 'sol.0180' w l, 'sol.0200' w l" | gnuplot -persist


#for f in sol.* ; do echo "set output 'esempio.png'; plot [:][:] '$f' w l "; done | gnuplot -persist

# Rimuove i files sol.*
#rm sol.*

#non funziona
#echo 	"set output 'esempio.png';  list(i) = system ('ls -1B sol.*',i);  plot  for [f in list]  '$f'  " | gnuplot -persist 
