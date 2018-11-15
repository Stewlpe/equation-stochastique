set terminal png
set output "output/Tint.png"

set xlabel "Itérations en temps"
set ylabel "Température intérieure"

set title "Evolution de la température"


plot "Tint.txt" with lines title "Tint"
