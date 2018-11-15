set terminal png
set output "Erreur.png"

set xlabel "Itérations en temps"
set ylabel "Erreur absolue"

set title "Courbe d'erreur"


plot "res.txt" with lines title "Erreur"
