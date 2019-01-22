exe : main.f90
	gfortran  -g -O0 -Wall -pg -o exe main.f90
	./exe

plot : Tint.txt res.txt
	gnuplot AffErr.gnu
	gnuplot AffTint.gnu
