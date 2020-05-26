# compiler
FC      := gfortran
GC      := gcc

hello: hello.o
	$(FC) gfortran -o hello hello.o

hello.o: hello.f90
	$(FC) gfortran -c hello.f90

clean:
	rm hello.o hello
