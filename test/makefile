# compiler
FC      := gfortran
GC      := gcc

hello: hello.o
	$(FC) -o hello hello.o

hello.o: hello.f90
	$(FC) -c hello.f90

clean:
	rm hello.o hello
