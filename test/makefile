# compiler
FC      := gfortran
GC      := gcc

hello: hello.o
	$(GC) gfortran -o hello hello.o

hello.o: hello.f90
	$(GC) gfortran -c hello.f90

clean:
	rm hello.o hello
