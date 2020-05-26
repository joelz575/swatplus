hello: hello.o
	gfortran -o hello hello.o

hello.o: hello.f90
	gfortran -c hello.f90

clean:
	rm hello.o hello
