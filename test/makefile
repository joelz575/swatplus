# compiler
FC      := gfortran
GC      := gcc

# source files and objects
SRCS = hello.f90
OBJS = $(patsubst %.f90, %.o, $(SRCS))

# program name
EXE = test

OBJS = testF.o testC.o

$(EXE): $(OBJS)
	$(FC) $(OBJS) -o test

testC.o: testC.c
	$(GC) -c testC.c

testF.o: testF.f90
	$(FC) -c testF.f90

clean:
	rm -r $(OBJS) $(EXE)
