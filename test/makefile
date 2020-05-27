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
	$(FC) $(OBJS) -o $@

%.o: %.c
	$(GC) -c $<

testF.o: testF.f90
	$(FC) -c $<

clean:
	rm -r $(OBJS) $(EXE)
