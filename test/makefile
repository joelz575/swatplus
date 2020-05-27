# compiler
FC      := gfortran
GC      := gcc

# source files and objects
SRCS = testF.f90
OBJS = $(patsubst %.f90, %.o, $(SRCS))

# program name
EXE = test

COBJS = testC.o

$(EXE): $(OBJS) $(COBJS)
	$(FC) $(OBJS) $(COBJS) -o $@

%.o: %.c
	$(GC) -c $<

testF.o: testF.f90
	$(FC) -c $<

clean:
	rm -r $(OBJS) $(COBJS) $(EXE)
