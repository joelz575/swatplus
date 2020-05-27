# compiler
FC      := gfortran
GC      := gcc

# source files and objects
SRCS = testF.f90
OBJS = $(patsubst %.f90, %.o, $(BUILDDIR)/$(SRCS))

CSRCS = testC.c
COBJS = $(patsubst %.c, %.o, $(CSRCS))

# program name
EXE = test
BUILDDIR = build

all: dir $(BUILDDIR)/$(EXE)

dir:
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/$(EXE): $(OBJS) $(COBJS)
	$(FC) $(OBJS) $(COBJS) -o $@

%.o: %.c
	$(GC) -c $<

$(OBJS): $(BUILDDIR)/%.o: %.f90
	$(FC) -c $< -o $@

clean:
	rm -r $(COBJS) $(BUILDDIR)
