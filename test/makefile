# compiler
FC      := gfortran
GC      := gcc

# source files and objects
SRCS = hello.f90
OBJS = $(patsubst %.f90, $(BUILDDIR)/%.o, $(SRCS))

# program name
EXE = hello
BUILDDIR = build

all: dir $(BUILDDIR)/$(EXE)

dir:
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/$(EXE): $(BUILDDIR)/hello.o
	$(FC) -o $(BUILDDIR)/hello $(BUILDDIR)/hello.o

$(OBJS): $(BUILDDIR)/%.o: %.f90 %.F90
	$(FC) $(FCFLAGS) $< -c -o $@

clean:
	rm -r $(BUILDDIR)
