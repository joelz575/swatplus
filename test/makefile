# compiler
FC      := gfortran
GC      := gcc

# program name
EXE = hello
BUILDDIR = build

all: dir $(BUILDDIR)/$(EXE)

dir:
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/$(EXE): hello.o
	$(FC) -o $(BUILDDIR)/hello hello.o

hello.o: hello.f90
	$(FC) -c hello.f90

clean:
	rm hello.o hello
