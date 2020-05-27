# compiler
FC      := gfortran
GC      := gcc

# source files and objects
SRCS = hru_module.f90 \
	organic_mineral_mass_module.f90

OBJS = $(patsubst %.f90, $(BUILDDIR)/%.o, $(SRCS))
$(info    $(SRCS))
$(info    $(OBJS))
CSRCS = socket.c
COBJS = $(patsubst %.c, $(BUILDDIR)/%.o, $(CSRCS))

# program name
EXE = test
BUILDDIR = build

all: dir $(BUILDDIR)/$(EXE)

dir:
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/$(EXE): $(OBJS) $(COBJS)
	$(FC) $^ -o $@

$(COBJS): $(BUILDDIR)/%.o: %.c
	$(GC) -c $< -o $@

$(OBJS): $(BUILDDIR)/%.o: %.f90
	$(FC) -c $< -o $@

%.mod: %.f90 %.F90
	$(FC) -c  $< -o $@

clean:
	rm -r $(BUILDDIR)
