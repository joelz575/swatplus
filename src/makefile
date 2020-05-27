# compiler
FC      := gfortran
GC      := gcc

#ToDo: shell script, um json-c in den pfad zu bringen (mit Git) depth=1? oder eine environment variable um json-c
# compile flags
FCFLAGS = -g -ffree-line-length-none -Jbuild

CCFLAGS = -g #-m64
JSON_C_DIR= json-c
CCFLAGS += -I/usr/local/include/json-c

# link flags
FLFLAGS = -ffree-line-length-none -Jbuild
LDFLAGS += -L$(JSON_C_DIR)/lib

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
	$(FC) $(CCFLAGS) $(FLFLAGS) $^ -o $@

$(COBJS): $(BUILDDIR)/%.o: %.c
	$(GC) $(CCFLAGS) -c $< -o $@ $(LDFLAGS) -ljson-c

$(OBJS): $(BUILDDIR)/%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -o $@

%.mod: %.f90 %.F90
	$(FC) -c  $< -o $@

clean:
	rm -r $(BUILDDIR)
