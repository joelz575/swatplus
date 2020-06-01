# compiler
FC      := gfortran
GC      := gcc

# compile flags
FCFLAGS = -g -ffree-line-length-none -Jbuild

#CCFLAGS = -g #-m64
JSON_C_DIR= json-c
#CCFLAGS += -I/usr/local/include/json-c #fix for vcpkg

# link flags
#FLFLAGS = -ffree-line-length-none -Jbuild
#LDFLAGS+= -L$(JSON_C_DIR)/lib #fix for vcpckg

# source files and objects
SRCS =  hru_module.f90 \
	organic_mineral_mass_module.f90 \
	carbon_module.f90 \
	conditional_module.f90 \
	climate_module.f90 \
	plant_module.f90 \
	plant_data_module.f90 \
	mgt_operations_module.f90 \
	hru_lte_module.f90 \
	basin_module.f90 \
	time_module.f90 \
	hydrograph_module.f90 \
	output_landscape_module.f90 \
	constituent_mass_module.f90 \
	maximum_data_module.f90 \
	input_file_module.f90 \
	aquifer_module.f90 \
	calibration_data_module.f90 \
	output_ls_pesticide_module.f90 \
	ch_pesticide_module.f90 \
	channel_module.f90 \
	res_pesticide_module.f90 \
	channel_data_module.f90 \
	pesticide_data_module.f90 \
	output_ls_pathogen_module.f90 \
	channel_velocity_module.f90 \
	pathogen_data_module.f90 \
	landuse_data_module.f90 \
	dr_module.f90 \
	fertilizer_data_module.f90 \
	exco_module.f90 \
	output_ls_salt_module.f90 \
	hydrology_data_module.f90 \
	recall_module.f90 \
    sd_channel_module.f90 \
    tinamit_module.f90 \
    soil_module.f90 \
    tillage_data_module.f90 \
    reservoir_module.f90 \
    tiles_data_module.f90 \
    aqu_pesticide_module.f90 \
    water_body_module.f90 \
    reservoir_data_module.f90 \
    ru_module.f90 \
    urban_data_module.f90 \
    topography_data_module.f90 \
    septic_data_module.f90 \
    soil_data_module.f90 \
	$(wildcard *.f90 *.F90)
OBJS = $(patsubst %.f90, $(BUILDDIR)/%.o, $(SRCS))
#CRCS = socket.c
COBJS = $(patsubst %.c, $(BUILDDIR)/%.o, $(CRCS))

# program name
EXE = swatplus
BUILDDIR = build

all: dir $(BUILDDIR)/$(EXE)

dir:
	mkdir -p $(BUILDDIR)

%.mod:  %.f90 %.F90
	$(FC) -c -o $@ $<

#%.mod: %.f90 %.F90
#	echo $(OBJS)
#	$(FC) $(FCFLAGS) $< -c -o $@
#
$(OBJS): $(BUILDDIR)/%.o: %.f90 %.F90
	$(FC) $(FCFLAGS) $< -c -o $@

#$(COBJS): $(BUILDDIR)/%.o: %.c
#	$(GC) $(CCFLAGS) $< -c -o $@ $(LDFLAGS) -ljson-c

$(BUILDDIR)/$(EXE): $(OBJS) $(COBJS)
	$(FC) $(FLFLAGS) $^ -o $@