# Compiler settings - can be customized.
FC = gfortran
FCFLAGS = -O2 -fopenmp
LINKFLAGS = -fopenmp -isysroot $(shell xcrun --show-sdk-path)

# Directory structure
SRCDIR = ../../src/
MODDIR = $(SRCDIR)/modules
BUILDDIR = .
LAMMPSIO_DIR = ~/wrk/LammpsDumpReader/fortran

# File name
TARGET = rdf
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR and other directories
MOD_SOURCES = $(LAMMPSIO_DIR)/lammpsIO.f90 $(MODDIR)/string_utils.f90 $(MODDIR)/global_types.f90 $(MODDIR)/math.f90 $(MODDIR)/coord_convert.f90 $(MODDIR)/statistics.f90 $(MODDIR)/io.f90 $(MODDIR)/physical_constants.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(filter-out $(LAMMPSIO_DIR)/lammpsIO.f90, $(MOD_SOURCES))) $(LAMMPSIO_DIR)/lammpsIO.o


# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o
	$(FC) $(LINKFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90 $(MOD_OBJECTS)
	$(FC) $(FCFLAGS) -c -o $@ $<

# Dependencies
$(MODDIR)/io.o: $(MODDIR)/string_utils.o

$(LAMMPSIO_DIR)/lammpsIO.o: $(LAMMPSIO_DIR)/lammpsIO.f90 

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(LAMMPSIO_DIR)/lammpsIO.o: $(LAMMPSIO_DIR)/lammpsIO.f90 $(MODDIR)/io.o $(MODDIR)/string_utils.o
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS) $(LAMMPSIO_DIR)/lammpsIO.o

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS) $(LAMMPSIO_DIR)/lammpsIO.o

test:
	echo $(TARGETOBJ)
