# Compiler settings - can be customized.
FC = gfortran
FCFLAGS = -O2 -fopenmp
LDFLAGS = -fopenmp -lprofiler

# Directory structure
SRCDIR = ../../src/
MODDIR = $(SRCDIR)/modules
BUILDDIR = .

# File name
TARGET = rdf
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR and other directories
MOD_SOURCES = ~/wrk/LammpsDumpReader/fortran/lammpsIO.f90 $(MODDIR)/global_types.f90 $(MODDIR)/math.f90 $(MODDIR)/coord_convert.f90 $(MODDIR)/statistics.f90 $(MODDIR)/io.f90 $(MODDIR)/physical_constants.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(filter-out ~/wrk/LammpsDumpReader/fortran/lammpsIO.f90, $(MOD_SOURCES))) ~/wrk/LammpsDumpReader/fortran/lammpsIO.o

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90 $(MOD_OBJECTS)
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

~/wrk/LammpsDumpReader/fortran/lammpsIO.o: ~/wrk/LammpsDumpReader/fortran/lammpsIO.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS) ~/wrk/LammpsDumpReader/fortran/lammpsIO.o

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS) ~/wrk/LammpsDumpReader/fortran/lammpsIO.o

test:
	echo $(TARGETOBJ)

