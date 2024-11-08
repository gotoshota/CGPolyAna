# Compiler settings - can be customized.
FC = gfortran
FCFLAGS =
LDFLAGS =

# Directory structure
SRCDIR = ../../src
MODDIR = $(SRCDIR)/modules
LDRDIR = $(SRCDIR)/LammpsDumpReader/fortran
BUILDDIR = .

# File name
TARGET = msd
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR
MOD_SOURCES = $(MODDIR)/global_types.f90 $(MODDIR)/string_utils.f90 $(MODDIR)/correlation_function.f90 $(MODDIR)/math.f90 $(MODDIR)/coord_convert.f90 $(LDRDIR)/lammpsio.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(filter-out $(LDRDIR)/lammpsio.f90, $(MOD_SOURCES))) $(LDRDIR)/lammpsio.o

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90 $(MOD_OBJECTS) $(LDRDIR)/lammpsio.o
	$(FC) $(FCFLAGS) -I $(LDRDIR) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90 $(LDRDIR)/lammpsio.o
	$(FC) $(FCFLAGS) -I $(LDRDIR) -c -o $@ $<

$(LDRDIR)/lammpsio.o: $(LDRDIR)/lammpsio.f90
	$(FC) $(FCFLAGS) -I $(LDRDIR) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS) $(LDRDIR)/*.o $(LDRDIR)/*.mod

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS) $(LDRDIR)/*.o $(LDRDIR)/*.mod

test:
	echo $(MOD_OBJECTS)


