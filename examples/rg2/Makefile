# Compiler settings - can be customized.
FC = gfortran
FCFLAGS =
LDFLAGS = -llapack -lblas

# Directory structure
SRCDIR = ../../src
MODDIR = $(SRCDIR)/modules
LDRDIR = $(SRCDIR)/LammpsDumpReader/fortran
BUILDDIR = .

# File name
TARGET = rg2
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR
MOD_SOURCES = $(wildcard $(MODDIR)/*.f90) $(LDRDIR)/lammpsio.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(filter-out $(LDRDIR)/lammpsio.f90, $(MOD_SOURCES))) $(LDRDIR)/lammpsio.o

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o 
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90 $(MOD_OBJECTS)
	$(FC) $(FCFLAGS) -c -o $@ $<

$(LDRDIR)/lammpsio.o: $(LDRDIR)/lammpsio.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/io.o: $(MODDIR)/io.f90 $(MODDIR)/string_utils.o


.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS) $(LDRDIR)/*.o $(LDRDIR)/*.mod

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS) $(LDRDIR)/*.o $(LDRDIR)/*.mod

test:
	echo $(MOD_OBJECTS)
