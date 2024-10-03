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
MOD_OBJECTS = $(MOD_SOURCES:.f90=.o)

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o 
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# Pattern rules for modules in MODDIR
$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

# Pattern rules for modules in LDRDIR
$(LDRDIR)/lammpsio.o: $(LDRDIR)/lammpsio.f90
	$(FC) $(FCFLAGS) -c -o $@ $<


.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS)

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS)

test:
	echo $(MOD_OBJECTS)
