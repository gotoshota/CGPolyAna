# Compiler settings - can be customized.
FC = gfortran
FCFLAGS = -O2 -Wall
LDFLAGS =

# Directory structure
SRCDIR = ../src
MODDIR = $(SRCDIR)/modules
LDRDIR = $(SRCDIR)/LammpsDumpReader
TESTDIR = .
BUILDDIR = $(TESTDIR)

# File name
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR
MOD_SOURCES = $(wildcard $(MODDIR)/*.f90) $(LDRDIR)/fortran/lammpsio.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(wildcard $(MODDIR)/*.f90)) $(LDRDIR)/fortran/lammpsio.o

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TESTDIR)/test.o
	$(FC) $(LDFLAGS) -o $@ $^

$(TESTDIR)/test.o: $(TESTDIR)/test.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c $(MODDIR)/global_types.f90
	$(FC) $(FCFLAGS) -c $(MODDIR)/string_utils.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(LDRDIR)/fortran/lammpsio.o: $(LDRDIR)/fortran/lammpsio.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod $(TESTDIR)/*.o $(PROGRAMS)

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod $(TESTDIR)/*.o $(TESTDIR)/*.mod $(PROGRAMS)

test:
	echo $(MOD_SOURCES)
	echo $(MOD_OBJECTS)
