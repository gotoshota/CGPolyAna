# Compiler settings - can be customized.
FC = gfortran
<<<<<<< HEAD
FCFLAGS = -O2 -Wall -fdefault-integer-8
=======
FCFLAGS =
>>>>>>> 55cb8aabf10345c90ed4e7ede8823e99d1a78f2c
LDFLAGS = -llapack -lblas

# Directory structure
SRCDIR = ../../src/
MODDIR = $(SRCDIR)/modules
BUILDDIR = .
LDReaderDIR = ~/wrk/LammpsDumpReader/fortran

# File name
TARGET = main
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR
MOD_SOURCES = $(wildcard $(MODDIR)/*.f90) $(LDReaderDIR)/lammpsIO.f90

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(MOD_SOURCES))

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/io.o: $(MODDIR)/io.f90 $(MODDIR)/string_utils.o
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/string_utils.o: $(MODDIR)/string_utils.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS)

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS)

test:
	echo $(TARGETOBJ)
