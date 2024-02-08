# Compiler settings - can be customized.
FC = gfortran
FCFLAGS = -O2 -Wall
LDFLAGS =

# Directory structure
SRCDIR = ../../src/
MODDIR = $(SRCDIR)/modules
BUILDDIR = .

# File name
TARGET = msd
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# List of source files in MODDIR
MOD_SOURCES = $(wildcard $(MODDIR)/*.f90)

# List of object files to be built from MOD_SOURCES
MOD_OBJECTS = $(patsubst $(MODDIR)/%.f90,$(MODDIR)/%.o,$(MOD_SOURCES))

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MOD_OBJECTS) $(TARGET).o
	$(FC) $(LDFLAGS) -o $@ $^

$(TARGET).o: $(TARGET).f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/%.o: $(MODDIR)/%.f90
	$(FC) $(FCFLAGS) -c $(MODDIR)/global_types.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o $(PROGRAMS)

clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod *.o *.mod $(PROGRAMS)

test:
	echo $(TARGETOBJ)
