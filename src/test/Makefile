# Compiler settings - can be customized.
FC = gfortran
FCFLAGS = -O2 -Wall
LDFLAGS =

# Directory structure
SRCDIR = ..
MODDIR = $(SRCDIR)/modules
TESTDIR = .
BUILDDIR = $(TESTDIR)
OUTPUT = a.out

# List of executables to be built within the package
PROGRAMS = $(BUILDDIR)/$(OUTPUT)

# "make" builds all
all: $(PROGRAMS)

$(BUILDDIR)/$(OUTPUT): $(MODDIR)/global_type.o $(TESTDIR)/test.o
	$(FC) $(LDFLAGS) -o $@ $^

$(MODDIR)/global_type.o: $(MODDIR)/global_type.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

$(TESTDIR)/test.o: $(TESTDIR)/test.f90 $(MODDIR)/global_type.mod
	$(FC) $(FCFLAGS) -c -o $@ $<

$(MODDIR)/global_type.mod: $(MODDIR)/global_type.f90
	$(FC) $(FCFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod $(TESTDIR)/*.o $(PROGRAMS)
clean-all:
	rm -f $(MODDIR)/*.o $(MODDIR)/*.mod $(TESTDIR)/*.o $(TESTDIR)/*.mod $(PROGRAMS)

