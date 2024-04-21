# Coarse-Grained Polymer Analyser
This repository provides Fortran modules and some executable binaries to analyze trajectory from molecular dynamics simulation of coarse-grained polymer (bead-spring model).

Currently, only LAMMPS format trajectory files (`.lammpstrj`) are supported.

## Examples
You can easily compute mean square displacement (MSD) and mean square radius of gyration (Rg2) in `examples` directory.

### compile 
```
cd path/to/CGPolyAna
cd examples/xxx
make clean
make
```
xxx = ( msd rg2 )
