# DummyModel
A simple modern Fortran model used to demonstrate the 
[ObjectiveFunction](https://github.com/optclim/ObjectiveFunction) and 
[ModelOptimisation2](https://github.com/optclim/ModelOptimisation2) modules.

It can also be used as a template Fortran project.

The program computes a two dimension polynomial of 2nd order
```math
z(x,y) = ax^2 + bx^2 + cxy + dx + ey +f
```
## Installation
The Fortran code uses `cmake` to configure the build. To build
```bash
mkdir build
cd build
cmake ..
make
```
The binaray can found in `src/dummy`.

## Running the model
The program takes two arguments:
```bash
dummy config.nml output.nc
```
The configuration file contains Fortran namelists. See the example [config.nml](../blob/main/example/config.nml). The program produces a netCDF file as output.
