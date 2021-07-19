# fft_test
Fortran programs to compare matrix multiplication of sine/cosine matrices with FFT

## How to compile on mahuika

```
ml CMake gimkl NVHPC
mkdir build
cd build
cmake ..
make
```
To configure the cuf version, specify the pgfortran compiler of NVHPC:
```
FC=pgfortran cmake ..
```
