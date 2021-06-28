# if you don't specify a minumum version a warning will be printed
project(fft_tests)
cmake_minimum_required (VERSION 3.1)

enable_language(Fortran)

# fall back to Release build type unless specified
set(default_build_type "Release")
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to ${default_build_type} as none was specified.")
    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
endif()

# multithreaded version of MKL
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mkl=parallel -qopenmp")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fexternal-blas -fopenmp")
   # recommended libraries from 
   # https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor
   set(MKL_LIBRARIES mkl_gf_lp64 mkl_gnu_thread mkl_core gomp)
endif()

# create executables
file(GLOB srcs "*.f90")
foreach(src ${srcs})
	get_filename_component(fname ${src} NAME)
	string(REGEX REPLACE ".f90" "" exe ${fname})
	message(${exe})
	add_executable(${exe} ${src})
	target_link_libraries(${exe} ${MKL_LIBRARIES})
endforeach()

