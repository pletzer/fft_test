# if you don't specify a minumum version a warning will be printed
project(fft_tests LANGUAGES Fortran CUDA)
cmake_minimum_required (VERSION 3.18)

# fall back to Release build type unless specified
set(default_build_type "Release")
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to ${default_build_type} as none was specified.")
    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)
endif()

find_package(OpenMP REQUIRED)

# multithreaded version of MKL
if (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mkl=parallel")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fexternal-blas")
   # recommended libraries from
   # https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor
   set(MKL_LIBRARIES mkl_gf_lp64 mkl_gnu_thread mkl_core gomp)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
#   set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fexternal-blas -fopenmp")
   # recommended libraries from
   # https://software.intel.com/en-us/articles/intel-mkl-link-line-advisor
   set(MKL_LIBRARIES mkl_rt)
endif()

# create executables
file(GLOB srcs "${CMAKE_CURRENT_SOURCE_DIR}/*.f90")
foreach(src ${srcs})
  message(STATUS "src file: ${src}")
	get_filename_component(fname ${src} NAME)
	string(REGEX REPLACE ".f90" "" exe ${fname})
	message(STATUS "${exe}")
	add_executable(${exe} $ENV{MKLROOT}/include/mkl_dfti.f90 ${src})
	target_link_libraries(${exe} ${MKL_LIBRARIES})
    target_link_libraries(${exe} OpenMP::OpenMP_Fortran)
endforeach()

# require NVIDIA HPC SDK compilers for CUDA Fortran
if (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    find_package(CUDAToolkit)
    file(GLOB srcs "${CMAKE_CURRENT_SOURCE_DIR}/*.cuf")
    foreach(src ${srcs})
      message(STATUS "src file: ${src}")
        get_filename_component(fname ${src} NAME)
        string(REGEX REPLACE ".cuf$" "" exe ${fname})
        message(STATUS "${exe}")
        add_executable(${exe} ${src})
        target_link_libraries(${exe} CUDA::cufft CUDA::cublas)
        set_target_properties(${exe} PROPERTIES LINKER_LANGUAGE Fortran)
        set_source_files_properties(${src} PROPERTIES LANGUAGE Fortran)
        target_link_options(${exe} PUBLIC -Mcuda)
    endforeach()
endif()
