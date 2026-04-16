# Set UFS specific MPAS Model build options
set(FETCH_MPAS_DATA OFF CACHE BOOL "Fetch mpas_data" FORCE)
set(MPAS_CHECKOUT_EXTERNALS OFF CACHE BOOL "Use checkout_externals tool to clone dependencies" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libraries" FORCE)
if(32BIT)
  set(MPAS_DOUBLE_PRECISION OFF CACHE BOOL "Use double precision 64-bit Floating point." FORCE)
else()
  set(MPAS_DOUBLE_PRECISION ON  CACHE BOOL "Use double precision 64-bit Floating point." FORCE)
endif()
set(MPAS_USE_PIO ON CACHE BOOL "Build with PIO I/O library" FORCE)

add_definitions(-DMPAS_USE_MPI_F08)

add_subdirectory(MPAS-Model)

target_sources(core_atmosphere PRIVATE MPAS-Model/src/external/esmf_time_f90/wrf_error_fatal.F90)

add_library(mpas ALIAS core_atmosphere)
