# Set UFS specific MPAS Model build options
set(FETCH_MPAS_DATA OFF CACHE BOOL "Fetch mpas_data" FORCE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build shared libraries" FORCE)
if(32BIT)
  set(MPAS_DOUBLE_PRECISION OFF CACHE BOOL "Use double precision 64-bit Floating point." FORCE)
else()
  set(MPAS_DOUBLE_PRECISION ON  CACHE BOOL "Use double precision 64-bit Floating point." FORCE)
endif()
set(MPAS_USE_PIO ON CACHE BOOL "Build with PIO I/O library" FORCE)

add_definitions(-DMPAS_USE_MPI_F08)

add_subdirectory(MPAS-Model)

add_library(mpas ALIAS core_atmosphere)
