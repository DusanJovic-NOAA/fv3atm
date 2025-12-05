! #########################################################################################
!
! MPAS configuration information
!
! #########################################################################################
module module_mpasmodel_config
  use mpi_f08

  use mpas_derived_types, only : core_type, domain_type

  implicit none

  type (core_type), pointer :: corelist => null()
  type (domain_type), pointer :: domain => null()

  integer, pointer :: nCellsSolve      ! number of cells that a task solves
  integer, pointer :: nEdgesSolve      ! number of edges that a task solves
  integer, pointer :: nVerticesSolve   ! number of vertices (vorticity) that a task solves
  integer, pointer :: nVertLevels      ! number of vertical layers (midpoints)

  !> Global gridded data
  integer :: nCellsGlobal     ! global number of cells/columns
  integer :: nEdgesGlobal     ! global number of edges
  integer :: nVerticesGlobal  ! global number of vertices

  !> MPI communicator for the forecast grid component
  type(MPI_Comm)           :: fcst_mpi_comm

  !> Atmosphere time step in seconds
  integer                  :: dt_atmos

  !> Number of MPAS dycore calls per ATMosphere time step.
  integer                  :: n_atmos

  !> Total number of mpi tasks for the forecast grid components
  integer                  :: fcst_ntasks

  !> The first integration step
  integer                  :: first_kdt

  !> ID number for the coupled grids
  integer                  :: cpl_grid_id

  !> Flag to decide if model writes out coupled diagnostic fields
  logical                  :: cplprint_flag = .false.

  !> Flag to decide if write grid components is used
  logical                  :: quilting = .false.

  !> Flag to decide if write grid component writes out restart files
  logical                  :: quilting_restart = .false.

  !> Output frequency if this array has only two elements and the value of
  !! the second eletment is -1. Otherwise, it is the specific output forecast
  !! hours
  real,dimension(:),allocatable :: output_fh

  !> Calendar type
  character(17)            :: calendar='                 '

end module module_mpasmodel_config
