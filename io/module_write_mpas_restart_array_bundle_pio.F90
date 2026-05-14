#define ESMF_ERR(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

#define ESMF_ERR_RETURN(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1
module module_write_mpas_restart_array_bundle_pio

  use mpi_f08
  use esmf

#ifdef MPAS_SMIOL_SUPPORT
  use SMIOLf
#include "smiol_codes.inc"
#else
  use pio
#endif

  implicit none
  private
  public write_mpas_restart_array_bundle_pio

#ifdef MPAS_SMIOL_SUPPORT
  integer, parameter :: FILE_IO_int = SMIOL_INT32
  integer, parameter :: FILE_IO_real = SMIOL_REAL32
  integer, parameter :: FILE_IO_double =  SMIOL_REAL64
  integer, parameter :: FILE_IO_char =  SMIOL_CHAR
#else
  integer, parameter :: FILE_IO_int = PIO_int
  integer, parameter :: FILE_IO_real = PIO_real
  integer, parameter :: FILE_IO_double = PIO_double
  integer, parameter :: FILE_IO_char = PIO_char
#endif

  contains

!----------------------------------------------------------------------------------------
  subroutine write_mpas_restart_array_bundle_pio(wrtfb, filename, comm, mype, xtime, seconds_since_start, rc)

    type(ESMF_ArrayBundle), intent(in) :: wrtfb
    character(*), intent(in)           :: filename
    type(MPI_Comm), intent(in)         :: comm
    integer, intent(in)                :: mype
    character(len=*), intent(in)       :: xtime
    real, intent(in)                   :: seconds_since_start
    integer, optional,intent(out)      :: rc

!** local vars
    integer :: i,j,k,n
    integer :: nproc

    type(ESMF_Info) :: bundle_info

    integer(ESMF_KIND_I4), dimension(:), pointer       :: array_cellid, array_vertexid, array_edgeid, array_index

    integer(ESMF_KIND_I4), dimension(:), pointer       :: array_i4_1d
    integer(ESMF_KIND_I4), dimension(:,:), pointer     :: array_i4_2d
    integer(ESMF_KIND_I4), dimension(:,:,:), pointer   :: array_i4_3d

    real(ESMF_KIND_R4), dimension(:), pointer       :: array_r4_1d
    real(ESMF_KIND_R4), dimension(:,:), pointer     :: array_r4_2d
    real(ESMF_KIND_R4), dimension(:,:,:), pointer   :: array_r4_3d

    real(ESMF_KIND_R8), dimension(:), pointer       :: array_r8_1d
    real(ESMF_KIND_R8), dimension(:,:), pointer     :: array_r8_2d
    real(ESMF_KIND_R8), dimension(:,:,:), pointer   :: array_r8_3d

    integer :: arrayCount

    type(ESMF_Array)                     :: array_indexToCellID, array_indexToVertexID, array_indexToEdgeID
    type(ESMF_TypeKind_Flag)             :: typekind
    type(ESMF_Array)                     :: array
    character(len=ESMF_MAXSTR)           :: arrName

    integer :: ierr

    integer :: rank_esmf
    integer :: rank, deCount, localDeCount, dimCount, tileCount, tile_number
    integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
    integer, dimension(:), allocatable :: minIndexPDimPDe, maxIndexPDimPDe

    integer :: num_dims_esmf

    character(64), allocatable :: dimension_names(:)
    character(64), allocatable :: variable_names(:)
    integer :: var_count
    character(64), allocatable :: var_dim_names(:)
    integer :: var_dim_names_count
    character(256), allocatable :: global_att_names(:)

    integer :: dimSize, numAtt, itemCount

    integer(ESMF_KIND_I4) :: iVal
    integer(ESMF_KIND_I4), allocatable, target :: intVals(:)
    real(ESMF_KIND_R4) :: r4Val
    real(ESMF_KIND_R4), allocatable, target :: real4Vals(:)
    real(ESMF_KIND_R8) :: r8Val
    real(ESMF_KIND_R8), allocatable, target :: real8Vals(:)
    character(256) :: cVal
    character(len=64) :: textVal

    type :: dim_info_t
      character(64) :: dimName
      integer :: dimSize
      integer :: dimId
    end type
    type (dim_info_t), allocatable :: dim_info_arr(:)

    type :: var_info_t
      character(64) :: varName
      character(64),allocatable :: dimNames(:)
      integer, allocatable :: dimIDs(:)
      integer, allocatable :: dimSizes(:)
      integer, allocatable :: locArrayShape(:)
      integer :: rank
      integer :: file_io_type
      logical :: isESMFArray = .false.
#ifdef MPAS_SMIOL_SUPPORT
      type (SMIOLf_decomp), pointer :: iodesc
#else
      type(var_desc_t) :: varid
      type(io_desc_t)  :: iodesc
#endif
    end type
    type (var_info_t), allocatable :: var_info_arr(:)

    type :: decomp_info_t
      integer :: file_io_type
      integer, allocatable :: array_shape(:)
      integer, allocatable :: array_index(:)
#ifdef MPAS_SMIOL_SUPPORT
      type (SMIOLf_decomp), pointer :: iodesc
#else
      type (io_desc_t) :: iodesc
#endif
    end type decomp_info_t
    integer, parameter :: max_decomp_info_arr = 30
    type (decomp_info_t) :: decomp_info_arr(max_decomp_info_arr)
    integer :: num_decomp_info_array = 0

#ifdef MPAS_SMIOL_SUPPORT
    type(SMIOLf_context),pointer  :: smiol_context
    type(SMIOLf_file), pointer :: smiol_file
    integer(kind=SMIOL_offset_kind) :: frame=1
    type (SMIOLf_decomp), pointer :: null_decomp => NULL()
#else
    type(iosystem_desc_t) :: pioIoSystem(1)
    type(file_desc_t)     :: pioFileDesc
    type(var_desc_t)      :: pioVar
    integer(kind=pio_offset_kind) :: frame=1
#endif

    integer               :: retVal
    integer               :: stride
    integer               :: optBase
    integer               :: numAggregator
    integer               :: iotype
    integer               :: file_io_type
    logical               :: added_time_dim
    logical               :: isDecomposed
    logical               :: isPresent
    integer               :: i1, i2, i3, i4, i5
    character(len=64), allocatable :: var_att_names(:)
    character(len=512) :: key
    character(len=16) :: var_type

#ifdef MPAS_SMIOL_SUPPORT
    integer(ESMF_KIND_I4), target        :: iVal_target
    integer(ESMF_KIND_I4), pointer       :: iVal_ptr

    real(ESMF_KIND_R4), target        :: r4Val_target
    real(ESMF_KIND_R4), pointer       :: r4Val_ptr

    real(ESMF_KIND_R8), target        :: r8Val_target
    real(ESMF_KIND_R8), pointer       :: r8Val_ptr

    character(len=64), target   :: textVal_target
    character(len=:), pointer  :: textVal_ptr
#endif

#ifdef MPAS_SMIOL_SUPPORT
    iVal_ptr => iVal_target
    r4Val_ptr => r4Val_target
    r8Val_ptr => r8Val_target
    textVal_ptr => textVal_target
#endif

    num_decomp_info_array = 0

    call MPI_Comm_Size(comm, nproc, ierr)
    if (ierr /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#ifdef MPAS_SMIOL_SUPPORT
#else
    ierr = PIO_set_log_level(3)
    call PIO_setdebuglevel(0)
#endif

    stride        = 1
    numAggregator = 0
    optBase       = 1

#ifdef MPAS_SMIOL_SUPPORT
    retVal = SMIOLf_init(comm%mpi_val, nproc, stride, smiol_context)
    call errorHandle("Could not initialize SMIOL", retVal)
#else
    call PIO_init(mype,        & ! MPI rank
         comm%mpi_val,         & ! MPI communicator
         nproc,                & ! Number of iotasks (ntasks/stride)`
         numAggregator,        & ! number of aggregators to use
         stride,               & ! stride
         ! PIO_REARR_SUBSET,     & ! do not use any form of rearrangement
         PIO_REARR_BOX,        & ! do not use any form of rearrangement
         pioIoSystem(1),       & ! iosystem
         base=optBase)           ! base (optional argument)
#endif

    call ESMF_InfoGetFromHost(wrtfb, info=bundle_info, rc=rc); ESMF_ERR(rc)

    ! if (mype == 0) then
    !    call ESMF_InfoPrint(bundle_info, rc=rc); ESMF_ERR(rc)
    ! end if

    ! Gather information about dimensions
    call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/dimension_names', values=dimension_names, itemCount=itemCount, rc=rc); ESMF_ERR(rc)
    call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/dimensions', size=num_dims_esmf, rc=rc); ESMF_ERR(rc)
    allocate(dim_info_arr(num_dims_esmf))
    do i = 1, num_dims_esmf
       call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/dimensions/'//trim(dimension_names(i)), value=dimSize, rc=rc); ESMF_ERR(rc)
       dim_info_arr(i) % dimName = trim(dimension_names(i))
       dim_info_arr(i) % dimSize = dimSIze
    end do

    ! Gather information about variables
    call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/variable_names', values=variable_names, itemCount=var_count, rc=rc); ESMF_ERR(rc)

    call ESMF_ArrayBundleGet(wrtfb, arrayName='indexToCellID', array=array_indexToCellID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_ArrayGet(array_indexToCellID, localDe=0, farrayPtr=array_cellid, rc=rc); ESMF_ERR_RETURN(rc)

    call ESMF_ArrayBundleGet(wrtfb, arrayName='indexToVertexID', array=array_indexToVertexID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_ArrayGet(array_indexToVertexID, localDe=0, farrayPtr=array_vertexid, rc=rc); ESMF_ERR_RETURN(rc)

    call ESMF_ArrayBundleGet(wrtfb, arrayName='indexToEdgeID', array=array_indexToEdgeID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_ArrayGet(array_indexToEdgeID, localDe=0, farrayPtr=array_edgeid, rc=rc); ESMF_ERR_RETURN(rc)

    ! Gather information about variables
    allocate(var_info_arr(var_count))
    do i = 1, var_count

       arrName = trim(variable_names(i))

       call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/variables/'//trim(arrName), values=var_dim_names, itemCount=var_dim_names_count, rc=rc); ESMF_ERR(rc)

       ! Based on whether var_dim_names have one of 'distributed' dimension we decide if we are going to get an esmf array, of use /MPAS/${varName} info
       isDecomposed = .false.
       do j = 1, var_dim_names_count
           isDecomposed = isDecomposed .or. is_decomposed_dim(trim(var_dim_names(j)))
       end do

       if (isDecomposed) then
           ! Get an ESMF Array
           call ESMF_ArrayBundleGet(wrtfb, arrName, array=array, arrayCount=arrayCount, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
           ASSERT (isPresent)
           ASSERT (arrayCount == 1)
           call ESMF_ArrayGet(array, rank=rank, typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

           rank_esmf = rank

           added_time_dim = .false.
           if (rank /= var_dim_names_count) then
               if (trim(var_dim_names(var_dim_names_count)) == 'Time') then ! it's ok extend rank by 1 and add 'Time' dimension
                  rank = rank + 1
                  added_time_dim = .true.
               else
                  write(0,*)'var=',trim(arrName), ' rank /= itemCount ', rank, itemcount
                  write(0,*)'var_dim_names ', var_dim_names
                  stop 1
               end if
            end if
            var_info_arr(i) % varName = trim(arrName)
            var_info_arr(i) % rank = rank
            var_info_arr(i) % isESMFArray = .true.
            allocate(var_info_arr(i) % dimNames(rank))
            allocate(var_info_arr(i) % dimIDs(rank))
            allocate(var_info_arr(i) % dimSizes(rank))
            var_info_arr(i) % dimNames = var_dim_names
            if (added_time_dim) var_info_arr(i) % dimNames(rank) = 'Time'

            do n = 1, var_info_arr(i) % rank
               call get_dimid_for_dimname(var_info_arr(i) % dimNames(n), var_info_arr(i) % dimIDs(n), var_info_arr(i) % dimSizes(n))
            end do

            if (typekind == ESMF_TYPEKIND_R4) then
              var_info_arr(i) % file_io_type = FILE_IO_real
            else if (typekind == ESMF_TYPEKIND_R8) then
              var_info_arr(i) % file_io_type = FILE_IO_double
            else if (typekind == ESMF_TYPEKIND_I4) then
              var_info_arr(i) % file_io_type = FILE_IO_int
            else
              if (mype == 0) write(0,*)'1) Unsupported typekind ', typekind
              call ESMF_Finalize(endflag=ESMF_END_ABORT)
            end if

            if (rank_esmf == 1) then

              if (var_info_arr(i) % dimNames(1) == 'nCells') then
                  array_index => array_cellid
              else if (var_info_arr(i) % dimNames(1) == 'nVertices') then
                  array_index => array_vertexid
              else if (var_info_arr(i) % dimNames(1) == 'nEdges') then
                  array_index => array_edgeid
              else
                  if (mype == 0) write(0,*)'1) Unsupported array_index ', var_info_arr(i) % dimNames(1)
                  call ESMF_Finalize(endflag=ESMF_END_ABORT)
              end if

              if (typekind == ESMF_TYPEKIND_R4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_1d, rc=rc); ESMF_ERR_RETURN(rc)
                 call set_iodesc(shape(array_r4_1d), var_info_arr(i))
                 nullify (array_r4_1d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_1d, rc=rc); ESMF_ERR_RETURN(rc)
                 call set_iodesc(shape(array_r8_1d), var_info_arr(i))
                 nullify (array_r8_1d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_1d, rc=rc); ESMF_ERR_RETURN(rc)
                 call set_iodesc(shape(array_i4_1d), var_info_arr(i))
                 nullify (array_i4_1d)

              else
                 if (mype == 0) write(0,*)'2) Unsupported typekind ', typekind
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
              end if

            else if (rank_esmf == 2) then

               if (var_info_arr(i) % dimNames(2) == 'nCells') then
                   array_index => array_cellid
               else if (var_info_arr(i) % dimNames(2) == 'nVertices') then
                   array_index => array_vertexid
               else if (var_info_arr(i) % dimNames(2) == 'nEdges') then
                   array_index => array_edgeid
               else
                   if (mype == 0) write(0,*)'1) Unsupported array_index ', var_info_arr(i) % dimNames(2)
                   call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if

              if (typekind == ESMF_TYPEKIND_R4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r4_2d), var_info_arr(i))
                 nullify (array_r4_2d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r8_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r8_2d), var_info_arr(i))
                 nullify (array_r8_2d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_i4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_i4_2d), var_info_arr(i))
                 nullify (array_i4_2d)

              else
                 if (mype == 0) write(0,*)'2) Unsupported typekind ', typekind
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
              end if

            else if (rank_esmf == 3) then

               if (var_info_arr(i) % dimNames(3) == 'nCells') then
                   array_index => array_cellid
               else if (var_info_arr(i) % dimNames(3) == 'nVertices') then
                   array_index => array_vertexid
               else if (var_info_arr(i) % dimNames(3) == 'nEdges') then
                   array_index => array_edgeid
               else
                   if (mype == 0) write(0,*)'1) Unsupported array_index ', var_info_arr(i) % dimNames(3)
                   call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if

              if (typekind == ESMF_TYPEKIND_R4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r4_3d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r4_3d), var_info_arr(i))
                 nullify (array_r4_3d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r8_3d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r8_3d), var_info_arr(i))
                 nullify (array_r8_3d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_i4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 ASSERT (size(array_i4_2d, dim=2) == var_info_arr(i) % dimSizes(2))
                 call set_iodesc(shape(array_i4_3d), var_info_arr(i))
                 nullify (array_i4_3d)
              else
                 if (mype == 0) write(0,*)'2) Unsupported typekind ', typekind
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
              end if
          else
                 if (mype == 0) write(0,*)'Unsupported rank_esmf ', rank_esmf
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
          end if ! rank_esmf
       else ! non decomposed
           ! Variable not stored in an ESMF Array but in Info
           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(arrName)//'_rank', value=rank, rc=rc); ESMF_ERR(rc)
           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(arrName)//'_type', value=var_type, rc=rc); ESMF_ERR(rc)

           if (trim(var_type) == 'int') then
              var_info_arr(i) % file_io_type = FILE_IO_int
           else if (trim(var_type) == 'real') then
              var_info_arr(i) % file_io_type = FILE_IO_real
           else if (trim(var_type) == 'double') then
              var_info_arr(i) % file_io_type = FILE_IO_double
           else if (trim(var_type) == 'char') then
              var_info_arr(i) % file_io_type = FILE_IO_char
           else
              if (mype == 0) write(0,*)'Unsupported var_type ', trim(var_type)
              call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if

           if (rank == 0 .and. var_info_arr(i) % file_io_type == FILE_IO_char) then
              rank = rank + 1
           endif
           ! FIXME pass hasTimeDimension and use it here
           if (rank == 0 .and. trim(arrName) == 'Time') then
              rank = rank + 1
           endif

           added_time_dim = .false.
           if (rank > 0 .and. rank /= var_dim_names_count) then
               if (trim(var_dim_names(var_dim_names_count)) == 'Time') then ! it's ok extend rank by 1 and add 'Time' dimension
                  rank = rank + 1
                  added_time_dim = .true.
               else
                  write(0,*)'var=',trim(arrName), ' rank /= itemCount ', rank, itemcount
                  write(0,*)'var_dim_names ', var_dim_names
                  stop 1
               end if
            end if

            var_info_arr(i) % varName = trim(arrName)
            var_info_arr(i) % rank = rank
            var_info_arr(i) % isESMFArray = .false.
            allocate(var_info_arr(i) % dimNames(rank))
            allocate(var_info_arr(i) % dimIDs(rank))
            allocate(var_info_arr(i) % dimSizes(rank))
            var_info_arr(i) % dimNames = var_dim_names
            if (added_time_dim) var_info_arr(i) % dimNames(rank) = 'Time'

            do n = 1, var_info_arr(i) % rank
               call get_dimid_for_dimname(var_info_arr(i) % dimNames(n), var_info_arr(i) % dimIDs(n), var_info_arr(i) % dimSizes(n))
            end do

       end if ! decomposed vs. non decomposed

       deallocate(var_dim_names)

    end do ! i = 1, var_count

#ifdef MPAS_SMIOL_SUPPORT
    retVal = SMIOLf_open_file(smiol_context, trim(fileName), SMIOL_FILE_CREATE, smiol_file)
#else
    ! iotype        = PIO_iotype_netcdf
    iotype        = PIO_iotype_pnetcdf
    ! iotype        = PIO_iotype_netcdf4c
    retVal = PIO_createfile(pioIoSystem(1), pioFileDesc, iotype, trim(fileName), PIO_64BIT_DATA)
#endif
    call errorHandle("Could not create "//trim(fileName), retVal)

    ! Define dimensions
    do i = 1, size(dim_info_arr)
       if (trim(dim_info_arr(i) % dimName) == 'Time') then
#ifdef MPAS_SMIOL_SUPPORT
           retVal = SMIOLf_define_dim(smiol_file, trim(dim_info_arr(i) % dimName), -1_SMIOL_offset_kind)
#else
           retVal = PIO_def_dim(pioFileDesc, trim(dim_info_arr(i) % dimName), PIO_UNLIMITED, dim_info_arr(i) % dimId)
#endif
           call errorHandle("Could not define dimension", retVal)
       else
#ifdef MPAS_SMIOL_SUPPORT
           retVal = SMIOLf_define_dim(smiol_file, trim(dim_info_arr(i) % dimName),  int(dim_info_arr(i) % dimSize,kind=SMIOL_offset_kind))
#else
           retVal = PIO_def_dim(pioFileDesc, trim(dim_info_arr(i) % dimName), dim_info_arr(i) % dimSize, dim_info_arr(i) % dimId)
#endif
           call errorHandle("Could not define dimension", retVal)
       end if
    end do

   ! Define variables
   do i = 1, var_count
      if (ESMF_InfoIsPresent(bundle_info, key='/NetCDF/FV3/variables/'//trim(variable_names(i))//':'//'do_not_write')) then
         cycle
      end if
      do n = 1, var_info_arr(i) % rank
         call get_dimid_for_dimname(var_info_arr(i) % dimNames(n), var_info_arr(i) % dimIDs(n), var_info_arr(i) % dimSizes(n))
      end do

#ifdef MPAS_SMIOL_SUPPORT
      retVal = SMIOLf_define_var(smiol_file, var_info_arr(i) % varName, var_info_arr(i) % file_io_type, size(var_info_arr(i) % dimNames), var_info_arr(i) % dimNames)
#else
      retVal = PIO_def_var(pioFileDesc, var_info_arr(i) % varName, var_info_arr(i) % file_io_type, var_info_arr(i) % dimids, var_info_arr(i) % varid)
#endif
      call errorHandle("Could not create "//trim(fileName), retVal)

      ! Define variable attributes
      call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//'_att_names', values=var_att_names, itemCount=itemCount, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//'_att_names', size=numAtt, rc=rc); ESMF_ERR(rc)
      do k = 1, numAtt
         key = '/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//':'//trim(var_att_names(k))
         typekind = ESMF_InfoGetTK(bundle_info, key=trim(key), rc=rc); ESMF_ERR(rc)
         if (typekind == ESMF_TYPEKIND_I4) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=iVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
            retVal = SMIOLf_define_att(smiol_file, trim(var_info_arr(i) % varName), trim(var_att_names(k)), ival)
#else
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), iVal)
#endif
         else if (typekind == ESMF_TYPEKIND_R4) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=r4Val, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
            retVal = SMIOLf_define_att(smiol_file, trim(var_info_arr(i) % varName), trim(var_att_names(k)), r4Val)
#else
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), r4Val)
#endif
         else if (typekind == ESMF_TYPEKIND_CHARACTER) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=cVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
            retVal = SMIOLf_define_att(smiol_file, trim(var_info_arr(i) % varName), trim(var_att_names(k)), trim(cVal))
#else
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), trim(cVal))
#endif
         else
            write(0,*)'Unsupported variable attribute typekind ', typekind, trim(var_info_arr(i) % varName), ' ', trim(var_att_names(k))
            stop
         end if
      end do

   end do

   ! Define global attributes
   call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/global_att_names', values=global_att_names, itemCount=itemCount, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att', size=numAtt, rc=rc); ESMF_ERR(rc)
   do i = 1, numAtt
      typekind = ESMF_InfoGetTK(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), rc=rc); ESMF_ERR(rc)
      if (typekind == ESMF_TYPEKIND_I4) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=iVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
         retVal = SMIOLf_define_att(smiol_file, '', trim(global_att_names(i)), ival)
#else
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), iVal)
#endif
      else if (typekind == ESMF_TYPEKIND_R4) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=r4Val, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
         retVal = SMIOLf_define_att(smiol_file, '', trim(global_att_names(i)), r4Val)
#else
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), r4Val)
#endif
      else if (typekind == ESMF_TYPEKIND_R8) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=r8Val, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
         retVal = SMIOLf_define_att(smiol_file, '', trim(global_att_names(i)), r8Val)
#else
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), r8Val)
#endif
      else if (typekind == ESMF_TYPEKIND_CHARACTER) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=cVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
         retVal = SMIOLf_define_att(smiol_file, '', trim(global_att_names(i)), cVal)
#else
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), cVal)
#endif
      else
         write(0,*)'Unsupported typekind ', typekind
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
   end do

#ifdef MPAS_SMIOL_SUPPORT
   retVal = SMIOLf_define_att(smiol_file, '', 'file_id', 'UFSATM Write Component - SMIOL')
#else
   retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, 'file_id', 'UFSATM Write Component - PIO')
#endif
   call errorHandle("Could not end define mode", retVal)

#ifdef MPAS_SMIOL_SUPPORT
#else
   retVal = PIO_enddef(pioFileDesc)
#endif
   call errorHandle("Could not end define mode", retVal)


   ! Write variables
   do i = 1, var_count
      if (ESMF_InfoIsPresent(bundle_info, key='/NetCDF/FV3/variables/'//trim(variable_names(i))//':'//'do_not_write')) then
         cycle
      end if
#ifdef MPAS_SMIOL_SUPPORT
       retVal = SMIOLf_set_frame(smiol_file, int(frame-1, kind=SMIOL_offset_kind))
#else
       call PIO_setframe(pioFileDesc, var_info_arr(i) % varid, frame)
#endif
       if (var_info_arr(i) % isESMFArray) then

           call ESMF_ArrayBundleGet(wrtfb, trim(var_info_arr(i) % varName), array=array, arrayCount=arrayCount, isPresent=isPresent, rc=rc); ESMF_ERR(rc)

           call ESMF_ArrayGet(array, name=arrName, rank=rank, typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

           if (trim(arrName) /= trim(var_info_arr(i) % varName)) then
              write(0,*)'trim(arrName) /= trim(var_info_arr(i) % varName) ', trim(arrName), ' ', trim(var_info_arr(i) % varName)
              call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if

           if (rank == 1) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_1d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r4_1d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_1d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_1d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r8_1d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_1d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_1d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_i4_1d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_1d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'2) Unsupported typekind ', typekind
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if
           else if (rank == 2) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_2d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r4_2d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_2d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_2d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r8_2d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_2d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_2d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_i4_2d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_2d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'2) Unsupported typekind ', typekind
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if

           else if (rank == 3) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r4_3d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r4_3d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_3d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_r8_3d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_r8_3d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_3d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_ArrayGet(array, localDe=0, farrayPtr=array_i4_3d, rc=rc); ESMF_ERR_RETURN(rc)
#ifdef MPAS_SMIOL_SUPPORT
                retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varname), var_info_arr(i) % iodesc, array_i4_3d)
#else
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_3d, retVal)
#endif
                call errorHandle("Could not write "//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'3) Unsupported typekind ', typekind, trim(arrName)
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if
           else
             if (mype == 0) write(0,*)'Unsupported rank ', rank
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if ! end rank

       else ! non ESMF Array

           ! Get data from MPAS Info
           rank = var_info_arr(i) % rank
           file_io_type = var_info_arr(i) % file_io_type

           if (rank == 0) then
               if (file_io_type == FILE_IO_real) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r4Val, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
                   r4Val_ptr = r4Val
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, r4Val_ptr)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, r4Val)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               else if (file_io_type == FILE_IO_double) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r8Val, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
                   r8Val_ptr = r8Val
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, r8Val_ptr)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, r8Val)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               else if (file_io_type == FILE_IO_int) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=iVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
                   iVal_ptr = iVal
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, iVal_ptr)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, iVal)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               else
                   if (mype == 0) write(0,*)'3) Unsupported file_io_type ', file_io_type, trim(var_info_arr(i) % varName), rank
                   call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if
           else if (rank == 1) then
               if (file_io_type == FILE_IO_real) then
                   if (trim(var_info_arr(i) % varName) == 'Time') then
                       r4Val = seconds_since_start
#ifdef MPAS_SMIOL_SUPPORT
                       r4Val_ptr = r4Val
                       retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, r4Val_ptr)
#else
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r4Val)
#endif
                       call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
                   else
                       call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=real4Vals, rc=rc)
#ifdef MPAS_SMIOL_SUPPORT
                       array_r4_1d => real4Vals
                       retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, array_r4_1d)
#else
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, real4Vals)
#endif
                       call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
                   end if
               else if (file_io_type == FILE_IO_double) then
                   if (trim(var_info_arr(i) % varName) == 'Time') then
                       r8Val = seconds_since_start
#ifdef MPAS_SMIOL_SUPPORT
                       r8Val_ptr = r8Val
                       retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, r8Val_ptr)
#else
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r8Val)
#endif
                       call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
                   else
                       call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=real8Vals, rc=rc)
#ifdef MPAS_SMIOL_SUPPORT
                       array_r8_1d => real8Vals
                       retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, array_r8_1d)
#else
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, real8Vals)
#endif
                       call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
                   end if
               else if (file_io_type == FILE_IO_int) then
                   call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=intVals, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
                   array_i4_1d => intVals
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, array_i4_1d)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, intVals)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               else if (file_io_type == FILE_IO_char) then
                   textVal = ' '
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=textVal, rc=rc); ESMF_ERR(rc)
#ifdef MPAS_SMIOL_SUPPORT
                   textVal_ptr = textVal
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, textVal_ptr)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], textVal)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               else
                   if (mype == 0) write(0,*)'3) Unsupported file_io_type ', file_io_type, trim(var_info_arr(i) % varName), rank
                   call ESMF_Finalize(endflag=ESMF_END_ABORT)
               end if
           else if (rank == 2) then
               if (file_io_type == FILE_IO_char) then
                   textVal = ' '
                   if (trim(var_info_arr(i) % varName) == 'xtime') then
                       textVal = xtime
                   else
                       call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=textVal, rc=rc); ESMF_ERR(rc)
                   end if
#ifdef MPAS_SMIOL_SUPPORT
                   textVal_ptr = textVal
                   retVal = SMIOLf_put_var(smiol_file, trim(var_info_arr(i) % varName), null_decomp, textVal_ptr)
#else
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1,1], textVal)
#endif
                   call errorHandle("Could not write "//trim(var_info_arr(i) % varName), retVal)
               end if
           else
             if (mype == 0) write(0,*)'Unsupported rank ', rank
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if

       end if ! isESMFArray
    end do

#ifdef MPAS_SMIOL_SUPPORT
    retVal = SMIOLf_sync_file(smiol_file)
    call errorHandle("Could not sync the file", retVal)

    retVal = SMIOLf_close_file(smiol_file)
    call errorHandle("Could not close the file", retVal)


    do n=1,num_decomp_info_array
       retVal = SMIOLf_free_decomp(decomp_info_arr(n) % iodesc)
    end do

    retVal = SMIOLf_finalize(smiol_context)
    call errorHandle("Could not finalize the SMIOL system", retVal)
#else
    call PIO_syncfile(pioFileDesc)

    call PIO_closefile(pioFileDesc)

    do n=1,num_decomp_info_array
       call PIO_freedecomp(pioIoSystem(1), decomp_info_arr(n) % iodesc)
    end do

    call PIO_finalize(pioIoSystem(1), ierr)
#endif

  contains

    subroutine get_dimid_for_dimname(dimName, dimID, dimSize)

       character(len=*), intent(in) :: dimName
       integer, intent(out) :: dimID
       integer, intent(out) :: dimSize

       integer :: n

       do n = 1, size(dim_info_arr)
           if (trim(dim_info_arr(n) % dimName) == trim(dimName)) then
               dimID = dim_info_arr(n) % dimID
               dimSize = dim_info_arr(n) % dimSize
               return
           end if
       end do

       dimID = -1
       write(0,*)'unknown dim ', trim(dimName)
       stop 1

    end subroutine get_dimid_for_dimname

    subroutine set_iodesc(array_shape, var_info)

       integer, intent(in) :: array_shape(:)
       type (var_info_t), intent(inout) :: var_info

       integer, allocatable  :: compdof(:)
       integer :: i1, i2, i3, i4, i5
       integer :: n, ndim, ncompdof
       integer :: k
#ifdef MPAS_SMIOL_SUPPORT
       integer(kind=SMIOL_offset_kind), dimension(:), pointer :: smiol_indices
       integer(kind=SMIOL_offset_kind) :: smiol_n_compute_elements
#endif

       do n = 1, num_decomp_info_array
           if (decomp_info_arr(n) % file_io_type == var_info % file_io_type) then
           if (size(decomp_info_arr(n) % array_shape) == size(array_shape)) then
           if (ALL(decomp_info_arr(n) % array_shape == array_shape)) then
           if (size(decomp_info_arr(n) % array_index) == size(array_index)) then
           if (ALL(decomp_info_arr(n) % array_index == array_index)) then
               ! Found iodesc in the cache, use it.
#ifdef MPAS_SMIOL_SUPPORT
               var_info % iodesc => decomp_info_arr(n) % iodesc
#else
               var_info % iodesc = decomp_info_arr(n) % iodesc
#endif
               return
           end if
           end if
           end if
           end if
           end if
       end do

       ! Need new iodesc
       num_decomp_info_array = num_decomp_info_array + 1
       if (num_decomp_info_array > max_decomp_info_arr) then
          write(0,*)'ERROR: num_decomp_info_array > max_decomp_info_arr'
          stop 9
       end if

       decomp_info_arr(num_decomp_info_array) % file_io_type = var_info % file_io_type
       decomp_info_arr(num_decomp_info_array) % array_shape = array_shape
       decomp_info_arr(num_decomp_info_array) % array_index = array_index

       ndim = size(array_shape)
       ncompdof = 1
       do n = 1, ndim
         ncompdof = ncompdof * array_shape(n)
       end do

       allocate (compdof(ncompdof))

       if (ndim == 1) then
          k = 0
          do i1 = 1, array_shape(1)
               k = k + 1
               compdof(k) = array_index(i1)
          end do
       else if (ndim == 2) then
          k = 0
          do i2 = 1, array_shape(2)
          do i1 = 1, array_shape(1)
               k = k + 1
               compdof(k) = i1 + (array_index(i2)-1)*(var_info % dimSizes(1))
          end do
          end do
       else if (ndim == 3) then
          k = 0
          do i3 = 1, array_shape(3)
          do i2 = 1, array_shape(2)
          do i1 = 1, array_shape(1)
               k = k + 1
               compdof(k) = i1 + (i2-1)*(var_info % dimSizes(1)) + (array_index(i3)-1)*(var_info % dimSizes(1)*var_info % dimSizes(2))
          end do
          end do
          end do
       else
          write(0,*)'Unsuported rank ', ndim
       end if

       ! create new iodesc, and save it in decomp_info_arr
#if MPAS_SMIOL_SUPPORT
       allocate(smiol_indices(size(array_index)))
       smiol_indices(:) = int(array_index(:), kind=SMIOL_offset_kind) - 1_SMIOL_offset_kind   ! SMIOL indices are 0-based
       smiol_n_compute_elements = size(array_index,kind=SMIOL_offset_kind)
       ierr = SMIOLf_create_decomp(smiol_context, smiol_n_compute_elements, smiol_indices, &
                                        decomp_info_arr(num_decomp_info_array) % iodesc)
       deallocate(smiol_indices)
       var_info % iodesc => decomp_info_arr(num_decomp_info_array) % iodesc
#else
       call PIO_initdecomp(pioIoSystem(1), var_info % file_io_type, var_info % dimSizes, compdof, decomp_info_arr(num_decomp_info_array) % iodesc)

       var_info % iodesc = decomp_info_arr(num_decomp_info_array) % iodesc
#endif
       deallocate (compdof)

    end subroutine set_iodesc

    subroutine errorHandle(errMsg, retVal)
        implicit none
        character(len=*),       intent(in)    :: errMsg
        integer,                intent(in)    :: retVal
        integer :: lretval

#if MPAS_SMIOL_SUPPORT
        if (retVal /= SMIOL_SUCCESS) then
           write(*,*) errMsg
           if (retVal == SMIOL_LIBRARY_ERROR) then
              write(0,*)trim(SMIOLf_lib_error_string(smiol_context))
           else
              write(0,*)trim(SMIOLf_error_string(retVal))
           end if
           lretval = SMIOLf_close_file(smiol_file)
           stop 1
        end if
#else
        if (retVal /= PIO_NOERR) then
           write(*,*) retVal,errMsg
           call PIO_closefile(pioFileDesc)
           stop 1
       end if
#endif
    end subroutine errorHandle

  end subroutine write_mpas_restart_array_bundle_pio

  logical function is_decomposed_dim(dimName)

      implicit none

      character(len=*), intent(in) :: dimName

      if (trim(dimName) == 'nCells' .or. &
          trim(dimName) == 'nEdges' .or. &
          trim(dimName) == 'nVertices') then

          is_decomposed_dim = .true.

      else

          is_decomposed_dim = .false.

      end if

  end function is_decomposed_dim

!----------------------------------------------------------------------------------------
end module module_write_mpas_restart_array_bundle_pio
