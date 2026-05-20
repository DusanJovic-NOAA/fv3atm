#define ESMF_ERR(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

#define ESMF_ERR_RETURN(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1
module module_write_mpas_restart_field_bundle_pio

  use mpi_f08
  use esmf
  use pio

  implicit none
  private
  public write_mpas_restart_field_bundle_pio

  contains

!----------------------------------------------------------------------------------------
  subroutine write_mpas_restart_field_bundle_pio(wrtfb, filename, comm, mype, xtime, seconds_since_start, rc)

    type(ESMF_FieldBundle), intent(in) :: wrtfb
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

    integer :: fieldCount

    type(ESMF_Field)                     :: field_indexToCellID, field_indexToVertexID ! , field_indexToEdgeID
    type(ESMF_TypeKind_Flag)             :: typekind
    type(ESMF_Field)                     :: field
    character(len=ESMF_MAXSTR)           :: arrName

    integer :: ierr

    integer :: rank_esmf
    integer :: rank

    integer :: num_dims_esmf

    character(64), allocatable :: dimension_names(:)
    character(64), allocatable :: variable_names(:)
    integer :: var_count
    character(64), allocatable :: var_dim_names(:)
    integer :: var_dim_names_count
    character(256), allocatable :: global_att_names(:)

    integer :: dimSize, numAtt, itemCount

    integer(ESMF_KIND_I4) :: iVal
    integer(ESMF_KIND_I4), allocatable :: intVals(:)
    real(ESMF_KIND_R4) :: r4Val
    real(ESMF_KIND_R4), allocatable :: real4Vals(:)
    real(ESMF_KIND_R8) :: r8Val
    real(ESMF_KIND_R8), allocatable :: real8Vals(:)
    character(256) :: cVal
    character(len=64) :: textVal

    type :: dim_info_t
      character(64) :: dimName
      integer :: dimSize
      integer :: dimId
    end type dim_info_t
    type(dim_info_t), allocatable :: dim_info_arr(:)

    type :: var_info_t
      character(64) :: varName
      character(64),allocatable :: dimNames(:)
      integer, allocatable :: dimIDs(:)
      integer, allocatable :: dimSizes(:)
      integer, allocatable :: locArrayShape(:)
      integer :: rank
      integer :: pio_type
      logical :: isESMFArray
      type(var_desc_t) :: varid
      type(io_desc_t)  :: iodesc
    end type var_info_t
    type(var_info_t), allocatable :: var_info_arr(:)

    type :: decomp_info_t
      integer :: pio_type
      integer, allocatable :: array_shape(:)
      integer, allocatable :: array_index(:)
      type(io_desc_t) :: iodesc
    end type decomp_info_t
    integer, parameter :: max_decomp_info_arr = 30
    type(decomp_info_t) :: decomp_info_arr(max_decomp_info_arr)
    integer :: num_decomp_info_array

    integer               :: retVal
    integer               :: stride
    integer               :: optBase
    integer               :: numAggregator
    type(iosystem_desc_t) :: pioIoSystem(1)
    type(file_desc_t)     :: pioFileDesc
    integer               :: iotype
    integer               :: pio_type
    logical :: added_time_dim
    logical :: isDecomposed
    logical :: isPresent
    character(len=64), allocatable :: var_att_names(:)
    character(len=512) :: key
    integer(kind=pio_offset_kind) :: frame

    num_decomp_info_array = 0

    frame = 1

    call MPI_Comm_Size(comm, nproc, ierr)
    if (ierr /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    ierr = PIO_set_log_level(3)
    call PIO_setdebuglevel(0)

    stride        = 1
    numAggregator = 0
    optBase       = 1
    ! iotype        = PIO_iotype_netcdf
    iotype        = PIO_iotype_pnetcdf
    ! iotype        = PIO_iotype_netcdf4c

    call PIO_init(mype,        & ! MPI rank
         comm%mpi_val,         & ! MPI communicator
         nproc,                & ! Number of iotasks (ntasks/stride)`
         numAggregator,        & ! number of aggregators to use
         stride,               & ! stride
         ! PIO_REARR_SUBSET,     & ! do not use any form of rearrangement
         PIO_REARR_BOX,        & ! do not use any form of rearrangement
         pioIoSystem(1),       & ! iosystem
         base=optBase)           ! base (optional argument)

    call ESMF_InfoGetFromHost(wrtfb, info=bundle_info, rc=rc); ESMF_ERR(rc)

    ! call ESMF_InfoPrint(bundle_info, rc=rc); ESMF_ERR(rc)

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

    call ESMF_FieldBundleGet(wrtfb, fieldName='indexToCellID', field=field_indexToCellID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_FieldGet(field_indexToCellID, localDe=0, farrayPtr=array_cellid, rc=rc); ESMF_ERR_RETURN(rc)

    call ESMF_FieldBundleGet(wrtfb, fieldName='indexToVertexID', field=field_indexToVertexID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_FieldGet(field_indexToVertexID, localDe=0, farrayPtr=array_vertexid, rc=rc); ESMF_ERR_RETURN(rc)

#if 0
   ! FIXME disable 'edge' variable for now
    call ESMF_FieldBundleGet(wrtfb, fieldName='indexToEdgeID', field=field_indexToEdgeID, rc=rc); ESMF_ERR_RETURN(rc)
    call ESMF_FieldGet(field_indexToEdgeID, localDe=0, farrayPtr=array_edgeid, rc=rc); ESMF_ERR_RETURN(rc)
#endif
    ! call PIO_initdecomp(pioIoSystem(1), PIO_int, [nEdges], array_edgeid, iodescEdges)

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
           call ESMF_FieldBundleGet(wrtfb, arrName, field=field, fieldCount=fieldCount, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
           ASSERT (isPresent)
           ASSERT (fieldCount == 1)
           call ESMF_FieldGet(field, name=arrName, rank=rank, typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

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
              var_info_arr(i) % pio_type = PIO_real
            else if (typekind == ESMF_TYPEKIND_R8) then
              var_info_arr(i) % pio_type = PIO_double
            else if (typekind == ESMF_TYPEKIND_I4) then
              var_info_arr(i) % pio_type = PIO_int
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

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_1d, rc=rc); ESMF_ERR_RETURN(rc)
                 call set_iodesc(shape(array_r4_1d), var_info_arr(i))
                 nullify (array_r4_1d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_1d, rc=rc); ESMF_ERR_RETURN(rc)
                 call set_iodesc(shape(array_r8_1d), var_info_arr(i))
                 nullify (array_r8_1d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_1d, rc=rc); ESMF_ERR_RETURN(rc)
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

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r4_2d), var_info_arr(i))
                 nullify (array_r4_2d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r8_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r8_2d), var_info_arr(i))
                 nullify (array_r8_2d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_i4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_i4_2d), var_info_arr(i))
                 nullify (array_i4_2d)

              else
                 if (mype == 0) write(0,*)'3) Unsupported typekind ', typekind
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

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r4_3d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r4_3d), var_info_arr(i))
                 nullify (array_r4_3d)

              else if (typekind == ESMF_TYPEKIND_R8) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_r8_3d, dim=1) == var_info_arr(i) % dimSizes(1))
                 call set_iodesc(shape(array_r8_3d), var_info_arr(i))
                 nullify (array_r8_3d)

              else if (typekind == ESMF_TYPEKIND_I4) then

                 call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                 ASSERT (size(array_i4_2d, dim=1) == var_info_arr(i) % dimSizes(1))
                 ASSERT (size(array_i4_2d, dim=2) == var_info_arr(i) % dimSizes(2))
                 call set_iodesc(shape(array_i4_3d), var_info_arr(i))
                 nullify (array_i4_3d)
              else
                 if (mype == 0) write(0,*)'4) Unsupported typekind ', typekind
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
              end if
          else
                 if (mype == 0) write(0,*)'Unsupported rank_esmf ', rank_esmf
                 call ESMF_Finalize(endflag=ESMF_END_ABORT)
          end if ! rank_esmf
       else ! non decomposed
           ! Variable not stored in an ESMF Array but in Info
           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(arrName)//'_rank', value=rank, rc=rc); ESMF_ERR(rc)
           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(arrName)//'_type', value=var_info_arr(i) % pio_type, rc=rc); ESMF_ERR(rc)

           if (rank == 0 .and. var_info_arr(i) % pio_type == PIO_char) then
                  rank = rank + 1
           end if
           ! FIXME pass hasTimeDimension and use it here
           if (rank == 0 .and. trim(arrName) == 'Time') then
                  rank = rank + 1
           end if

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

    retVal = PIO_createfile(pioIoSystem(1), pioFileDesc, iotype, trim(fileName), PIO_64BIT_OFFSET)
    call errorHandle('Could not create '//trim(fileName), retVal)

    ! Define dimensions
    do i = 1, size(dim_info_arr)
       if (trim(dim_info_arr(i) % dimName) == 'Time') then
           retVal = PIO_def_dim(pioFileDesc, trim(dim_info_arr(i) % dimName), PIO_UNLIMITED, dim_info_arr(i) % dimId)
           call errorHandle('Could not define dimension', retVal)
       else
           retVal = PIO_def_dim(pioFileDesc, trim(dim_info_arr(i) % dimName), dim_info_arr(i) % dimSize, dim_info_arr(i) % dimId)
           call errorHandle('Could not define dimension', retVal)
       end if
    end do

   ! Define variables
   do i = 1, var_count
      do n = 1, var_info_arr(i) % rank
         call get_dimid_for_dimname(var_info_arr(i) % dimNames(n), var_info_arr(i) % dimIDs(n), var_info_arr(i) % dimSizes(n))
      end do

      retVal = PIO_def_var(pioFileDesc, var_info_arr(i) % varName, var_info_arr(i) % pio_type, var_info_arr(i) % dimids, var_info_arr(i) % varid)
      call errorHandle('Could not create '//trim(fileName), retVal)

      ! Define variable attributes
      call ESMF_InfoGetAlloc(bundle_info, key='/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//'_att_names', values=var_att_names, itemCount=itemCount, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//'_att_names', size=numAtt, rc=rc); ESMF_ERR(rc)
      do k = 1, numAtt
         key = '/NetCDF/FV3/variables/'//trim(var_info_arr(i) % varName)//':'//trim(var_att_names(k))
         typekind = ESMF_InfoGetTK(bundle_info, key=trim(key), rc=rc); ESMF_ERR(rc)
         if (typekind == ESMF_TYPEKIND_I4) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=iVal, rc=rc); ESMF_ERR(rc)
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), iVal)
         else if (typekind == ESMF_TYPEKIND_R4) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=r4Val, rc=rc); ESMF_ERR(rc)
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), r4Val)
         else if (typekind == ESMF_TYPEKIND_R8) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=r8Val, rc=rc); ESMF_ERR(rc)
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), r8Val)
         else if (typekind == ESMF_TYPEKIND_CHARACTER) then
            call ESMF_InfoGet(bundle_info, key=trim(key), value=cVal, rc=rc); ESMF_ERR(rc)
            retVal = pio_put_att(pioFileDesc, var_info_arr(i) % varid, trim(var_att_names(k)), trim(cVal))
         else
            write(0,*)'Unsupported typekind ', typekind
            call ESMF_Finalize(endflag=ESMF_END_ABORT)
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
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), iVal)
      else if (typekind == ESMF_TYPEKIND_R4) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=r4Val, rc=rc); ESMF_ERR(rc)
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), r4Val)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=r8Val, rc=rc); ESMF_ERR(rc)
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), r8Val)
      else if (typekind == ESMF_TYPEKIND_CHARACTER) then
         call ESMF_InfoGet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(global_att_names(i)), value=cVal, rc=rc); ESMF_ERR(rc)
         retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, trim(global_att_names(i)), cVal)
      else
         write(0,*)'Unsupported typekind ', typekind
         call ESMF_Finalize(endflag=ESMF_END_ABORT)
      end if
      ! retVal = pio_put_att(pioFileDesc, PIO_GLOBAL, 'file_id', 'random num')
   end do

   retVal = PIO_enddef(pioFileDesc)
   call errorHandle('Could not end define mode', retVal)


   ! Write variables
   do i = 1, var_count
       call PIO_setframe(pioFileDesc, var_info_arr(i) % varid, frame)
       if (var_info_arr(i) % isESMFArray) then

           call ESMF_FieldBundleGet(wrtfb, trim(var_info_arr(i) % varName), field=field, fieldCount=fieldCount, isPresent=isPresent, rc=rc); ESMF_ERR(rc)

           call ESMF_FieldGet(field, name=arrName, rank=rank, typekind=typekind, rc=rc); ESMF_ERR_RETURN(rc)

           if (rank == 1) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_1d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_1d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_1d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_1d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_1d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_1d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'5) Unsupported typekind ', typekind
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if
           else if (rank == 2) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_2d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_2d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_2d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_2d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_2d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'6) Unsupported typekind ', typekind
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if

           else if (rank == 3) then
             if (typekind == ESMF_TYPEKIND_R4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r4_3d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_R8) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_r8_3d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_r8_3d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else if (typekind == ESMF_TYPEKIND_I4) then

                call ESMF_FieldGet(field, localDe=0, farrayPtr=array_i4_3d, rc=rc); ESMF_ERR_RETURN(rc)
                call PIO_write_darray(pioFileDesc, var_info_arr(i) % varid, var_info_arr(i) % iodesc, array_i4_3d, retVal)
                call errorHandle('Could not write '//trim(arrName), retVal)

             else
                if (mype == 0) write(0,*)'7) Unsupported typekind ', typekind, trim(arrName)
                call ESMF_Finalize(endflag=ESMF_END_ABORT)
             end if
           else
             if (mype == 0) write(0,*)'Unsupported rank ', rank
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if ! end rank
       else ! non ESMF Array

           ! Get data from MPAS Info
           rank = var_info_arr(i) % rank
           pio_type = var_info_arr(i) % pio_type

           if (rank == 0) then
               if (pio_type == PIO_real) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r4Val, rc=rc); ESMF_ERR(rc)
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, r4Val)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else if (pio_type == PIO_double) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r8Val, rc=rc); ESMF_ERR(rc)
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, r8Val)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else if (pio_type == PIO_int) then
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=iVal, rc=rc); ESMF_ERR(rc)
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, iVal)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else
                   if (mype == 0) write(0,*)'1) Unsupported pio_type', pio_type
               end if
           else if (rank == 1) then
               if (pio_type == PIO_real) then
                   if (trim(var_info_arr(i) % varName) == 'Time') then
                       r4Val = seconds_since_start
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r4Val)
                       call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                   else
                       call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=real4Vals, rc=rc)
                       if (rc == ESMF_RC_ATTR_WRONGTYPE) then
                           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r4Val, rc=rc); ESMF_ERR(rc)
                           retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r4Val)
                           call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                       else
                           ESMF_ERR(rc)
                           retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, real4Vals)
                           call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                       end if
                   end if
               else if (pio_type == PIO_double) then
                   if (trim(var_info_arr(i) % varName) == 'Time') then
                       r8Val = seconds_since_start
                       retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r8Val)
                       call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                   else
                       call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=real8Vals, rc=rc)
                       if (rc == ESMF_RC_ATTR_WRONGTYPE) then
                           call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=r8Val, rc=rc); ESMF_ERR(rc)
                           retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], r8Val)
                           call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                       else
                           ESMF_ERR(rc)
                           retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, real8Vals)
                           call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
                       end if
                   end if
               else if (pio_type == PIO_int) then
                   call ESMF_InfoGetAlloc(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), values=intVals, rc=rc); ESMF_ERR(rc)
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, intVals)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else if (pio_type == PIO_char) then
                   textVal = ' '
                   call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=textVal, rc=rc); ESMF_ERR(rc)
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1], textVal)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else
                   if (mype == 0) write(0,*)'2) Unsupported pio_type', pio_type
               end if
           else if (rank == 2) then
               if (pio_type == PIO_char) then
                   textVal = ' '
                   if (trim(var_info_arr(i) % varName) == 'xtime') then
                       textVal = xtime
                   else
                       call ESMF_InfoGet(bundle_info, key='/MPAS/'//trim(var_info_arr(i) % varName), value=textVal, rc=rc); ESMF_ERR(rc)
                   end if
                   retVal = PIO_put_var(pioFileDesc, var_info_arr(i) % varid, [1,1], textVal)
                   call errorHandle('Could not write '//trim(var_info_arr(i) % varName), retVal)
               else
                   if (mype == 0) write(0,*)'3) Unsupported pio_type', pio_type
               end if
           else
             if (mype == 0) write(0,*)'Unsupported rank ', rank
             call ESMF_Finalize(endflag=ESMF_END_ABORT)
           end if

       end if ! isESMFArray
    end do

    call PIO_syncfile(pioFileDesc)

    call PIO_closefile(pioFileDesc)

    ! call PIO_freedecomp(pioIoSystem(1), iodescCells)
    ! call PIO_freedecomp(pioIoSystem(1), iodescVertices)
    ! call PIO_freedecomp(pioIoSystem(1), iodescEdges)

    call PIO_finalize(pioIoSystem(1), ierr)

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
       type(var_info_t), intent(inout) :: var_info

       integer, allocatable  :: compdof(:)
       integer :: i1, i2, i3
       integer :: n, ndim, ncompdof
       integer :: k

       do n = 1, num_decomp_info_array
           if (decomp_info_arr(n) % pio_type == var_info % pio_type) then
           if (size(decomp_info_arr(n) % array_shape) == size(array_shape)) then
           if (ALL(decomp_info_arr(n) % array_shape == array_shape)) then
           if (size(decomp_info_arr(n) % array_index) == size(array_index)) then
           if (ALL(decomp_info_arr(n) % array_index == array_index)) then
               ! Found iodesc in the cache, use it.
               var_info % iodesc = decomp_info_arr(n) % iodesc
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

       decomp_info_arr(num_decomp_info_array) % pio_type = var_info % pio_type
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
       call PIO_initdecomp(pioIoSystem(1), var_info % pio_type, var_info % dimSizes, compdof, decomp_info_arr(num_decomp_info_array) % iodesc)

       var_info % iodesc = decomp_info_arr(num_decomp_info_array) % iodesc

       deallocate (compdof)

    end subroutine set_iodesc

    subroutine errorHandle(errMsg, retVal)
        implicit none
        character(len=*),       intent(in)    :: errMsg
        integer,                intent(in)    :: retVal
        if (retVal /= PIO_NOERR) then
            write(*,*) retVal,errMsg
            call PIO_closefile(pioFileDesc)
            stop 1
       end if
    end subroutine errorHandle

  end subroutine write_mpas_restart_field_bundle_pio

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
end module module_write_mpas_restart_field_bundle_pio
