#define ESMF_ERR(rc) \
  if (rc /= 0) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (rc /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1

module ufs_mpas_wgc_output

  use mpi_f08
  use esmf
  use pio, only : PIO_int, PIO_real, PIO_double, PIO_char

  use mpas_derived_types, only : domain_type
  use mpas_kind_types,    only : StrKIND, RKIND, R4KIND, R8KIND

  use module_mpasmodel_config, only : nVertLevels
  use module_mpasmodel_config, only : nCellsGlobal, nVerticesGlobal, nEdgesGlobal
  use module_mpasmodel_config, only : nCellsSolve, nVerticesSolve, nEdgesSolve
  use module_mpasmodel_config, only : domain_ptr => domain

  implicit none

  private

  public :: ufs_mpas_get_esmf_mesh

  public :: ufs_mpas_create_history_bundle
  public :: ufs_mpas_update_history_bundle

  public :: ufs_mpas_create_restart_bundle
  public :: ufs_mpas_update_restart_bundle

  public :: ufs_mpas_create_restart_array_bundle
  public :: ufs_mpas_update_restart_array_bundle

contains

 subroutine ufs_mpas_create_history_bundle(output_bundle, output_vars, interp_method, rc)

   type(ESMF_FieldBundle), intent(out) :: output_bundle
   character(len=*), intent(in)        :: output_vars(:)
   character(len=*), intent(in)        :: interp_method
   integer, intent(out)                :: rc

   type(ESMF_Info) :: bundle_info
   character(*), parameter :: subname = 'ufs_mpas_create_history_bundle'

   call ufs_mpas_create_output_bundle(output_bundle, 'atm_'//trim(interp_method), output_vars, rc); ESMF_ERR(rc)

   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)
   ! call ESMF_FieldBundlePrint(output_bundle, rc=rc); ESMF_ERR(rc)
   ! write(*,*)'bundle '//trim(interp_method)
   ! call ESMF_InfoPrint(bundle_info, rc=rc); ESMF_ERR(rc)

 end subroutine ufs_mpas_create_history_bundle

 subroutine ufs_mpas_update_history_bundle(output_bundle, output_vars, rc)

   type(ESMF_FieldBundle), intent(inout) :: output_bundle
   character(len=*), intent(in)          :: output_vars(:)
   integer, intent(out)                  :: rc

   character(*), parameter :: subname = 'ufs_mpas_update_history_bundle'

   call ufs_mpas_update_output_bundle(output_bundle, output_vars, rc); ESMF_ERR(rc)

 end subroutine ufs_mpas_update_history_bundle

 subroutine ufs_mpas_create_output_bundle(output_bundle, bundle_name, output_vars, rc)

   use mpas_attlist,       only : att_list_type, att_lists_type, &
                                  MPAS_ATT_INT, MPAS_ATT_INTA, MPAS_ATT_REAL,MPAS_ATT_REALA, MPAS_ATT_TEXT, &
                                  MPAS_LOG_CRIT
   use mpas_derived_types, only : field1dinteger, field2dinteger, field1dreal, field2dreal, field3dreal, &
                                  mpas_pool_type, mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, block_type
   use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_dimension
   use mpas_log,           only : mpas_log_write

   type(ESMF_FieldBundle), intent(out) :: output_bundle
   character(len=*), intent(in)        :: bundle_name
   character(len=*), intent(in)        :: output_vars(:)
   integer, intent(out)                :: rc

   character(*), parameter :: subname = 'ufs_mpas_create_output_bundle'

   character(64) :: field_name
   type(mpas_pool_type), pointer :: allFields
   type(mpas_pool_field_info_type) :: mpas_pool_field_info
   type(field1dinteger), pointer :: field_1d_integer
   type(field2dinteger), pointer :: field_2d_integer
   type(field1dreal), pointer :: field_1d_real
   type(field2dreal), pointer :: field_2d_real
   type(field3dreal), pointer :: field_3d_real

   type(ESMF_Mesh) :: mesh
   type(ESMF_Field) :: field
   type(ESMF_Info) :: field_info, bundle_info

   real(ESMF_KIND_R4), pointer    :: ptr_r4_d1(:), ptr_r4_d2(:,:), ptr_r4_d3(:,:,:)
   real(ESMF_KIND_R8), pointer    :: ptr_r8_d1(:), ptr_r8_d2(:,:), ptr_r8_d3(:,:,:)
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   integer :: frestart(1)
   integer :: i,j,k,n
   integer :: localpet

   type :: dim_info_t
     character(64) :: dimName
     integer :: dimSize
   end type

   type (dim_info_t), allocatable :: dim_info_arr(:)

   type (block_type), pointer :: block
   type (att_list_type), pointer :: att_cursor => null()
   type (att_lists_type), dimension(:), pointer :: attLists => null()
   character (len=StrKIND), dimension(5) :: dimNames
   logical :: isVarArray
   integer :: total_unique, nDims, dimSize

   rc = 0

   ASSERT (size(output_vars) > 0)

   frestart(:) = -1

   localpet = domain_ptr % dminfo % my_proc_id

   output_bundle = ESMF_FieldBundleCreate(name=trim(bundle_name), rc=rc); ESMF_ERR(rc)
   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

   call ufs_mpas_get_esmf_mesh(mesh, rc=rc); ESMF_ERR(rc)

   allFields => domain_ptr % blocklist % allfields

   ! call mpas_log_write('   allfields:')
   ! call pool_print_members(domain_ptr % blocklist % allfields)
   ! call mpas_log_write('---------------')

   allocate(dim_info_arr(0))

   do n = 1, size(output_vars)
      field_name = trim(adjustl(output_vars(n)))

      call mpas_log_write('Inquiring field information for "' // trim(field_name) // '"')

      call mpas_pool_get_field_info(allFields, trim(field_name), mpas_pool_field_info)

      if (mpas_pool_field_info % fieldtype == -1 .or. &
          mpas_pool_field_info % ndims == -1 .or. &
          mpas_pool_field_info % nhalolayers == -1) then
         call mpas_log_write(subname//' Invalid field information for "' // trim(field_name) // '"', MPAS_LOG_CRIT)
      end if

      attLists => null()
      dimNames = ''
      nDims = mpas_pool_field_info % nDims
      isVarArray = .false.

      select case (mpas_pool_field_info % fieldtype)
      case (mpas_pool_integer)
         select case (mpas_pool_field_info % ndims)
         case (1)
            call mpas_pool_get_field(allFields, trim(field_name), field_1d_integer, timelevel=1)
            attLists => field_1d_integer % attLists
            dimNames(1:nDims) = field_1d_integer % dimNames
            block => field_1d_integer % block

            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
            ptr_i4_d1 = field_1d_integer%array(1:nCellsSolve)

            call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
            call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=field_1d_integer % missingValue, rc=rc); ESMF_ERR(rc)
            nullify(field_1d_integer)
         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_integer, timelevel=1)
            attLists => field_2d_integer % attLists
            dimNames(1:nDims) = field_2d_integer % dimNames
            block => field_2d_integer % block

            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(field_2d_integer%array,dim=1)], &
                                     meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
            ptr_i4_d2 = field_2d_integer%array(:,1:nCellsSolve)

            call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
            call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=field_2d_integer % missingValue, rc=rc); ESMF_ERR(rc)
            nullify(field_2d_integer)
         case default
            call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_CRIT, intArgs=(/ mpas_pool_field_info % ndims /))
         end select

      case (mpas_pool_real)
         select case (mpas_pool_field_info % ndims)

         case (1)
            call mpas_pool_get_field(allFields, trim(field_name), field_1d_real, timelevel=1)
            attLists => field_1d_real % attLists
            dimNames(1:nDims) = field_1d_real % dimNames
            block => field_1d_real % block

            if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                call ESMF_FieldGet(field, farrayPtr=ptr_r4_d1, rc=rc); ESMF_ERR(rc)
                ptr_r4_d1 = field_1d_real%array(1:nCellsSolve)
            else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_NODE, name=trim(output_vars(n)), rc=rc); ESMF_ERR(rc)
                call ESMF_FieldGet(field, farrayPtr=ptr_r4_d1, rc=rc); ESMF_ERR(rc)
                ptr_r4_d1 = field_1d_real%array(1:nVerticesSolve)
            else
                if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
            end if

            call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
            ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_1d_real % missingValue, rc=rc); ESMF_ERR(rc)
            call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=9.99e20, rc=rc); ESMF_ERR(rc)
            nullify(field_1d_real)

         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_real, timelevel=1)
            attLists => field_2d_real % attLists
            dimNames(1:nDims) = field_2d_real % dimNames
            block => field_2d_real % block

            if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(field_2d_real%array,dim=1)], &
                                         meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                ptr_r4_d2 = field_2d_real%array(:,1:nCellsSolve)
            else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(field_2d_real%array,dim=1)], &
                                         meshloc=ESMF_MESHLOC_NODE, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                ptr_r4_d2 = field_2d_real%array(:,1:nVerticesSolve)
            else
                if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                cycle
            end if

            call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
            ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_2d_real % missingValue, rc=rc); ESMF_ERR(rc)
            call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=9.99e20, rc=rc); ESMF_ERR(rc)
            nullify(field_2d_real)

         case (3)
            call mpas_pool_get_field(allFields, trim(field_name), field_3d_real, timelevel=1)
            attLists => field_3d_real % attLists
            block => field_3d_real % block

            if (field_3d_real % isVarArray) then

               nDims = nDims - 1
               dimNames(1:nDims) = field_3d_real % dimNames(2: mpas_pool_field_info % nDims)   ! strip first dimension (num_scalar)
               isVarArray = .true.

               do k = 1, size(field_3d_real % constituentNames)

                  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(field_3d_real%array,dim=2)], &
                                           meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_3d_real % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                  ptr_r4_d2 = field_3d_real%array(k,:,1:nCellsSolve)

                  call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                  ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_2d_real % missingValue, rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=9.99e20, rc=rc); ESMF_ERR(rc)

                  call add_field_to_bundle(field_3d_real % constituentNames(k), attLists(k) % attList)

               end do ! k = 1, size(field_3d_real % constituentNames)

            else

               dimNames(1:nDims) = field_3d_real % dimNames
               field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/3/), ungriddedLBound=[1,1], ungriddedUBound=[size(field_3d_real%array,dim=1), size(field_3d_real%array,dim=2)], &
                                        meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
               call ESMF_FieldGet(field, farrayPtr=ptr_r4_d3, rc=rc); ESMF_ERR(rc)
               ptr_r4_d3 = field_3d_real%array(:,:,1:nCellsSolve)

               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
               ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_3d_real % missingValue, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=9.99e20, rc=rc); ESMF_ERR(rc)

            end if

            nullify(field_3d_real)

         case default
            call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_CRIT, intArgs=(/ mpas_pool_field_info % ndims /))
         end select
      case default
         call mpas_log_write(subname//' Unsupported field type (Must be one of: integer, real)', MPAS_LOG_CRIT)
      end select

      if (.not. isVarArray) then
         call add_field_to_bundle(field_name, attLists(1) % attList)
      end if

   end do

   ! bundle attributes
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3/grid_id", value=1, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)

   ! dimensions attributes
   do i = 1, size(dim_info_arr)
       call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/ungridded_dimensions/'//trim(dim_info_arr(i) % dimName), value=dim_info_arr(i) % dimSize, rc=rc); ESMF_ERR(rc)
   end do

   contains

       subroutine add_field_to_bundle(varName, attList)

          character(len=*), intent(in) :: varName
          type(att_list_type), pointer, intent(in) :: attList

          integer :: i
          logical :: is_unique
          integer, pointer :: dimSize_ptr

          call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="atm", rc=rc); ESMF_ERR(rc)

          do i = 1, nDims
            call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

            if (associated(dimSize_ptr)) then
                dimSize = dimSize_ptr
            else
                dimSize = -1
            end if

            if (i == nDims) then ! last dimension should be one of the decomposed nCells, nEdges, nVertices
                if (trim(dimNames(i)) == 'nCells' .or. trim(dimNames(i)) == 'nEdges' .or. trim(dimNames(i)) == 'nVertices') then
                    cycle
                else
                    call mpas_log_write(subname//' Last dimension is not a decomposed dimension', MPAS_LOG_CRIT)
                end if
            end if

            if (dimSize >= 0) then
                is_unique = .true.
                do j = 1, size(dim_info_arr)
                    if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                        if (dimSize /= dim_info_arr(j) % dimSize) then
                           write(0,*)'conflicting dimSize for ', trim(varName), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
                           stop 1
                        end if
                        is_unique = .false.
                        exit
                    end if
                end do
                ! If unique, append to collection
                if (is_unique) then
                    total_unique = size(dim_info_arr)
                    call resize_dim_info_array(dim_info_arr, total_unique + 1)
                    dim_info_arr(total_unique + 1) % dimName = trim(dimNames(i))
                    dim_info_arr(total_unique + 1) % dimSize = dimSize
                end if
            end if

          end do

          call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/variables/'//trim(varName), values=dimNames(1:nDims-1), rc=rc); ESMF_ERR(rc) ! last nDims is distributed dimension

          att_cursor => attList
          do while (associated(att_cursor))
             if (att_cursor % attType == MPAS_ATT_INT) then
                call ESMF_InfoSet(field_info, key="/NetCDF/FV3/"//trim(att_cursor % attName), value=att_cursor % attValueInt, rc=rc); ESMF_ERR(rc)
             else if (att_cursor % attType == MPAS_ATT_REAL) then
                call ESMF_InfoSet(field_info, key="/NetCDF/FV3/"//trim(att_cursor % attName), value=att_cursor % attValueReal, rc=rc); ESMF_ERR(rc)
             else if (att_cursor % attType == MPAS_ATT_TEXT) then
                call ESMF_InfoSet(field_info, key="/NetCDF/FV3/"//trim(att_cursor % attName), value=trim(att_cursor % attValueText), rc=rc); ESMF_ERR(rc)
             else
                ! write(0,*) i, '"'//trim(att_cursor % attName)//'" unknown type ', att_cursor % attType
             end if
             att_cursor => att_cursor % next
          end do

          nullify(att_cursor)

          call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)

       end subroutine add_field_to_bundle

       subroutine resize_dim_info_array(arr, new_size)
          type(dim_info_t), allocatable, intent(inout) :: arr(:)
          integer, intent(in) :: new_size

          type(dim_info_t), allocatable :: temp(:)
          integer :: old_size, copy_size

          old_size = size(arr)
          allocate(temp(new_size))

          ! Copy existing elements
          copy_size = min(old_size, new_size)
          temp(1:copy_size) = arr(1:copy_size)

          ! Deallocate and reassign
          deallocate(arr)
          call move_alloc(temp, arr)
       end subroutine resize_dim_info_array

 end subroutine ufs_mpas_create_output_bundle

 subroutine ufs_mpas_update_output_bundle(output_bundle, output_vars, rc)

   use mpas_attlist,       only : MPAS_LOG_CRIT
   use mpas_derived_types, only : field1dinteger, field2dinteger, field1dreal, field2dreal, field3dreal, &
                                  mpas_pool_type, mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, block_type
   use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_dimension
   use mpas_log,           only : mpas_log_write

   type(ESMF_FieldBundle), intent(inout) :: output_bundle
   character(len=*), intent(in)          :: output_vars(:)
   integer, intent(out)                  :: rc

   character(*), parameter :: subname = 'ufs_mpas_update_output_bundle'

   character(64) :: field_name
   type(mpas_pool_type), pointer :: allFields
   type(mpas_pool_field_info_type) :: mpas_pool_field_info
   type(field1dinteger), pointer :: field_1d_integer
   type(field2dinteger), pointer :: field_2d_integer
   type(field1dreal), pointer :: field_1d_real
   type(field2dreal), pointer :: field_2d_real
   type(field3dreal), pointer :: field_3d_real

   type(ESMF_Field) :: field

   real(ESMF_KIND_R4), pointer    :: ptr_r4_d1(:), ptr_r4_d2(:,:), ptr_r4_d3(:,:,:)
   real(ESMF_KIND_R8), pointer    :: ptr_r8_d1(:), ptr_r8_d2(:,:), ptr_r8_d3(:,:,:)
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   integer :: i,j,k,n
   integer :: localpet

   character (len=StrKIND), dimension(5) :: dimNames
   logical :: isVarArray
   integer :: nDims

   rc = 0

   ASSERT (size(output_vars) > 0)

   localpet = domain_ptr % dminfo % my_proc_id

   allFields => domain_ptr % blocklist % allfields

   do n = 1, size(output_vars)
      field_name = trim(adjustl(output_vars(n)))

      call mpas_log_write('Inquiring field information for "' // trim(adjustl(field_name)) // '"')

      call mpas_pool_get_field_info(allFields, trim(field_name), mpas_pool_field_info)

      if (mpas_pool_field_info % fieldtype == -1 .or. &
          mpas_pool_field_info % ndims == -1 .or. &
          mpas_pool_field_info % nhalolayers == -1) then
         call mpas_log_write(subname//' Invalid field information for "' // trim(field_name) // '"', MPAS_LOG_CRIT)
      end if

      dimNames = ''
      nDims = mpas_pool_field_info % nDims
      isVarArray = .false.

      select case (mpas_pool_field_info % fieldtype)
      case (mpas_pool_integer)
         select case (mpas_pool_field_info % ndims)
         ! FIXME add check for nCells vs. nVertices
         case (1)
            call mpas_pool_get_field(allFields, trim(field_name), field_1d_integer, timelevel=1)
            dimNames(1:nDims) = field_1d_integer % dimNames
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
            ptr_i4_d1 = field_1d_integer%array(1:nCellsSolve)
            nullify(field_1d_integer)
         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_integer, timelevel=1)
            dimNames(1:nDims) = field_2d_integer % dimNames
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
            ptr_i4_d2 = field_2d_integer%array(:,1:nCellsSolve)
            nullify(field_2d_integer)
         case default
            call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_CRIT, intArgs=(/ mpas_pool_field_info % ndims /))
         end select

      case (mpas_pool_real)
         select case (mpas_pool_field_info % ndims)

         case (1)
            call mpas_pool_get_field(allFields, trim(field_name), field_1d_real, timelevel=1)
            dimNames(1:nDims) = field_1d_real % dimNames
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d1, rc=rc); ESMF_ERR(rc)
            if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
               ptr_r4_d1 = field_1d_real%array(1:nCellsSolve)
            else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
               ptr_r4_d1 = field_1d_real%array(1:nVerticesSolve)
            else
               if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
               cycle
            end if
            nullify(field_1d_real)
         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_real, timelevel=1)
            dimNames(1:nDims) = field_2d_real % dimNames
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
            if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
               ptr_r4_d2 = field_2d_real%array(:,1:nCellsSolve)
            else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
               ptr_r4_d2 = field_2d_real%array(:,1:nVerticesSolve)
            else
               if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
               cycle
            end if
            nullify(field_2d_real)

         case (3)
            call mpas_pool_get_field(allFields, trim(field_name), field_3d_real, timelevel=1)
            dimNames(1:nDims) = field_3d_real % dimNames

            if (field_3d_real % isVarArray) then
               do k = 1, size(field_3d_real % constituentNames)
                  call ESMF_FieldBundleGet(output_bundle, fieldName=trim(field_3d_real % constituentNames(k)), field=field, rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                  if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                     ptr_r4_d2 = field_3d_real%array(k,:,1:nCellsSolve)
                  else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                     ptr_r4_d2 = field_3d_real%array(k,:,1:nVerticesSolve)
                  else
                     if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                     cycle
                  end if
               end do ! k = 1, size(field_3d_real % constituentNames)
            else
               call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
               call ESMF_FieldGet(field, farrayPtr=ptr_r4_d3, rc=rc); ESMF_ERR(rc)
               if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                  ptr_r4_d3 = field_3d_real%array(:,:,1:nCellsSolve)
               else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                  ptr_r4_d3 = field_3d_real%array(:,:,1:nVerticesSolve)
               else
                  if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                  cycle
               end if
            end if
            nullify(field_3d_real)

         case default
            call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_CRIT, intArgs=(/ mpas_pool_field_info % ndims /))
         end select
      case default
         call mpas_log_write(subname//' Unsupported field type (Must be one of: integer, real)', MPAS_LOG_CRIT)
      end select

   end do

 end subroutine ufs_mpas_update_output_bundle


 subroutine ufs_mpas_create_restart_bundle(output_bundle, rc)

   use mpas_derived_types, only : MPAS_stream_list_type, field_list_type
   use mpas_stream_list,   only : MPAS_stream_list_query
   use mpas_log,           only : mpas_log_write
   use mpas_pool_routines, only : pool_print_members

   use mpas_attlist,       only : att_list_type, att_lists_type, &
                                  MPAS_ATT_INT, MPAS_ATT_INTA, MPAS_ATT_REAL,MPAS_ATT_REALA, MPAS_ATT_TEXT
   use mpas_derived_types, only : MPAS_stream_list_type, MPAS_Pool_type, MPAS_Pool_iterator_type, mpas_pool_field_info_type, block_type,  &
                                  field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                  field3DInteger, field2DInteger, field1DInteger, field0DInteger, &
                                  field1DChar, field0DChar, &
                                  MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, MPAS_POOL_LOGICAL, &
                                  MPAS_POOL_SILENT, MPAS_POOL_CONFIG
   use mpas_pool_routines, only : mpas_pool_get_next_member, mpas_pool_get_config, mpas_pool_get_error_level, &
                                  mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_set_error_level, &
                                  mpas_pool_begin_iteration, mpas_pool_get_dimension
   use mpas_stream_manager, only : prewrite_reindex, postwrite_reindex

   type(ESMF_FieldBundle), intent(out) :: output_bundle
   integer, intent(out)                :: rc


   integer :: localpet, nprocs

   type (MPAS_stream_list_type), pointer :: stream
   type (MPAS_Pool_type), pointer :: allFields
   type (MPAS_Pool_type), pointer :: allPackages
   integer :: timeLevelIn
   integer :: ierr

   type (ESMF_Mesh) :: mesh
   type (ESMF_Field) :: field
   type (ESMF_Info) :: field_info, bundle_info
   type (ESMF_TypeKind_Flag) :: rkind_typekind ! Default ESMF real typekind that corresponds to default MPAS real kind

   real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:) ! Default MPAS real kind
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   type (MPAS_Pool_iterator_type) :: itr
   type (mpas_pool_field_info_type) :: info
   integer :: timeLevel

   type (field5DReal), pointer :: real5d
   type (field4DReal), pointer :: real4d
   type (field3DReal), pointer :: real3d
   type (field2DReal), pointer :: real2d
   type (field1DReal), pointer :: real1d
   type (field0DReal), pointer :: real0d

   type (field3DInteger), pointer :: int3d
   type (field2DInteger), pointer :: int2d
   type (field1DInteger), pointer :: int1d
   type (field0DInteger), pointer :: int0d

   type (field1DChar), pointer :: char1d
   type (field0DChar), pointer :: char0d

   integer, pointer :: intAtt
   logical, pointer :: logAtt
   character (len=StrKIND), pointer :: charAtt
   real (kind=RKIND), pointer :: realAtt

   character (len=StrKIND), pointer :: packages
   logical :: active_field
   integer :: err_level

   type (block_type), pointer :: block => null()
   character (len=StrKIND), dimension(5+1) :: dimNames ! +1 for Time
   type (att_list_type), pointer :: att_cursor => null()
   type (att_lists_type), dimension(:), pointer :: attLists => null()
   integer :: i, j, k, total_unique, nDims, dimSize
   integer, pointer :: dimSize_ptr
   logical :: hasTimeDimension = .false.
   logical :: is_unique
   logical :: isVarArray
   character (len=8) :: typeName
   character(len=64) :: decomp_dim_name
   character(len=64), allocatable :: dimension_names(:)
   character(len=64) :: variable_names(3000)
   character(len=64) :: att_names(256)
   character(len=256), allocatable :: global_att_names(:)
   integer :: numAtts
   integer :: numVars

   type :: dim_info_t
     character(64) :: dimName
     integer :: dimSize
   end type

   type (dim_info_t), allocatable :: dim_info_arr(:)

   character(*), parameter :: subname = 'dyn_mpas_subdriver::ufs_mpas_create_restart_bundle'

   rc = 0

   ! Look at 'restart' stream
   nullify(stream)
   if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, 'restart', stream, ierr=ierr)) then
      rc = 1
      return
   end if

   allFields => domain_ptr % streamManager % allFields
   allPackages => domain_ptr % streamManager % allPackages
   timeLevelIn = 1

   if (RKIND == R4KIND) then
       rkind_typekind = ESMF_TYPEKIND_R4
   else if (RKIND == R8KIND) then
       rkind_typekind = ESMF_TYPEKIND_R8
   else
       write(0,*)'Unrecognized RKIND'
       rc = 1
       return
   end if

   output_bundle = ESMF_FieldBundleCreate(name='restart_mpas', rc=rc); ESMF_ERR(rc)

   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

   localpet = domain_ptr % dminfo % my_proc_id
   nprocs = domain_ptr % dminfo % nprocs

   call ufs_mpas_get_esmf_mesh(mesh, rc=rc); ESMF_ERR(rc)

   allocate(dim_info_arr(0))

   variable_names = ''
   numVars = 0

   call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

   call mpas_pool_begin_iteration(stream % field_pool)
   FIELD_LOOP: do while ( mpas_pool_get_next_member(stream % field_pool, itr) )

       if (itr % memberType == MPAS_POOL_CONFIG) then

           err_level = mpas_pool_get_error_level()
           call mpas_pool_set_error_level(MPAS_POOL_SILENT)

           nullify(packages)
           call mpas_pool_get_config(stream % field_pkg_pool, trim(itr % memberName)//':packages', packages)
           if (associated(packages)) then
               active_field = parse_package_list(allPackages, trim(packages))
           else
               active_field = .true.
           end if
           call mpas_pool_set_error_level(err_level)

           if (.not. active_field) then
               write(0,*)'skip .not. active_field: ',trim(itr % memberName)
               cycle FIELD_LOOP
           end if

           ! To avoid accidentally matching in case statements below...
           info % fieldType = -1

           call mpas_pool_get_field_info(allFields, itr % memberName, info)

           ! Set time level to read
           if (info % nTimeLevels >= timeLevelIn) then
               timeLevel = timeLevelIn
           else
               timeLevel = 1
           end if

           hasTimeDimension = .false.
           attLists => null()
           dimNames = ''
           nDims = info % nDims
           isVarArray = .false.

           select case (info % fieldType)
           case (MPAS_POOL_REAL)
               typeName = 'float'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, real0d, timeLevel)
                       hasTimeDimension = real0d % hasTimeDimension
                       attLists => real0d % attLists
                       ! no dimNames in 0d
                       block => real0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=real0d % scalar, rc=rc); ESMF_ERR(rc)
                       if (RKIND == R4KIND) then
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_real, rc=rc); ESMF_ERR(rc)
                       else if (RKIND == R8KIND) then
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_double, rc=rc); ESMF_ERR(rc)
                       end if
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, real1d, timeLevel)
                       hasTimeDimension = real1d % hasTimeDimension
                       attLists => real1d % attLists
                       dimNames(1:info%nDims) = real1d % dimNames;
                       block => real1d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           field = ESMF_FieldCreate(mesh, rkind_typekind, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d1 = real1d % array(1:nCellsSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d1)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           field = ESMF_FieldCreate(mesh, rkind_typekind, meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d1 = real1d % array(1:nVerticesSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d1)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                           cycle FIELD_LOOP
                       else ! Field has no distributed dimension
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=real1d % array, rc=rc); ESMF_ERR(rc)
                           if (RKIND == R4KIND) then
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_real, rc=rc); ESMF_ERR(rc)
                           else if (RKIND == R8KIND) then
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_double, rc=rc); ESMF_ERR(rc)
                           end if
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                       end if

                   case (2)
                       call mpas_pool_get_field(allFields, itr % memberName, real2d, timeLevel)
                       hasTimeDimension = real2d % hasTimeDimension
                       attLists => real2d % attLists
                       dimNames(1:info%nDims) = real2d % dimNames;
                       block => real2d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(real2d%array,dim=1)], &
                                                    meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d2 = real2d % array(:,1:nCellsSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d2)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(real2d%array,dim=1)], &
                                                    meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d2 = real2d % array(:,1:nVerticesSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d2)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                           cycle FIELD_LOOP
                       end if

                   case (3)
                       call mpas_pool_get_field(allFields, itr % memberName, real3d, timeLevel)
                       hasTimeDimension = real3d % hasTimeDimension
                       attLists => real3d % attLists
                       block => real3d % block
                       decomp_dim_name = trim(real3d % dimNames(info%nDims)) ! save decomp name before stripping num_scalar

                       if (real3d % isVarArray) then
                           nDims = nDims - 1
                           dimNames(1:nDims) = real3d % dimNames(2:info % nDims)   ! strip first dimension (num_scalar)
                           isVarArray = .true.

                           if (hasTimeDimension) then
                               nDims = nDims + 1
                               dimNames(nDims:nDims) = 'Time'
                           end if

                           do k = 1, size(real3d % constituentNames)
                               if (trim(decomp_dim_name) == 'nCells') then
                                   field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(real3d%array,dim=2)], &
                                                            meshloc=ESMF_MESHLOC_ELEMENT, name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                                   ptr_rm_d2 = real3d % array(k,:,1:nCellsSolve)

                                   call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                                   call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                                   nullify(ptr_rm_d2)
                               else if (trim(decomp_dim_name) == 'nVertices') then
                                   field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(real3d%array,dim=2)], &
                                                            meshloc=ESMF_MESHLOC_NODE, name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                                   ptr_rm_d2 = real3d % array(k,:,1:nVerticesSolve)

                                   call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                                   call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                                   nullify(ptr_rm_d3)
                               else if (trim(decomp_dim_name) == 'nEdges') then
                                   if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                                   cycle FIELD_LOOP
                               end if

                               ! write(0,'(A,A,A,A,A)',advance='no') '   ',trim(typeName), ' ', trim(real3d % constituentNames(k)),'('
                               do i = 1, nDims
                                 call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

                                 if (associated(dimSize_ptr)) then
                                     dimSize = dimSize_ptr
                                 else
                                     dimSize = -1
                                 end if

                                 if (trim(dimNames(i)) == 'nCells') then
                                     dimSize = nCellsGlobal
                                 else if (trim(dimNames(i)) == 'nEdges') then
                                     dimSize = nEdgesGlobal
                                 else if (trim(dimNames(i)) == 'nVertices' ) then
                                     dimSize = nVerticesGlobal
                                 else if (trim(dimNames(i)) == 'StrLen' ) then
                                     dimSize = 64
                                 else if (trim(dimNames(i)) == 'Time' ) then
                                     dimSize = 1
                                 end if

                                 ! write(0,'(A)',advance='no') trim(dimNames(i))
                                 ! if (i < nDims) write(0,'(A)',advance='no') ', '

                                 if (dimSize >= 0) then
                                     is_unique = .true.
                                     do j = 1, size(dim_info_arr)
                                         if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                                             if (dimSize /= dim_info_arr(j) % dimSize) then
                                                write(0,*)'conflictiing dimSize for ', trim(real3d % constituentNames(k)), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
                                                stop 1
                                             end if
                                             is_unique = .false.
                                             exit
                                         end if
                                     end do
                                     ! If unique, append to collection
                                     if (is_unique) then
                                         total_unique = size(dim_info_arr)
                                         call resize_dim_info_array(dim_info_arr, total_unique + 1)
                                         dim_info_arr(total_unique + 1) % dimName = trim(dimNames(i))
                                         dim_info_arr(total_unique + 1) % dimSize = dimSize
                                     end if
                                 end if

                               end do
                               ! write(0,'(A)')') ;'

                               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(real3d % constituentNames(k)), values=dimNames(1:nDims), rc=rc); ESMF_ERR(rc)

                               numVars = numVars + 1
                               variable_names(numVars) = trim(real3d % constituentNames(k))

                               call put_variable_attributes(attLists(k) % attList, trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                           end do
                       else  ! isVarArray is false
                           dimNames(1:info%nDims) = real3d % dimNames;
                           if (trim(dimNames(info%nDims)) == 'nCells') then
                               field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/3/), ungriddedLBound=[1,1], ungriddedUBound=[size(real3d%array,dim=1), size(real3d%array,dim=2)], &
                                                        meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                               call ESMF_FieldGet(field, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                               ptr_rm_d3 = real3d % array(:,:,1:nCellsSolve)

                               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                               call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                               call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                               nullify(ptr_rm_d3)
                           else if (trim(dimNames(info%nDims)) == 'nVertices') then
                               field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = (/3/), ungriddedLBound=[1,1], ungriddedUBound=[size(real3d%array,dim=1), size(real3d%array,dim=2)], &
                                                        meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                               call ESMF_FieldGet(field, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                               ptr_rm_d3 = real3d % array(:,:,1:nVerticesSolve)

                               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                               call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                               call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                               nullify(ptr_rm_d3)
                           else if (trim(dimNames(info%nDims)) == 'nEdges') then
                               if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                               cycle FIELD_LOOP
                           end if
                       end if

                   case (4)
                       call mpas_pool_get_field(allFields, itr % memberName, real4d, timeLevel)
                       hasTimeDimension = real4d % hasTimeDimension
                       attLists => real4d % attLists
                       dimNames(1:info%nDims) = real4d % dimNames;
                       block => real4d % block
                   case (5)
                       call mpas_pool_get_field(allFields, itr % memberName, real5d, timeLevel)
                       hasTimeDimension = real5d % hasTimeDimension
                       attLists => real5d % attLists
                       dimNames(1:info%nDims) = real5d % dimNames;
                       block => real5d % block
               end select
           case (MPAS_POOL_INTEGER)
               typeName = 'int'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, int0d, timeLevel)
                       hasTimeDimension = int0d % hasTimeDimension
                       attLists => int0d % attLists
                       ! no dimNames in 0d
                       block => int0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=int0d % scalar, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_int, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, int1d, timeLevel)
                       hasTimeDimension = int1d % hasTimeDimension
                       attLists => int1d % attLists
                       dimNames(1:info%nDims) = int1d % dimNames;
                       block => int1d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                           ptr_i4_d1 = int1d % array(1:nCellsSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_i4_d1)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                           ptr_i4_d1 = int1d % array(1:nVerticesSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_i4_d1)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                           cycle FIELD_LOOP
                       end if

                   case (2)
                       call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)
                       hasTimeDimension = int2d % hasTimeDimension
                       attLists => int2d % attLists
                       dimNames(1:info%nDims) = int2d % dimNames;
                       block => int2d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(int2d%array,dim=1)], &
                                                    meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
                           ptr_i4_d2 = int2d%array(:,1:nCellsSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_i4_d2)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(int2d%array,dim=1)], &
                                                    meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
                           ptr_i4_d2 = int2d% array(:,1:nVerticesSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="restart_mpas", rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_i4_d2)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                           cycle FIELD_LOOP
                       end if

                   case (3)
                       call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)
                       hasTimeDimension = int3d % hasTimeDimension
                       attLists => int3d % attLists
                       dimNames(1:info%nDims) = int3d % dimNames;
                       block => int3d % block
               end select
           case (MPAS_POOL_CHARACTER)
               typeName = 'char'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                       hasTimeDimension = char0d % hasTimeDimension
                       attLists => char0d % attLists
                       ! no dimNames in 0d
                       nDims = nDims + 1
                       dimNames(nDims:nDims) = 'StrLen'
                       block => char0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_char, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                       hasTimeDimension = char1d % hasTimeDimension
                       attLists => char1d % attLists
                       dimNames(1:info%nDims) = char1d % dimNames;
                       nDims = nDims + 1
                       dimNames(nDims:nDims) = 'StrLen'
                       block => char1d % block
               end select
           end select

           if (.not. isVarArray) then

               if (hasTimeDimension) then
                   nDims = nDims + 1
                   dimNames(nDims:nDims) = 'Time'
               end if

               ! write(0,'(A,A,A,A,A)',advance='no') '   ',trim(typeName), ' ', trim(itr % memberName),'('
               do i = 1, nDims
                 call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

                 if (associated(dimSize_ptr)) then
                     dimSize = dimSize_ptr
                 else
                     dimSize = -1
                 end if

                 if (trim(dimNames(i)) == 'nCells') then
                     dimSize = nCellsGlobal
                 else if (trim(dimNames(i)) == 'nEdges') then
                     dimSize = nEdgesGlobal
                 else if (trim(dimNames(i)) == 'nVertices' ) then
                     dimSize = nVerticesGlobal
                 else if (trim(dimNames(i)) == 'StrLen' ) then
                     dimSize = 64
                 else if (trim(dimNames(i)) == 'Time' ) then
                     dimSize = 1
                 end if

                 ! write(0,'(A)',advance='no') trim(dimNames(i))
                 ! if (i < nDims) write(0,'(A)',advance='no') ', '

                 if (dimSize >= 0) then
                     is_unique = .true.
                     do j = 1, size(dim_info_arr)
                         if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                             if (dimSize /= dim_info_arr(j) % dimSize) then
                                write(0,*)'conflictiing dimSize for ', trim(itr % memberName), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
                                stop 1
                             end if
                             is_unique = .false.
                             exit
                         end if
                     end do
                     ! If unique, append to collection
                     if (is_unique) then
                         total_unique = size(dim_info_arr)
                         call resize_dim_info_array(dim_info_arr, total_unique + 1)
                         dim_info_arr(total_unique + 1) % dimName = trim(dimNames(i))
                         dim_info_arr(total_unique + 1) % dimSize = dimSize
                     end if
                 end if

               end do
               ! write(0,'(A)')') ;'

               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(itr % memberName), values=dimNames(1:nDims), rc=rc); ESMF_ERR(rc)

               numVars = numVars + 1
               variable_names(numVars) = trim(itr % memberName)


               call put_variable_attributes(attLists(1) % attList, trim(itr % memberName), rc=rc); ESMF_ERR(rc)
           end if ! .not. isVarArray

           nullify(attLists)

       else
           write(0,*)'Unknown memberType; memberName: ', trim(itr % memberName), ' memberType: ', itr % memberType
       end if

   end do FIELD_LOOP

   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variable_names', values=variable_names(1:numVars), rc=rc); ESMF_ERR(rc)

   ! dimensions attributes
   allocate(dimension_names(size(dim_info_arr)))
   do i = 1, size(dim_info_arr)
       ! write(0,*)trim(dim_info_arr(i) % dimName), ' ', dim_info_arr(i) % dimSize
       call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/dimensions/'//trim(dim_info_arr(i) % dimName), value=dim_info_arr(i) % dimSize, rc=rc); ESMF_ERR(rc)
       dimension_names(i)=trim(dim_info_arr(i) % dimName)
   end do
   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/dimension_names', values=dimension_names, rc=rc); ESMF_ERR(rc)

   !
   ! Write attributes to stream
   !
   call mpas_pool_begin_iteration(stream % att_pool)
   numAtts = 0
   do while (mpas_pool_get_next_member(stream % att_pool, itr))
       if ( itr % memberType == MPAS_POOL_CONFIG) then
          numAtts = numAtts + 1
       end if
   end do

   allocate(global_att_names(numAtts))
   global_att_names = ''
   numAtts = 0

   call mpas_pool_begin_iteration(stream % att_pool)
   do while (mpas_pool_get_next_member(stream % att_pool, itr))
       if ( itr % memberType == MPAS_POOL_CONFIG) then
          numAtts = numAtts + 1
           global_att_names(numAtts)=trim(itr % memberName)
           if ( itr % dataType == MPAS_POOL_REAL ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, realAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', realAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=realAtt, rc=rc); ESMF_ERR(rc)

           else if ( itr % dataType == MPAS_POOL_INTEGER ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, intAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', intAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=intAtt, rc=rc); ESMF_ERR(rc)

           else if ( itr % dataType == MPAS_POOL_CHARACTER ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, charAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', trim(charAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=trim(charAtt), rc=rc); ESMF_ERR(rc)
           else if ( itr % dataType == MPAS_POOL_LOGICAL ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, logAtt)
               if (logAtt) then
                   ! write(0,*)'config: ', trim(itr % memberName), ' YES'
                   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='YES', rc=rc); ESMF_ERR(rc)
               else
                   ! write(0,*)'config: ', trim(itr % memberName), ' NO'
                   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='NO', rc=rc); ESMF_ERR(rc)
               end if
           end if
       end if
   end do
   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att_names', values=global_att_names, rc=rc); ESMF_ERR(rc)

   ! bundle attributes
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3/grid_id", value=1, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3-nooutput/output_grid", value='restart_grid', rc=rc); ESMF_ERR(rc)

   call postwrite_reindex(allFields, stream % field_pool)

   contains

       subroutine add_field_to_bundle(varName, attList)

          character(len=*), intent(in) :: varName
          type(att_list_type), pointer, intent(in) :: attList

          integer :: i
          logical :: is_unique
          integer, pointer :: dimSize_ptr

          call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="atm", rc=rc); ESMF_ERR(rc)

          call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)

       end subroutine add_field_to_bundle

       subroutine resize_dim_info_array(arr, new_size)
             type(dim_info_t), allocatable, intent(inout) :: arr(:)
             integer, intent(in) :: new_size

             type(dim_info_t), allocatable :: temp(:)
             integer :: old_size, copy_size

             old_size = size(arr)
             allocate(temp(new_size))

             ! Copy existing elements
             copy_size = min(old_size, new_size)
             temp(1:copy_size) = arr(1:copy_size)

             ! Deallocate and reassign
             deallocate(arr)
             call move_alloc(temp, arr)
       end subroutine resize_dim_info_array


       subroutine put_variable_attributes(attList, varName, rc)

           type (att_list_type), pointer, intent(in) :: attList
           character(len=*), intent(in) :: varName
           integer, intent(out) :: rc

           character(len=64) :: att_names(256)
           integer :: numAtts
           type (att_list_type), pointer :: att_cursor => null()

           numAtts = 0
           att_names = ''
           att_cursor => attList
           do while (associated(att_cursor))
               if (trim(att_cursor % attName) /= '') then
                 if (att_cursor % attType == MPAS_ATT_INT) then
                    ! write (0,'(A,A,I0,A)') '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', att_cursor % attValueInt, '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=att_cursor % attValueInt, rc=rc); ESMF_ERR(rc)
                 else if (att_cursor % attType == MPAS_ATT_REAL) then
                    ! write (0,'(A,A,G0,A)') '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', att_cursor % attValueReal, '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=att_cursor % attValueReal, rc=rc); ESMF_ERR(rc)
                 else if (att_cursor % attType == MPAS_ATT_TEXT) then
                    ! write (0,'(A,A,A,A)' ) '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', trim(att_cursor % attValueText), '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=trim(att_cursor % attValueText), rc=rc); ESMF_ERR(rc)
                 else
                    ! write(0,*) i, '"'//trim(att_cursor % attName)//'" unknown type ', att_cursor % attType
                 end if
                 numAtts = numAtts + 1
                 att_names(numAtts) = trim(att_cursor % attName)
              end if
              att_cursor => att_cursor % next
           end do

           call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//'_att_names', values=att_names(1:numAtts), rc=rc); ESMF_ERR(rc)

           nullify(att_cursor)
       end subroutine put_variable_attributes

 end subroutine ufs_mpas_create_restart_bundle

    !-----------------------------------------------------------------------
    !  routine parse_package_list
    !
    !> \brief Parses a semi-colon-separated list of package names, indicating whether any are active
    !> \author Michael Duda
    !> \date   19 March 2015
    !> \details
    !>  This function determines whether any of the named strings in
    !>  the semi-colon-separated list provided in the 'packages' argument are
    !>  active.
    !>  If any of the packages does not exist in the package pool, the optional
    !>  argument ierr is set to a non-zero value; otherwise, if all packages exist,
    !>  ierr will be set to zero upon return.
    !
    !-----------------------------------------------------------------------
    logical function parse_package_list(package_pool, packages, ierr) result(active)

        use mpas_derived_types, only : MPAS_Pool_type, MPAS_POOL_SILENT
        use mpas_pool_routines, only : mpas_pool_get_package, mpas_pool_get_error_level, mpas_pool_set_error_level

        implicit none

        type (mpas_pool_type), intent(in) :: package_pool
        character (len=*), intent(in) :: packages
        integer, intent(out), optional :: ierr

        integer :: i, j, slen
        integer :: err_level
        logical, pointer :: pkg_val


        if (present(ierr)) ierr = 0

        slen = len_trim(packages)


        !
        ! No packages
        !
        if (slen == 0) then
            active = .true.
            return
        end if

        active = .false.

        err_level = mpas_pool_get_error_level()
        call mpas_pool_set_error_level(MPAS_POOL_SILENT)


        !
        ! Possible semi-colons in 'packages'
        !
        i = 1
        j = index(packages,';')
        do while (j >= i)
            if (j > i) then
                nullify(pkg_val)
                call mpas_pool_get_package(package_pool, packages(i:j-1)//'Active', pkg_val)
                if (associated(pkg_val)) then
                    if (pkg_val) then
                        active = .true.
                        call mpas_pool_set_error_level(err_level)
                        return
                    end if
                else
                    if (present(ierr)) ierr = 1
                end if
            end if
            i = j+1
            j = index(packages(i:slen),';') + i - 1
        end do


        !
        ! No more semi-colons to worry about
        !
        if (i < slen) then
            nullify(pkg_val)
            call mpas_pool_get_package(package_pool, packages(i:slen)//'Active', pkg_val)
            if (associated(pkg_val)) then
                if (pkg_val) then
                    active = .true.
                    call mpas_pool_set_error_level(err_level)
                    return
                end if
            else
                if (present(ierr)) ierr = 1
            end if
        end if

        call mpas_pool_set_error_level(err_level)

    end function parse_package_list


 subroutine ufs_mpas_get_esmf_mesh(mesh, rc)

   use mpas_derived_types, only : mpas_pool_type
   use mpas_pool_routines, only : mpas_pool_get_subpool, mpas_pool_get_dimension, mpas_pool_get_array
   use mpas_pool_routines, only : mpas_pool_get_config
   use mpas_dmpar,         only : mpas_dmpar_sum_int_array

   type(ESMF_Mesh), intent(out) :: mesh
   integer, intent(out) :: rc

   integer :: numNodes
   integer, dimension(:), allocatable :: nodeIds, nodeOwners
   real(8), dimension(:), allocatable :: nodeCoords

   integer :: numElems, numElemsConn, ielemConn
   integer, dimension(:), allocatable :: elemIds, elemTypes, elemConn, locNodeID
   real(8), dimension(:), allocatable :: elemCoords

   type (mpas_pool_type), pointer :: meshPool
   integer, dimension(:), pointer :: indexToCellID, indexToVertexID
   integer, dimension(:), pointer :: nEdgesOnCell
   real (kind=RKIND), dimension(:), pointer :: latCell, lonCell, latVertex, lonVertex
   integer, dimension(:,:), pointer :: cellsOnVertex, verticesOnCell
   integer, dimension(:), pointer :: nCellsOwnedIndices, nEdgesOwnedIndices, nVerticesOwnedIndices
   integer, pointer :: nCells, nVertices, vertexDegree
   integer :: i, j, localpet, nprocs, iloc, iv
   integer, dimension(:), pointer :: part_ids
   integer :: nVertex_on_pet
   character (len=StrKIND), pointer :: config_block_decomp_file_prefix
   character (len=StrKIND) :: fname

   integer, allocatable :: verticesOwnedByThisPET(:), new_verticesOwnedByThisPET(:)

   rc = 0

   localpet = domain_ptr % dminfo % my_proc_id
   nprocs = domain_ptr % dminfo % nprocs

   call mpas_pool_get_config(domain_ptr % blocklist % configs, 'config_block_decomp_file_prefix', config_block_decomp_file_prefix)
   write(fname,'(A,I0)') trim(config_block_decomp_file_prefix), nprocs

   allocate(part_ids(nCellsGlobal))
   open(unit=10,file=trim(fname), status='old', action='read')
   do i =1,nCellsGlobal
      read(10,*) part_ids(i)
   end do
   close(10)

   call mpas_pool_get_subpool(domain_ptr % blocklist % structs, 'mesh', meshPool)
   call mpas_pool_get_dimension(meshPool, 'nCells', nCells)
   call mpas_pool_get_dimension(meshPool, 'nVertices', nVertices)
   call mpas_pool_get_dimension(meshPool, 'vertexDegree', vertexDegree)
   call mpas_pool_get_array(meshPool, 'indexToCellID', indexToCellID)
   call mpas_pool_get_array(meshPool, 'indexToVertexID', indexToVertexID)
   call mpas_pool_get_array(meshPool, 'nEdgesOnCell', nEdgesOnCell)
   call mpas_pool_get_array(meshPool, 'latCell', latCell)
   call mpas_pool_get_array(meshPool, 'lonCell', lonCell)
   call mpas_pool_get_array(meshPool, 'latVertex', latVertex)
   call mpas_pool_get_array(meshPool, 'lonVertex', lonVertex)
   call mpas_pool_get_array(meshPool, 'cellsOnVertex', cellsOnVertex)
   call mpas_pool_get_array(meshPool, 'verticesOnCell', verticesOnCell)

   call mpas_pool_get_array(domain_ptr % blocklist % allfields, 'nCellsOwnedIndices', nCellsOwnedIndices)
   call mpas_pool_get_array(domain_ptr % blocklist % allfields, 'nEdgesOwnedIndices', nEdgesOwnedIndices)
   call mpas_pool_get_array(domain_ptr % blocklist % allfields, 'nVerticesOwnedIndices', nVerticesOwnedIndices)

   ! write(0,*) 'nCellsOwnedIndices    = ', size(nCellsOwnedIndices)
   ! write(0,*) 'nEdgesOwnedIndices    = ', size(nEdgesOwnedIndices)
   ! write(0,*) 'nVerticesOwnedIndices = ', size(nVerticesOwnedIndices)

   numElems = nCellsSolve
   allocate(elemIds(numElems))
   allocate(elemCoords(2*numElems))
   allocate(elemTypes(numElems))

   numElemsConn = 0
   do i = 1, nCellsSolve
      elemIds(i) = indexToCellID(i)
      elemCoords((i-1)*2+1) = lonCell(i)
      elemCoords((i-1)*2+2) = latCell(i)
      elemTypes(i) = nEdgesOnCell(i)
      numElemsConn = numElemsConn + nEdgesOnCell(i)
   end do

   allocate(verticesOwnedByThisPET(nVerticesGlobal))
   allocate(new_verticesOwnedByThisPET(nVerticesGlobal))
   verticesOwnedByThisPET = 0
   new_verticesOwnedByThisPET = 0
   do i = 1, nVerticesSolve
      verticesOwnedByThisPET(indexToVertexID(i)) = localpet + 1
   end do
   call mpas_dmpar_sum_int_array( domain_ptr % dminfo, nVerticesGlobal, verticesOwnedByThisPET, new_verticesOwnedByThisPET)
   verticesOwnedByThisPET = new_verticesOwnedByThisPET - 1

   nVertex_on_pet = 0
   do i = 1, nVertices
      do j = 1, vertexDegree
         if (indexToCellID(cellsOnVertex(j,i)) > 0) then
         if (localpet == part_ids(indexToCellID(cellsOnVertex(j,i)))) then
            nVertex_on_pet = nVertex_on_pet + 1
            go to 90
         end if
         end if
      end do
 90 continue
   end do
   ! write(0,*) 'nVertex_on_pet = ', nVertex_on_pet

   numNodes = nVertex_on_pet
   allocate(nodeIds(numNodes))
   allocate(nodeCoords(2*numNodes))
   allocate(nodeOwners(numNodes))
   allocate(locNodeID(nVerticesGlobal))
   locNodeID = 0
   iloc = 0
   do i =1,nVertices
      do j = 1, vertexDegree
         if (indexToCellID(cellsOnVertex(j,i)) > 0) then
         if (localpet == part_ids(indexToCellID(cellsOnVertex(j,i)))) then
            iloc = iloc + 1
            nodeIds(iloc) = indexToVertexID(i)
            nodeCoords((iloc-1)*2+1) = lonVertex(i)
            nodeCoords((iloc-1)*2+2) = latVertex(i)
            nodeOwners(iloc) = verticesOwnedByThisPET(indexToVertexID(i))
            ! nodeOwners(iloc) = maxval(part_ids(indexToCellID(cellsOnVertex(:,i))))
            locNodeID(indexToVertexID(i)) = iloc
            go to 92
         end if
         end if
     end do
 92 continue
   end do
   ASSERT( iloc == nVertex_on_pet )

   allocate(elemConn(numElemsConn))
   ielemConn = 0
   do i = 1, nCellsSolve
     do j = 1, nEdgesOnCell(i)
       ielemConn = ielemConn + 1
       ASSERT ( locNodeID(indexToVertexID(verticesOnCell(j,i))) > 0 )
       elemConn(ielemConn) = locNodeID(indexToVertexID(verticesOnCell(j,i)))
     end do
   end do
   ASSERT( ielemConn == numElemsConn )
   ! write(0,*) 'numElemsConn = ', numElemsConn

   mesh = ESMF_MeshCreate(parametricDim=2, &
                          spatialDim=2, &
                          coordSys=ESMF_COORDSYS_SPH_RAD, &
                          nodeIds=nodeIds, &
                          nodeCoords=nodeCoords, &
                          nodeOwners=nodeOwners, &
                          elementIds=elemIds, &
                          elementTypes=elemTypes, &
                          elementConn=elemConn, &
                          elementCoords=elemCoords, &
                          rc=rc); ESMF_ERR(rc)

 end subroutine ufs_mpas_get_esmf_mesh

 subroutine ufs_mpas_update_restart_bundle(output_bundle, rc)

   use mpas_derived_types, only : mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, &
                                  mpas_stream_list_type, mpas_pool_type, mpas_pool_iterator_type, &
                                  MPAS_POOL_CONFIG, MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, &
                                  field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                  field3DInteger, field2DInteger, field1DInteger, field0DInteger, field1DChar, field0DChar

   use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_subpool, &
                                  mpas_pool_get_next_member, mpas_pool_begin_iteration
   use mpas_stream_manager,only : prewrite_reindex, postwrite_reindex
   use mpas_stream_list,   only : MPAS_stream_list_query
   use mpas_log,           only : mpas_log_write

   type(ESMF_FieldBundle), intent(inout) :: output_bundle
   integer, intent(out) :: rc

   character(*), parameter :: subname = 'dyn_mpas_subdriver::ufs_mpas_update_restart_array_bundle'

   type(ESMF_Field), allocatable :: arrayList(:)
   type(ESMF_Field) :: field
   type(ESMF_Info) :: bundle_info

   character(64) :: field_name
   type(mpas_pool_field_info_type) :: mpas_pool_field_info
   type(mpaS_stream_list_type), pointer :: stream
   type(mpas_pool_iterator_type) :: itr

   real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:) ! Default MPAS real kind
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   integer :: i,j,k,n, ierr
   integer :: localpet
   integer :: dimCount, rank
   type(ESMF_TypeKind_Flag) :: typekind
   character(len=ESMF_MAXSTR) :: fieldName
   logical :: IsPresent
   type (mpas_pool_type), pointer :: statePool
   type (MPAS_Pool_type), pointer :: allFields
   type (MPAS_Pool_type), pointer :: allPackages
   integer :: timeLevelIn = 1

   type (mpas_pool_field_info_type) :: info
   integer :: timeLevel

   type (field5DReal), pointer :: real5d
   type (field4DReal), pointer :: real4d
   type (field3DReal), pointer :: real3d
   type (field2DReal), pointer :: real2d
   type (field1DReal), pointer :: real1d
   type (field0DReal), pointer :: real0d

   type (field3DInteger), pointer :: int3d
   type (field2DInteger), pointer :: int2d
   type (field1DInteger), pointer :: int1d
   type (field0DInteger), pointer :: int0d

   type (field1DChar), pointer :: char1d
   type (field0DChar), pointer :: char0d

   rc = 0

   ! Look at 'restart' stream
   nullify(stream)
   if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, 'restart', stream, ierr=ierr)) then
      rc = 1
      return
   endif

   allFields => domain_ptr % streamManager % allFields
   allPackages => domain_ptr % streamManager % allPackages

   call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

   call mpas_pool_begin_iteration(stream % field_pool)
   FIELD_LOOP: do while ( mpas_pool_get_next_member(stream % field_pool, itr) )

       if (itr % memberType == MPAS_POOL_CONFIG) then

           ! To avoid accidentally matching in case statements below...
           info % fieldType = -1

           call mpas_pool_get_field_info(allFields, itr % memberName, info)

           ! Set time level to read
           if (info % nTimeLevels >= timeLevelIn) then
               timeLevel = timeLevelIn
           else
               timeLevel = 1
           end if

           fieldName = trim(itr % memberName)

           select case (info % fieldType)
               case (MPAS_POOL_REAL)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, real0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=real0d % scalar, rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, real1d, timeLevel)
                           if (real1d % isDecomposed) then
                               call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
                               if (isPresent) then
                                   call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, field=field, rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldGet(field, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                                   if (trim(real1d % dimNames(1)) == 'nCells') then
                                      ptr_rm_d1 = real1d % array(1:nCellsSolve)
                                   else if (trim(real1d % dimNames(1)) == 'nVertices') then
                                      ptr_rm_d1 = real1d % array(1:nVerticesSolve)
                                   else if (trim(real1d % dimNames(1)) == 'nEdges') then
                                      ptr_rm_d1 = real1d % array(1:nEdgesSolve)
                                   end if
                               else
                                   write(0,*)'FIXME: ', trim(fieldName), ' is not present in the restart bundle'
                               end if
                           end if

                       case (2)
                           call mpas_pool_get_field(allFields, itr % memberName, real2d, timeLevel)
                           if (real2d % isDecomposed) then
                               call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
                               if (isPresent) then
                                   call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, field=field, rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                                   if (trim(real2d % dimNames(2)) == 'nCells') then
                                      ptr_rm_d2 = real2d % array(:,1:nCellsSolve)
                                   else if (trim(real2d % dimNames(2)) == 'nVertices') then
                                      ptr_rm_d2 = real2d % array(:,1:nVerticesSolve)
                                   else if (trim(real2d % dimNames(2)) == 'nEdges') then
                                      ptr_rm_d2 = real2d % array(:,1:nEdgesSolve)
                                   end if
                               else
                                   write(0,*)'FIXME: ', trim(fieldName), ' is not present in the restart bundle'
                               end if
                           end if

                       case (3)
                           call mpas_pool_get_field(allFields, itr % memberName, real3d, timeLevel)

                           if (real3d % isDecomposed) then
                               if (real3d % isVarArray) then
                                   do k = 1, size(real3d % constituentNames)
                                       fieldName = trim(real3d % constituentNames(k))
                                       call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
                                       if (isPresent) then
                                           call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, field=field, rc=rc); ESMF_ERR(rc)
                                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                                           if (trim(real3d % dimNames(3)) == 'nCells') then
                                              ptr_rm_d2 = real3d % array(k,:,1:nCellsSolve)
                                           else if (trim(real3d % dimNames(3)) == 'nVertices') then
                                              ptr_rm_d2 = real3d % array(k,:,1:nVerticesSolve)
                                           else if (trim(real3d % dimNames(3)) == 'nEdges') then
                                              ptr_rm_d2 = real3d % array(k,:,1:nEdgesSolve)
                                           end if
                                       else
                                           write(0,*)'FIXME: ', trim(fieldName), ' is not present in the restart bundle'
                                       end if
                                   end do
                               else
                                   call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
                                   if (isPresent) then
                                       call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, field=field, rc=rc); ESMF_ERR(rc)
                                       call ESMF_FieldGet(field, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                                       if (trim(real3d % dimNames(3)) == 'nCells') then
                                          ptr_rm_d3 = real3d % array(:,:,1:nCellsSolve)
                                       else if (trim(real3d % dimNames(3)) == 'nVertices') then
                                          ptr_rm_d3 = real3d % array(:,:,1:nVerticesSolve)
                                       else if (trim(real3d % dimNames(3)) == 'nEdges') then
                                          ptr_rm_d3 = real3d % array(:,:,1:nEdgesSolve)
                                       end if
                                   else
                                       write(0,*)'FIXME: ', trim(fieldName), ' is not present in the restart bundle'
                                   end if
                               end if
                           end if
                       case (4)
                           call mpas_pool_get_field(allFields, itr % memberName, real4d, timeLevel)
                           write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

                       case (5)
                           call mpas_pool_get_field(allFields, itr % memberName, real5d, timeLevel)
                           write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'
                   end select

               case (MPAS_POOL_INTEGER)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, int0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=int0d % scalar, rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, int1d, timeLevel)

                           if (int1d % isDecomposed) then
                               call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, isPresent=isPresent, rc=rc); ESMF_ERR(rc)
                               if (isPresent) then
                                   call ESMF_FieldBundleGet(output_bundle, fieldName=fieldName, field=field, rc=rc); ESMF_ERR(rc)
                                   call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                                   if (trim(int1d % dimNames(1)) == 'nCells') then
                                      ptr_i4_d1 = int1d % array(1:nCellsSolve)
                                   else if (trim(int1d % dimNames(1)) == 'nVertices') then
                                      ptr_i4_d1 = int1d % array(1:nVerticesSolve)
                                   else if (trim(int1d % dimNames(1)) == 'nEdges') then
                                      ptr_i4_d1 = int1d % array(1:nEdgesSolve)
                                   end if
                               else
                                   write(0,*)'FIXME: ', trim(fieldName), ' is not present in the restart bundle'
                               end if
                           else ! Array has no distributed dimension
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=int1d % array, rc=rc); ESMF_ERR(rc)
                           end if

                       case (2)
                           call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)
                           write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

                       case (3)
                           call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)
                           write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

                   end select

               case (MPAS_POOL_CHARACTER)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                           write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'
                   end select
           end select

       end if

   end do FIELD_LOOP

   call postwrite_reindex(allFields, stream % field_pool)

 end subroutine ufs_mpas_update_restart_bundle

 subroutine ufs_mpas_create_restart_array_bundle(output_bundle, bundle_name, stream_name, rc)

   use mpas_derived_types, only : MPAS_stream_list_type, field_list_type
   use mpas_stream_list,   only : MPAS_stream_list_query
   use mpas_log,           only : mpas_log_write
   use mpas_pool_routines, only : pool_print_members


   use mpas_attlist,       only : att_list_type, att_lists_type, &
                                  MPAS_ATT_INT, MPAS_ATT_INTA, MPAS_ATT_REAL,MPAS_ATT_REALA, MPAS_ATT_TEXT
   use mpas_derived_types, only : MPAS_stream_list_type, MPAS_Pool_type, MPAS_Pool_iterator_type, mpas_pool_field_info_type, block_type,  &
                                  field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                  field3DInteger, field2DInteger, field1DInteger, field0DInteger, &
                                  field1DChar, field0DChar, &
                                  MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, MPAS_POOL_LOGICAL, &
                                  MPAS_POOL_SILENT, MPAS_POOL_CONFIG
   use mpas_pool_routines, only : mpas_pool_get_next_member, mpas_pool_get_config, mpas_pool_get_error_level, &
                                  mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_set_error_level, &
                                  mpas_pool_begin_iteration, mpas_pool_get_dimension,mpas_pool_get_subpool
   use mpas_stream_manager, only : prewrite_reindex, postwrite_reindex

   implicit none

   type (ESMF_ArrayBundle), intent(out) :: output_bundle
   character(len=*), intent(in) :: bundle_name
   character(len=*), intent(in) :: stream_name
   integer, intent(out) :: rc

   ! Local
   type (MPAS_stream_list_type), pointer :: stream
   type (MPAS_Pool_type), pointer :: allFields
   type (MPAS_Pool_type), pointer :: allPackages
   integer :: timeLevelIn = 1

   type (ESMF_Array) :: array
   type (ESMF_Info) :: bundle_info
   type (ESMF_TypeKind_Flag) :: rkind_typekind ! Default ESMF real typekind that corresponds to default MPAS real kind

   real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:) ! Default MPAS real kind
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   type (MPAS_Pool_iterator_type) :: itr
   type (mpas_pool_field_info_type) :: info
   integer :: timeLevel

   type (field5DReal), pointer :: real5d
   type (field4DReal), pointer :: real4d
   type (field3DReal), pointer :: real3d
   type (field2DReal), pointer :: real2d
   type (field1DReal), pointer :: real1d
   type (field0DReal), pointer :: real0d

   type (field3DInteger), pointer :: int3d
   type (field2DInteger), pointer :: int2d
   type (field1DInteger), pointer :: int1d
   type (field0DInteger), pointer :: int0d

   type (field1DChar), pointer :: char1d
   type (field0DChar), pointer :: char0d

   integer, pointer :: intAtt
   logical, pointer :: logAtt
   character (len=StrKIND), pointer :: charAtt
   real (kind=RKIND), pointer :: realAtt

   character (len=StrKIND), pointer :: packages
   logical :: active_field
   integer :: err_level

   type (block_type), pointer :: block => null()
   character (len=StrKIND), dimension(5+1) :: dimNames ! +1 for Time
   type (att_lists_type), dimension(:), pointer :: attLists => null()
   integer :: i, j, k, total_unique, nDims, dimSize
   integer, pointer :: dimSize_ptr
   logical :: hasTimeDimension = .false.
   logical :: is_unique
   logical :: isVarArray
   character (len=8) :: typeName
   integer :: frestart(999) = -1
   character(len=64) :: decomp_dim_name
   character(len=64), allocatable :: dimension_names(:)
   character(len=64) :: variable_names(3000)
   character(len=256), allocatable :: global_att_names(:)
   integer :: numVars
   integer :: numGlobalAtts

   type :: dim_info_t
     character(64) :: dimName
     integer :: dimSize
   end type

   type (dim_info_t), allocatable :: dim_info_arr(:)

   character(*), parameter :: subname = 'dyn_mpas_subdriver::ufs_mpas_create_restart_array_bundle'

   type (field_list_type), pointer :: fieldList, field_cursor
   integer :: ierr
   integer :: nprocs, localpet, minIndexPTileCells, maxIndexPTileCells, dimCount, deCount
   TYPE(mpi_comm) :: comm
   integer, allocatable :: cell_counts(:)
   integer, allocatable :: deBlockList(:,:,:)
   type (ESMF_DistGrid) :: distgridCells, distgridVertices, distgridEdges
   integer :: nSolve, nGlobal

   type (mpas_pool_type), pointer :: statePool

   rc = 0

   ! Look at 'restart' stream
   nullify(stream)
   if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, trim(stream_name), stream, ierr=ierr)) then
      write(0,*)'ERROR, Unknown stream name: ', trim(stream_name)
      rc = 1
      return
   endif

   allFields => domain_ptr % streamManager % allFields
   allPackages => domain_ptr % streamManager % allPackages

   if (RKIND == R4KIND) then
       rkind_typekind = ESMF_TYPEKIND_R4
   else if (RKIND == R8KIND) then
       rkind_typekind = ESMF_TYPEKIND_R8
   else
       write(0,*)'Unrecognized RKIND'
       rc = 1
       return
   end if

   call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

   localpet = domain_ptr % dminfo % my_proc_id
   nprocs = domain_ptr % dminfo % nprocs
   comm = domain_ptr % dminfo % comm

   allocate(cell_counts(nprocs))
   dimCount = 1
   deCount = nprocs
   allocate(deBlockList(dimCount, 2, deCount)) ! (dimCount, 2, deCount)

   ! Cells
   nSolve = nCellsSolve
   nGlobal = nCellsGlobal

   call MPI_AllGather(nSolve, 1, MPI_INTEGER, cell_counts, 1, MPI_INTEGER, comm, ierr)

   do j = 1, deCount
      minIndexPTileCells =0
      do i=1,j-1
         minIndexPTileCells = minIndexPTileCells + cell_counts(i)
      end do
      minIndexPTileCells = minIndexPTileCells + 1
      maxIndexPTileCells = minIndexPTileCells + cell_counts(j) - 1
      deBlockList(1,1,j) = minIndexPTileCells
      deBlockList(1,2,j) = maxIndexPTileCells
   end do

   distgridCells = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/nGlobal/), &
                                       deBlockList=deBlockList, rc=rc); ESMF_ERR(rc)

   ! Vertices
   nSolve = nVerticesSolve
   nGlobal = nVerticesGlobal

   call MPI_AllGather(nSolve, 1, MPI_INTEGER, cell_counts, 1, MPI_INTEGER, comm, ierr)

   do j = 1, deCount
      minIndexPTileCells =0
      do i=1,j-1
         minIndexPTileCells = minIndexPTileCells + cell_counts(i)
      end do
      minIndexPTileCells = minIndexPTileCells + 1
      maxIndexPTileCells = minIndexPTileCells + cell_counts(j) - 1
      deBlockList(1,1,j) = minIndexPTileCells
      deBlockList(1,2,j) = maxIndexPTileCells
   end do

   distgridVertices = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/nGlobal/), &
                                       deBlockList=deBlockList, rc=rc); ESMF_ERR(rc)

   ! Edges
   nSolve = nEdgesSolve
   nGlobal = nEdgesGlobal

   call MPI_AllGather(nSolve, 1, MPI_INTEGER, cell_counts, 1, MPI_INTEGER, comm, ierr)

   do j = 1, deCount
      minIndexPTileCells =0
      do i=1,j-1
         minIndexPTileCells = minIndexPTileCells + cell_counts(i)
      end do
      minIndexPTileCells = minIndexPTileCells + 1
      maxIndexPTileCells = minIndexPTileCells + cell_counts(j) - 1
      deBlockList(1,1,j) = minIndexPTileCells
      deBlockList(1,2,j) = maxIndexPTileCells
   end do

   distgridEdges = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/nGlobal/), &
                                       deBlockList=deBlockList, rc=rc); ESMF_ERR(rc)

   output_bundle = ESMF_ArrayBundleCreate(name=trim(bundle_name), rc=rc); ESMF_ERR(rc)

   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

   allocate(dim_info_arr(0))

   variable_names = ''
   numVars = 0

   call mpas_pool_begin_iteration(stream % field_pool)
   FIELD_LOOP: do while ( mpas_pool_get_next_member(stream % field_pool, itr) )

       if (itr % memberType == MPAS_POOL_CONFIG) then

           err_level = mpas_pool_get_error_level()
           call mpas_pool_set_error_level(MPAS_POOL_SILENT)

           nullify(packages)
           call mpas_pool_get_config(stream % field_pkg_pool, trim(itr % memberName)//':packages', packages)
           if (associated(packages)) then
               active_field = parse_package_list(allPackages, trim(packages))
           else
               active_field = .true.
           end if
           call mpas_pool_set_error_level(err_level)

           if (.not. active_field) cycle FIELD_LOOP

           ! To avoid accidentally matching in case statements below...
           info % fieldType = -1

           call mpas_pool_get_field_info(allFields, itr % memberName, info)

           ! if (trim(itr % memberName) == 'scalars') cycle FIELD_LOOP

           ! Set time level to read
           if (info % nTimeLevels >= timeLevelIn) then
               timeLevel = timeLevelIn
           else
               timeLevel = 1
           end if

           hasTimeDimension = .false.
           attLists => null()
           dimNames = ''
           nDims = info % nDims
           isVarArray = .false.

           select case (info % fieldType)
           case (MPAS_POOL_REAL)
               typeName = 'float'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, real0d, timeLevel)
                       hasTimeDimension = real0d % hasTimeDimension
                       attLists => real0d % attLists
                       ! no dimNames in 0d
                       block => real0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=real0d % scalar, rc=rc); ESMF_ERR(rc)
                       if (RKIND == R4KIND) then
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_real, rc=rc); ESMF_ERR(rc)
                       else if (RKIND == R8KIND) then
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_double, rc=rc); ESMF_ERR(rc)
                       end if
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, real1d, timeLevel)
                       hasTimeDimension = real1d % hasTimeDimension
                       attLists => real1d % attLists
                       dimNames(1:info%nDims) = real1d % dimNames;
                       block => real1d % block

                       if (trim(dimNames(1)) == 'nCells') then
                           array = ESMF_ArrayCreate(distgridCells, rkind_typekind, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d1 = real1d % array(1:nCellsSolve)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d1)
                       else if (trim(dimNames(1)) == 'nVertices') then
                           array = ESMF_ArrayCreate(distgridVertices, rkind_typekind, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d1 = real1d % array(1:nVerticesSolve)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d1)
                       else if (trim(dimNames(1)) == 'nEdges') then
                           array = ESMF_ArrayCreate(distgridEdges, rkind_typekind, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d1 = real1d % array(1:nEdgesSolve)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d1)
                       else ! Array has no distributed dimension
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=real1d % array, rc=rc); ESMF_ERR(rc)
                           if (RKIND == R4KIND) then
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_real, rc=rc); ESMF_ERR(rc)
                           else if (RKIND == R8KIND) then
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_double, rc=rc); ESMF_ERR(rc)
                           end if
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                       end if

                   case (2)
                       call mpas_pool_get_field(allFields, itr % memberName, real2d, timeLevel)
                       hasTimeDimension = real2d % hasTimeDimension
                       attLists => real2d % attLists
                       dimNames(1:info%nDims) = real2d % dimNames;
                       block => real2d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           array = ESMF_ArrayCreate(distgridCells, farray=real2d % array(:,1:nCellsSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           array = ESMF_ArrayCreate(distgridVertices, farray=real2d % array(:,1:nVerticesSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           array = ESMF_ArrayCreate(distgridEdges, farray=real2d % array(:,1:nEdgesSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                       end if

                   case (3)
                       call mpas_pool_get_field(allFields, itr % memberName, real3d, timeLevel)
                       hasTimeDimension = real3d % hasTimeDimension
                       attLists => real3d % attLists
                       block => real3d % block
                       decomp_dim_name = trim(real3d % dimNames(info%nDims)) ! save decomp name before stripping num_scalar

                       if (real3d % isVarArray) then
                           nDims = nDims - 1
                           dimNames(1:nDims) = real3d % dimNames(2:info % nDims)   ! strip first dimension (num_scalar)
                           isVarArray = .true.

                           if (hasTimeDimension) then
                               nDims = nDims + 1
                               dimNames(nDims:nDims) = 'Time'
                           end if

                           do k = 1, size(real3d % constituentNames)
                               if (trim(decomp_dim_name) == 'nCells') then
                                   array = ESMF_ArrayCreate(distgridCells, farray=real3d % array(k,:,1:nCellsSolve), &
                                                            distgridToArrayMap = (/2/), &
                                                            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                            name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                                   call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                               else if (trim(decomp_dim_name) == 'nVertices') then
                                   array = ESMF_ArrayCreate(distgridVertices, farray=real3d % array(k,:,1:nVerticesSolve), &
                                                            distgridToArrayMap = (/2/), &
                                                            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                            name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                                   call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                               else if (trim(decomp_dim_name) == 'nEdges') then
                                   array = ESMF_ArrayCreate(distgridEdges, farray=real3d % array(k,:,1:nEdgesSolve), &
                                                            distgridToArrayMap = (/2/), &
                                                            indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                            name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                                   call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                               end if

                               ! write(0,'(A,A,A,A,A)',advance='no') '   ',trim(typeName), ' ', trim(real3d % constituentNames(k)),'('
                               do i = 1, nDims
                                 call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

                                 if (associated(dimSize_ptr)) then
                                     dimSize = dimSize_ptr
                                 else
                                     dimSize = -1
                                 end if

                                 if (trim(dimNames(i)) == 'nCells') then
                                     dimSize = nCellsGlobal
                                 else if (trim(dimNames(i)) == 'nEdges') then
                                     dimSize = nEdgesGlobal
                                 else if (trim(dimNames(i)) == 'nVertices' ) then
                                     dimSize = nVerticesGlobal
                                 else if (trim(dimNames(i)) == 'StrLen' ) then
                                     dimSize = 64
                                 else if (trim(dimNames(i)) == 'Time' ) then
                                     dimSize = 1
                                 end if

                                 ! write(0,'(A)',advance='no') trim(dimNames(i))
                                 ! if (i < nDims) write(0,'(A)',advance='no') ', '

                                 if (dimSize >= 0) then
                                     is_unique = .true.
                                     do j = 1, size(dim_info_arr)
                                         if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                                             if (dimSize /= dim_info_arr(j) % dimSize) then
                                                write(0,*)'conflictiing dimSize for ', trim(real3d % constituentNames(k)), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
                                                stop 1
                                             end if
                                             is_unique = .false.
                                             exit
                                         end if
                                     end do
                                     ! If unique, append to collection
                                     if (is_unique) then
                                         total_unique = size(dim_info_arr)
                                         call resize_dim_info_array(dim_info_arr, total_unique + 1)
                                         dim_info_arr(total_unique + 1) % dimName = trim(dimNames(i))
                                         dim_info_arr(total_unique + 1) % dimSize = dimSize
                                     end if
                                 end if

                               end do
                               ! write(0,'(A)')') ;'

                               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(real3d % constituentNames(k)), values=dimNames(1:nDims), rc=rc); ESMF_ERR(rc)

                               numVars = numVars + 1
                               variable_names(numVars) = trim(real3d % constituentNames(k))

                               call put_variable_attributes(attLists(k) % attList, trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                           end do
                       else

                           dimNames(1:info%nDims) = real3d % dimNames;
                           if (trim(dimNames(info%nDims)) == 'nCells') then
                               array = ESMF_ArrayCreate(distgridCells, farray=real3d % array(:,:,1:nCellsSolve), &
                                                        distgridToArrayMap = (/3/), &
                                                        indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                        name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           else if (trim(dimNames(info%nDims)) == 'nVertices') then
                               array = ESMF_ArrayCreate(distgridVertices, farray=real3d % array(:,:,1:nVerticesSolve), &
                                                        distgridToArrayMap = (/3/), &
                                                        indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                        name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           else if (trim(dimNames(info%nDims)) == 'nEdges') then
                               array = ESMF_ArrayCreate(distgridEdges, farray=real3d % array(:,:,1:nEdgesSolve), &
                                                        distgridToArrayMap = (/3/), &
                                                        indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                        name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                           end if
                       end if

                   case (4)
                       call mpas_pool_get_field(allFields, itr % memberName, real4d, timeLevel)
                       hasTimeDimension = real4d % hasTimeDimension
                       attLists => real4d % attLists
                       dimNames(1:info%nDims) = real4d % dimNames;
                       block => real4d % block
                   case (5)
                       call mpas_pool_get_field(allFields, itr % memberName, real5d, timeLevel)
                       hasTimeDimension = real5d % hasTimeDimension
                       attLists => real5d % attLists
                       dimNames(1:info%nDims) = real5d % dimNames;
                       block => real5d % block
               end select

           case (MPAS_POOL_INTEGER)
               typeName = 'int'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, int0d, timeLevel)
                       hasTimeDimension = int0d % hasTimeDimension
                       attLists => int0d % attLists
                       ! no dimNames in 0d
                       block => int0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=int0d % scalar, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_int, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, int1d, timeLevel)
                       hasTimeDimension = int1d % hasTimeDimension
                       attLists => int1d % attLists
                       dimNames(1:info%nDims) = int1d % dimNames;
                       block => int1d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           array = ESMF_ArrayCreate(distgridCells, farray=int1d % array(1:nCellsSolve), &
                                                    distgridToArrayMap = (/1/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)

                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           array = ESMF_ArrayCreate(distgridVertices, farray=int1d % array(1:nVerticesSolve), &
                                                    distgridToArrayMap = (/1/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)

                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           array = ESMF_ArrayCreate(distgridEdges, farray=int1d % array(1:nEdgesSolve), &
                                                    distgridToArrayMap = (/1/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)

                       else ! Array has no distributed dimension
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=int1d % array, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_int, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                       end if

                   case (2)
                       call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)
                       hasTimeDimension = int2d % hasTimeDimension
                       attLists => int2d % attLists
                       dimNames(1:info%nDims) = int2d % dimNames;
                       block => int2d % block

                       if (trim(dimNames(info%nDims)) == 'nCells') then
                           array = ESMF_ArrayCreate(distgridCells, farray=int2d % array(:,1:nCellsSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                       else if (trim(dimNames(info%nDims)) == 'nVertices') then
                           array = ESMF_ArrayCreate(distgridVertices, farray=int2d % array(:,1:nVerticesSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                       else if (trim(dimNames(info%nDims)) == 'nEdges') then
                           array = ESMF_ArrayCreate(distgridEdges, farray=int2d % array(:,1:nEdgesSolve), &
                                                    distgridToArrayMap = (/2/), &
                                                    indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                    name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                           call ESMF_ArrayBundleAdd(output_bundle,(/array/), rc=rc); ESMF_ERR(rc)
                      end if

                   case (3)
                       call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)
                       hasTimeDimension = int3d % hasTimeDimension
                       attLists => int3d % attLists
                       dimNames(1:info%nDims) = int3d % dimNames;
                       block => int3d % block
               end select

           case (MPAS_POOL_CHARACTER)
               typeName = 'char'
               select case (info % nDims)
                   case (0)
                       call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                       hasTimeDimension = char0d % hasTimeDimension
                       attLists => char0d % attLists
                       ! no dimNames in 0d
                       nDims = nDims + 1
                       dimNames(nDims:nDims) = 'StrLen'
                       block => char0d % block

                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value=PIO_char, rc=rc); ESMF_ERR(rc)
                       call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

                   case (1)
                       call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                       hasTimeDimension = char1d % hasTimeDimension
                       attLists => char1d % attLists
                       dimNames(1:info%nDims) = char1d % dimNames;
                       nDims = nDims + 1
                       dimNames(nDims:nDims) = 'StrLen'
                       block => char1d % block
               end select
           end select

           if (.not. isVarArray) then

               if (hasTimeDimension) then
                   nDims = nDims + 1
                   dimNames(nDims:nDims) = 'Time'
               end if

               ! write(0,'(A,A,A,A,A)',advance='no') '   ',trim(typeName), ' ', trim(itr % memberName),'('
               do i = 1, nDims
                 call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

                 if (associated(dimSize_ptr)) then
                     dimSize = dimSize_ptr
                 else
                     dimSize = -1
                 end if

                 if (trim(dimNames(i)) == 'nCells') then
                     dimSize = nCellsGlobal
                 else if (trim(dimNames(i)) == 'nEdges') then
                     dimSize = nEdgesGlobal
                 else if (trim(dimNames(i)) == 'nVertices' ) then
                     dimSize = nVerticesGlobal
                 else if (trim(dimNames(i)) == 'StrLen' ) then
                     dimSize = 64
                 else if (trim(dimNames(i)) == 'Time' ) then
                     dimSize = 1
                 end if

                 ! write(0,'(A)',advance='no') trim(dimNames(i))
                 ! if (i < nDims) write(0,'(A)',advance='no') ', '

                 if (dimSize >= 0) then
                     is_unique = .true.
                     do j = 1, size(dim_info_arr)
                         if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                             if (dimSize /= dim_info_arr(j) % dimSize) then
                                write(0,*)'conflictiing dimSize for ', trim(itr % memberName), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
                                stop 1
                             end if
                             is_unique = .false.
                             exit
                         end if
                     end do
                     ! If unique, append to collection
                     if (is_unique) then
                         total_unique = size(dim_info_arr)
                         call resize_dim_info_array(dim_info_arr, total_unique + 1)
                         dim_info_arr(total_unique + 1) % dimName = trim(dimNames(i))
                         dim_info_arr(total_unique + 1) % dimSize = dimSize
                     end if
                 end if

               end do
               ! write(0,'(A)')') ;'

               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(itr % memberName), values=dimNames(1:nDims), rc=rc); ESMF_ERR(rc)

               numVars = numVars + 1
               variable_names(numVars) = trim(itr % memberName)

               call put_variable_attributes(attLists(1) % attList, trim(itr % memberName), rc=rc); ESMF_ERR(rc)
           end if ! .not. isVarArray

           nullify(attLists)

       end if

   end do FIELD_LOOP
   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variable_names', values=variable_names(1:numVars), rc=rc); ESMF_ERR(rc)

   ! dimensions attributes
   allocate(dimension_names(size(dim_info_arr)))
   do i = 1, size(dim_info_arr)
       ! write(0,*)trim(dim_info_arr(i) % dimName), ' ', dim_info_arr(i) % dimSize
       call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/dimensions/'//trim(dim_info_arr(i) % dimName), value=dim_info_arr(i) % dimSize, rc=rc); ESMF_ERR(rc)
       dimension_names(i)=trim(dim_info_arr(i) % dimName)
   end do
   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/dimension_names', values=dimension_names, rc=rc); ESMF_ERR(rc)

   !
   ! Write attributes to stream
   !
   call mpas_pool_begin_iteration(stream % att_pool)
   numGlobalAtts = 0
   do while (mpas_pool_get_next_member(stream % att_pool, itr))
       if ( itr % memberType == MPAS_POOL_CONFIG) then
          numGlobalAtts = numGlobalAtts + 1
       end if
   end do

   allocate(global_att_names(numGlobalAtts))
   global_att_names = ''
   numGlobalAtts = 0

   call mpas_pool_begin_iteration(stream % att_pool)
   do while (mpas_pool_get_next_member(stream % att_pool, itr))
       if ( itr % memberType == MPAS_POOL_CONFIG) then
          numGlobalAtts = numGlobalAtts + 1
           global_att_names(numGlobalAtts)=trim(itr % memberName)
           if ( itr % dataType == MPAS_POOL_REAL ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, realAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', realAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=realAtt, rc=rc); ESMF_ERR(rc)

           else if ( itr % dataType == MPAS_POOL_INTEGER ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, intAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', intAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=intAtt, rc=rc); ESMF_ERR(rc)

           else if ( itr % dataType == MPAS_POOL_CHARACTER ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, charAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', trim(charAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=trim(charAtt), rc=rc); ESMF_ERR(rc)
           else if ( itr % dataType == MPAS_POOL_LOGICAL ) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, logAtt)
               if (logAtt) then
                   ! write(0,*)'config: ', trim(itr % memberName), ' YES'
                   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='YES', rc=rc); ESMF_ERR(rc)
               else
                   ! write(0,*)'config: ', trim(itr % memberName), ' NO'
                   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='NO', rc=rc); ESMF_ERR(rc)
               end if
           end if
       end if
   end do
   call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att_names', values=global_att_names, rc=rc); ESMF_ERR(rc)

   ! bundle attributes
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3/grid_id", value=1, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3-nooutput/output_grid", value='restart_grid', rc=rc); ESMF_ERR(rc)

   call postwrite_reindex(allFields, stream % field_pool)

   contains

       subroutine resize_dim_info_array(arr, new_size)
             type(dim_info_t), allocatable, intent(inout) :: arr(:)
             integer, intent(in) :: new_size

             type(dim_info_t), allocatable :: temp(:)
             integer :: old_size, copy_size

             old_size = size(arr)
             allocate(temp(new_size))

             ! Copy existing elements
             copy_size = min(old_size, new_size)
             temp(1:copy_size) = arr(1:copy_size)

             ! Deallocate and reassign
             deallocate(arr)
             call move_alloc(temp, arr)
       end subroutine resize_dim_info_array


       subroutine put_variable_attributes(attList, varName, rc)

           type (att_list_type), pointer, intent(in) :: attList
           character(len=*), intent(in) :: varName
           integer, intent(out) :: rc

           character(len=64) :: att_names(256)
           integer :: numAtts
           type (att_list_type), pointer :: att_cursor => null()

           numAtts = 0
           att_names = ''
           att_cursor => attList
           do while (associated(att_cursor))
               if (trim(att_cursor % attName) /= '') then
                 if (att_cursor % attType == MPAS_ATT_INT) then
                    ! write (0,'(A,A,I0,A)') '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', att_cursor % attValueInt, '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=att_cursor % attValueInt, rc=rc); ESMF_ERR(rc)
                 else if (att_cursor % attType == MPAS_ATT_REAL) then
                    ! write (0,'(A,A,G0,A)') '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', att_cursor % attValueReal, '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=att_cursor % attValueReal, rc=rc); ESMF_ERR(rc)
                 else if (att_cursor % attType == MPAS_ATT_TEXT) then
                    ! write (0,'(A,A,A,A)' ) '       ',trim(varName)//':'//trim(att_cursor % attName)//' = "', trim(att_cursor % attValueText), '" ;'
                    call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//trim(att_cursor % attName), value=trim(att_cursor % attValueText), rc=rc); ESMF_ERR(rc)
                 else
                    ! write(0,*) i, '"'//trim(att_cursor % attName)//'" unknown type ', att_cursor % attType
                 end if
                 numAtts = numAtts + 1
                 att_names(numAtts) = trim(att_cursor % attName)
              end if
              att_cursor => att_cursor % next
           end do

           call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//'_att_names', values=att_names(1:numAtts), rc=rc); ESMF_ERR(rc)

           nullify(att_cursor)
       end subroutine put_variable_attributes

 end subroutine ufs_mpas_create_restart_array_bundle

 subroutine ufs_mpas_update_restart_array_bundle(output_bundle, stream_name, rc)

   use mpas_derived_types, only : mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, &
                                  mpas_stream_list_type, mpas_pool_type, mpas_pool_iterator_type, &
                                  MPAS_POOL_CONFIG, MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, &
                                  field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                  field3DInteger, field2DInteger, field1DInteger, field0DInteger, field1DChar, field0DChar

   use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_subpool, &
                                  mpas_pool_get_next_member, mpas_pool_begin_iteration
   use mpas_stream_manager,only : prewrite_reindex, postwrite_reindex
   use mpas_stream_list,   only : MPAS_stream_list_query
   use mpas_log,           only : mpas_log_write

   type(ESMF_ArrayBundle), intent(inout) :: output_bundle
   character(len=*), intent(in) :: stream_name
   integer, intent(out) :: rc

   character(*), parameter :: subname = 'dyn_mpas_subdriver::ufs_mpas_update_restart_array_bundle'

   type(ESMF_Array), allocatable :: arrayList(:)
   type(ESMF_Array) :: array
   type (ESMF_Info) :: bundle_info

   character(64) :: field_name
   type(mpas_pool_field_info_type) :: mpas_pool_field_info
   type(MPAS_stream_list_type), pointer :: stream
   type(mpas_pool_iterator_type) :: itr

   real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:) ! Default MPAS real kind
   integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:), ptr_i4_d3(:,:,:)

   integer :: i,j,k,n, ierr
   integer :: localpet
   integer :: dimCount, rank
   type(ESMF_TypeKind_Flag) :: typekind
   character(len=ESMF_MAXSTR) :: arrayName
   type (mpas_pool_type), pointer :: statePool
   type (MPAS_Pool_type), pointer :: allFields
   type (MPAS_Pool_type), pointer :: allPackages
   integer :: timeLevelIn = 1

   type (mpas_pool_field_info_type) :: info
   integer :: timeLevel

   type (field5DReal), pointer :: real5d
   type (field4DReal), pointer :: real4d
   type (field3DReal), pointer :: real3d
   type (field2DReal), pointer :: real2d
   type (field1DReal), pointer :: real1d
   type (field0DReal), pointer :: real0d

   type (field3DInteger), pointer :: int3d
   type (field2DInteger), pointer :: int2d
   type (field1DInteger), pointer :: int1d
   type (field0DInteger), pointer :: int0d

   type (field1DChar), pointer :: char1d
   type (field0DChar), pointer :: char0d

   rc = 0

   ! Look at 'restart' stream
   nullify(stream)
   if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, trim(stream_name), stream, ierr=ierr)) then
      rc = 1
      return
   endif

   allFields => domain_ptr % streamManager % allFields
   allPackages => domain_ptr % streamManager % allPackages

   call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

   call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

   call mpas_pool_begin_iteration(stream % field_pool)
   FIELD_LOOP: do while ( mpas_pool_get_next_member(stream % field_pool, itr) )


       if (itr % memberType == MPAS_POOL_CONFIG) then

           ! To avoid accidentally matching in case statements below...
           info % fieldType = -1

           call mpas_pool_get_field_info(allFields, itr % memberName, info)

           ! Set time level to read
           if (info % nTimeLevels >= timeLevelIn) then
               timeLevel = timeLevelIn
           else
               timeLevel = 1
           end if

           arrayName = trim(itr % memberName)

           select case (info % fieldType)
               case (MPAS_POOL_REAL)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, real0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=real0d % scalar, rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, real1d, timeLevel)
                           if (real1d % isDecomposed) then
                               call ESMF_ArrayBundleGet(output_bundle, arrayName=arrayName, array=array, rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                              if (trim(real1d % dimNames(1)) == 'nCells') then
                                 ptr_rm_d1 = real1d % array(1:nCellsSolve)
                              else if (trim(real1d % dimNames(1)) == 'nVertices') then
                                 ptr_rm_d1 = real1d % array(1:nVerticesSolve)
                              else if (trim(real1d % dimNames(1)) == 'nEdges') then
                                 ptr_rm_d1 = real1d % array(1:nEdgesSolve)
                              end if
                           end if

                       case (2)
                           call mpas_pool_get_field(allFields, itr % memberName, real2d, timeLevel)

                           if (real2d % isDecomposed) then
                               call ESMF_ArrayBundleGet(output_bundle, arrayName=arrayName, array=array, rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                              if (trim(real2d % dimNames(2)) == 'nCells') then
                                 ptr_rm_d2 = real2d % array(:,1:nCellsSolve)
                              else if (trim(real2d % dimNames(2)) == 'nVertices') then
                                 ptr_rm_d2 = real2d % array(:,1:nVerticesSolve)
                              else if (trim(real2d % dimNames(2)) == 'nEdges') then
                                 ptr_rm_d2 = real2d % array(:,1:nEdgesSolve)
                              end if
                           end if

                       case (3)
                           call mpas_pool_get_field(allFields, itr % memberName, real3d, timeLevel)

                           if (real3d % isDecomposed) then
                               if (real3d % isVarArray) then
                                   do k = 1, size(real3d % constituentNames)
                                       call ESMF_ArrayBundleGet(output_bundle, arrayName=trim(real3d % constituentNames(k)), array=array, rc=rc); ESMF_ERR(rc)
                                       call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                                       if (trim(real3d % dimNames(3)) == 'nCells') then
                                          ptr_rm_d2 = real3d % array(k,:,1:nCellsSolve)
                                       else if (trim(real3d % dimNames(3)) == 'nVertices') then
                                          ptr_rm_d2 = real3d % array(k,:,1:nVerticesSolve)
                                       else if (trim(real3d % dimNames(3)) == 'nEdges') then
                                          ptr_rm_d2 = real3d % array(k,:,1:nEdgesSolve)
                                       end if
                                   end do
                               else
                                 call ESMF_ArrayBundleGet(output_bundle, arrayName=arrayName, array=array, rc=rc); ESMF_ERR(rc)
                                 call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                                 if (trim(real3d % dimNames(3)) == 'nCells') then
                                    ptr_rm_d3 = real3d % array(:,:,1:nCellsSolve)
                                 else if (trim(real3d % dimNames(3)) == 'nVertices') then
                                    ptr_rm_d3 = real3d % array(:,:,1:nVerticesSolve)
                                 else if (trim(real3d % dimNames(3)) == 'nEdges') then
                                    ptr_rm_d3 = real3d % array(:,:,1:nEdgesSolve)
                                 end if
                               end if
                           end if
                       case (4)
                           call mpas_pool_get_field(allFields, itr % memberName, real4d, timeLevel)

                       case (5)
                           call mpas_pool_get_field(allFields, itr % memberName, real5d, timeLevel)
                   end select

               case (MPAS_POOL_INTEGER)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, int0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=int0d % scalar, rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, int1d, timeLevel)

                           if (int1d % isDecomposed) then
                               call ESMF_ArrayBundleGet(output_bundle, arrayName=arrayName, array=array, rc=rc); ESMF_ERR(rc)
                               call ESMF_ArrayGet(array, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                               if (trim(int1d % dimNames(1)) == 'nCells') then
                                  ptr_i4_d1 = int1d % array(1:nCellsSolve)
                               else if (trim(int1d % dimNames(1)) == 'nVertices') then
                                  ptr_i4_d1 = int1d % array(1:nVerticesSolve)
                               else if (trim(int1d % dimNames(1)) == 'nEdges') then
                                  ptr_i4_d1 = int1d % array(1:nEdgesSolve)
                               end if
                           else ! Array has no distributed dimension
                               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=int1d % array, rc=rc); ESMF_ERR(rc)
                           end if

                       case (2)
                           call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)

                       case (3)
                           call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)

                   end select

               case (MPAS_POOL_CHARACTER)
                   select case (info % nDims)
                       case (0)
                           call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                           call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)

                       case (1)
                           call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                   end select
           end select

       end if

   end do FIELD_LOOP

   call postwrite_reindex(allFields, stream % field_pool)

 end subroutine ufs_mpas_update_restart_array_bundle

end module ufs_mpas_wgc_output
