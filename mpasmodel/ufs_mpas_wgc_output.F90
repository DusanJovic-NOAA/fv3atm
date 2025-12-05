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
  use mpas_kind_types,    only : StrKIND, rkind

  use module_mpasmodel_config, only : nVertLevels
  use module_mpasmodel_config, only : nCellsGlobal, nVerticesGlobal, nEdgesGlobal
  use module_mpasmodel_config, only : nCellsSolve, nVerticesSolve, nEdgesSolve
  use module_mpasmodel_config, only : domain_ptr => domain

  implicit none

  private

  public :: ufs_mpas_create_history_bundle
  public :: ufs_mpas_update_history_bundle

  type :: out_var_info
    character(64) :: var_name = ''
  end type

  type(out_var_info), parameter :: history_vars(*) = [ &
       out_var_info('latCell'                ), &
       out_var_info('lonCell'                ), &
       out_var_info('indexToCellID'          ), &
       out_var_info('precipw'                ), &
       out_var_info('vort_pv'                ), &
       out_var_info('surface_pressure'       ), &
       out_var_info('rho_zz'                 ), &
       out_var_info('o3clim'                 ), &
       out_var_info('o3vmr'                  ), &
       out_var_info('theta_m'                ), &
       out_var_info('theta'                  ), &
       out_var_info('iLev_DT'                ), &
       out_var_info('uReconstructZonal'      ), &
       out_var_info('uReconstructMeridional' ), &
       out_var_info('u'                      ), &
       out_var_info('w'                      ), &
       out_var_info('tslb'                   ), &
       out_var_info('zgrid'                  ), &
       out_var_info('scalars'                ), &
       out_var_info('relhum_925hPa'          )  &
  ]

  integer, parameter :: num_history_vars = size(history_vars)

contains

 subroutine ufs_mpas_create_history_bundle(output_bundle, rc)

   type(ESMF_FieldBundle), intent(out) :: output_bundle
   integer, intent(out)                :: rc

   character(*), parameter :: subname = 'ufs_mpas_create_history_bundle'

   call ufs_mpas_create_output_bundle(output_bundle, 'atm_bilinear', history_vars, rc); ESMF_ERR(rc)
   ! call ufs_mpas_create_output_bundle(output_bundle, 'atm_nearest_stod', history_vars, rc); ESMF_ERR(rc)

 end subroutine ufs_mpas_create_history_bundle

 subroutine ufs_mpas_update_history_bundle(output_bundle, rc)

   type(ESMF_FieldBundle), intent(inout) :: output_bundle
   integer, intent(out)                  :: rc

   character(*), parameter :: subname = 'ufs_mpas_update_history_bundle'

   call ufs_mpas_update_output_bundle(output_bundle, 'atm_bilinear', history_vars, rc); ESMF_ERR(rc)
   ! call ufs_mpas_update_output_bundle(output_bundle, 'atm_nearest_stod', history_vars, rc); ESMF_ERR(rc)

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
   type(out_var_info), intent(in)      :: output_vars(:)
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
   ! logical :: is_unique
   character(len=64), allocatable :: dimension_names(:)
   integer :: total_unique, nDims, dimSize
   ! integer, pointer :: dimSize_ptr
   character(len=64) :: variable_names(3000)
   integer :: numVars


   rc = 0

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

   variable_names = ''
   numVars = 0

   do n = 1, size(output_vars)
      field_name = trim(adjustl(output_vars(n)%var_name))

      call mpas_log_write('Inquiring field information for "' // trim(adjustl(field_name)) // '"')

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

            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(output_vars(n)%var_name), rc=rc); ESMF_ERR(rc)
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
                                     meshloc=ESMF_MESHLOC_ELEMENT, name=trim(output_vars(n)%var_name), rc=rc); ESMF_ERR(rc)
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

            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(output_vars(n)%var_name), rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d1, rc=rc); ESMF_ERR(rc)
            ptr_r4_d1 = field_1d_real%array(1:nCellsSolve)

            call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
            ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_1d_real % missingValue, rc=rc); ESMF_ERR(rc)
            call ESMF_InfoSet(field_info, key="/NetCDF/FV3/missing_value", value=9.99e20, rc=rc); ESMF_ERR(rc)
            nullify(field_1d_real)

         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_real, timelevel=1)
            attLists => field_2d_real % attLists
            dimNames(1:nDims) = field_2d_real % dimNames
            block => field_2d_real % block

            field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/2/), ungriddedLBound=[1], ungriddedUBound=[size(field_2d_real%array,dim=1)], &
                                     meshloc=ESMF_MESHLOC_ELEMENT, name=trim(output_vars(n)%var_name), rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
            ptr_r4_d2 = field_2d_real%array(:,1:nCellsSolve)

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

#if 0
                 ! FIXME --------------------------------------------------------------------------------------------
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
                                  write(0,*)'conflictiing dimSize for ', trim(field_3d_real % constituentNames(k)), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
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

                 call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/variables/'//trim(field_3d_real % constituentNames(k)), values=dimNames(1:nDims-1), rc=rc); ESMF_ERR(rc) ! last nDims is distributed dimension
                 numVars = numVars + 1
                 variable_names(numVars) = trim(field_3d_real % constituentNames(k))

                 ! do i = 1, size(attLists(k))
                    att_cursor => attLists(k) % attList
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
                 ! end do

                 nullify(att_cursor)
                 ! nullify(attLists)

                 call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
                 ! FIXME --------------------------------------------------------------------------------------------------
#else
                 call add_field_to_bundle(field_3d_real % constituentNames(k), attLists(k) % attList)
#endif
               end do ! k = 1, size(field_3d_real % constituentNames)

            else

               dimNames(1:nDims) = field_3d_real % dimNames
               field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = (/3/), ungriddedLBound=[1,1], ungriddedUBound=[size(field_3d_real%array,dim=1), size(field_3d_real%array,dim=2)], &
                                        meshloc=ESMF_MESHLOC_ELEMENT, name=trim(output_vars(n)%var_name), rc=rc); ESMF_ERR(rc)
               call ESMF_FieldGet(field, farrayPtr=ptr_r4_d3, rc=rc); ESMF_ERR(rc)
               ptr_r4_d3 = field_3d_real%array(:,:,1:nCellsSolve)
               write(0,*)trim(field_name), ' shape ', shape(ptr_r4_d3), shape(field_3d_real%array(:,:,1:nCellsSolve))

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
#if 0
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
                          write(0,*)'conflictiing dimSize for ', trim(output_vars(n)%var_name), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
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

         call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/variables/'//trim(output_vars(n)%var_name), values=dimNames(1:nDims-1), rc=rc); ESMF_ERR(rc) ! last nDims is distributed dimension
         numVars = numVars + 1
         variable_names(numVars) = trim(output_vars(n)%var_name)

         do i = 1, size(attLists)
            att_cursor => attLists(i) % attList
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
         end do

         nullify(att_cursor)
         nullify(attLists)

         call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)
#else
         call add_field_to_bundle(output_vars(n)%var_name, attLists(1) % attList)
#endif
      end if

   end do

   ! bundle attributes
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3/grid_id", value=1, rc=rc); ESMF_ERR(rc)
   call ESMF_InfoSet(bundle_info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)

   call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/variable_names', values=variable_names(1:numVars), rc=rc); ESMF_ERR(rc)

   ! dimensions attributes
   allocate(dimension_names(size(dim_info_arr)))
   do i = 1, size(dim_info_arr)
       ! write(0,*)trim(dim_info_arr(i) % dimName), ' ', dim_info_arr(i) % dimSize
       call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/dimensions/'//trim(dim_info_arr(i) % dimName), value=dim_info_arr(i) % dimSize, rc=rc); ESMF_ERR(rc)
       dimension_names(i)=trim(dim_info_arr(i) % dimName)
   end do
   call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/dimension_names', values=dimension_names, rc=rc); ESMF_ERR(rc)

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

          numVars = numVars + 1
          variable_names(numVars) = trim(varName)

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

 subroutine ufs_mpas_update_output_bundle(output_bundle, bundle_name, output_vars, rc)

   use mpas_attlist,       only : att_list_type, att_lists_type, &
                                  MPAS_ATT_INT, MPAS_ATT_INTA, MPAS_ATT_REAL,MPAS_ATT_REALA, MPAS_ATT_TEXT, &
                                  MPAS_LOG_CRIT
   use mpas_derived_types, only : field1dinteger, field2dinteger, field1dreal, field2dreal, field3dreal, &
                                  mpas_pool_type, mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, block_type
   use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_dimension
   use mpas_log,           only : mpas_log_write

   type(ESMF_FieldBundle), intent(inout) :: output_bundle
   character(len=*), intent(in)        :: bundle_name
   type(out_var_info), intent(in)      :: output_vars(:)
   integer, intent(out)                :: rc

   character(*), parameter :: subname = 'ufs_mpas_update_output_bundle'

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

   ! real(RKIND), allocatable :: pfull(:), phalf(:)

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
   logical :: is_unique
   character(len=64), allocatable :: dimension_names(:)
   integer :: total_unique, nDims, dimSize
   integer, pointer :: dimSize_ptr
   character(len=64) :: variable_names(3000)
   integer :: numVars


   rc = 0

   frestart(:) = -1

   localpet = domain_ptr % dminfo % my_proc_id

   allFields => domain_ptr % blocklist % allfields

   variable_names = ''
   numVars = 0

   do n = 1, size(output_vars)
      field_name = trim(adjustl(output_vars(n)%var_name))

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
         case (1)
            call mpas_pool_get_field(allFields, trim(field_name), field_1d_integer, timelevel=1)
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
            ptr_i4_d1 = field_1d_integer%array(1:nCellsSolve)
            nullify(field_1d_integer)
         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_integer, timelevel=1)
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
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d1, rc=rc); ESMF_ERR(rc)
            ptr_r4_d1 = field_1d_real%array(1:nCellsSolve)
            nullify(field_1d_real)
         case (2)
            call mpas_pool_get_field(allFields, trim(field_name), field_2d_real, timelevel=1)
            call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
            call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
            ptr_r4_d2 = field_2d_real%array(:,1:nCellsSolve)
            nullify(field_2d_real)

         case (3)
            call mpas_pool_get_field(allFields, trim(field_name), field_3d_real, timelevel=1)

            if (field_3d_real % isVarArray) then
               do k = 1, size(field_3d_real % constituentNames)
                  call ESMF_FieldBundleGet(output_bundle, fieldName=trim(field_3d_real % constituentNames(k)), field=field, rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                  ptr_r4_d2 = field_3d_real%array(k,:,1:nCellsSolve)
               end do ! k = 1, size(field_3d_real % constituentNames)
            else
               call ESMF_FieldBundleGet(output_bundle, fieldName=field_name, field=field, rc=rc); ESMF_ERR(rc)
               call ESMF_FieldGet(field, farrayPtr=ptr_r4_d3, rc=rc); ESMF_ERR(rc)
               ptr_r4_d3 = field_3d_real%array(:,:,1:nCellsSolve)
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


  !> #########################################################################################
  !> Convert one or more values of any intrinsic data types to a character string for pretty
  !> printing.
  !> If `value` contains more than one element, the elements will be stringified, delimited by `separator`, then concatenated.
  !> If `value` contains exactly one element, the element will be stringified without using `separator`.
  !> If `value` contains zero element or is of unsupported data types, an empty character string is produced.
  !> If `separator` is not supplied, it defaults to ", " (i.e., a comma and a space).
  !> (KCW, 2024-02-04)
  !> Ported for UWM (DJS: 2025)
  !> #########################################################################################
  pure function stringify(value, separator)
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64

    class(*), intent(in) :: value(:)
    character(*), optional, intent(in) :: separator
    character(:), allocatable :: stringify

    integer, parameter :: sizelimit = 1024

    character(:), allocatable :: buffer, delimiter, format
    character(:), allocatable :: value_c(:)
    integer :: i, n, offset

    if (present(separator)) then
       delimiter = separator
    else
       delimiter = ', '
    end if

    n = min(size(value), sizelimit)

    if (n == 0) then
       stringify = ''

       return
    end if

    select type (value)
    type is (character(*))
       allocate(character(len(value) * n + len(delimiter) * (n - 1)) :: buffer)

       buffer(:) = ''
       offset = 0

       ! Workaround for a bug in GNU Fortran >= 12. This is perhaps the manifestation of GCC Bugzilla Bug 100819.
       ! When a character string array is passed as the actual argument to an unlimited polymorphic dummy argument,
       ! its array index and length parameter are mishandled.
       allocate(character(len(value)) :: value_c(size(value)))

       value_c(:) = value(:)

       do i = 1, n
          if (len(delimiter) > 0 .and. i > 1) then
             buffer(offset + 1:offset + len(delimiter)) = delimiter
             offset = offset + len(delimiter)
          end if

          if (len_trim(adjustl(value_c(i))) > 0) then
             buffer(offset + 1:offset + len_trim(adjustl(value_c(i)))) = trim(adjustl(value_c(i)))
             offset = offset + len_trim(adjustl(value_c(i)))
          end if
       end do

       deallocate(value_c)
    type is (integer(int32))
       allocate(character(11 * n + len(delimiter) * (n - 1)) :: buffer)
       allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

       write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
       write(buffer, format) value
    type is (integer(int64))
       allocate(character(20 * n + len(delimiter) * (n - 1)) :: buffer)
       allocate(character(17 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

       write(format, '(a, i0, 3a)') '(ss, ', n, '(i0, :, "', delimiter, '"))'
       write(buffer, format) value
    type is (logical)
       allocate(character(1 * n + len(delimiter) * (n - 1)) :: buffer)
       allocate(character(13 + len(delimiter) + floor(log10(real(n))) + 1) :: format)

       write(format, '(a, i0, 3a)') '(', n, '(l1, :, "', delimiter, '"))'
       write(buffer, format) value
    type is (real(real32))
       allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

       if (maxval(abs(value)) < 1.0e5_real32) then
          allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
          write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
       else
          allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
          write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
       end if

       write(buffer, format) value
    type is (real(real64))
       allocate(character(13 * n + len(delimiter) * (n - 1)) :: buffer)

       if (maxval(abs(value)) < 1.0e5_real64) then
          allocate(character(20 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
          write(format, '(a, i0, 3a)') '(ss, ', n, '(f13.6, :, "', delimiter, '"))'
       else
          allocate(character(23 + len(delimiter) + floor(log10(real(n))) + 1) :: format)
          write(format, '(a, i0, 3a)') '(ss, ', n, '(es13.6e2, :, "', delimiter, '"))'
       end if

       write(buffer, format) value
    class default
       stringify = ''

       return
    end select

    stringify = trim(buffer)

  end function stringify

end module ufs_mpas_wgc_output
