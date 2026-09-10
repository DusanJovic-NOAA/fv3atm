#define ESMF_ERR(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

! #define ESMF_ERR(rc) \
!   if (rc /= 0) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
!   if (rc /= 0) call ESMF_Finalize(endflag=ESMF_END_ABORT)

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1

module ufs_mpas_wgc_output

#ifdef MPAS_USE_MPI_F08
   use mpi_f08
#else
   use mpi
#endif
   use esmf

   use mpas_derived_types, only : domain_type
   use mpas_kind_types,    only : StrKIND, RKIND, R4KIND, R8KIND

   use module_mpasmodel_config, only : nVertLevels
   use module_mpasmodel_config, only : nCellsGlobal, nVerticesGlobal, nEdgesGlobal
   use module_mpasmodel_config, only : nCellsSolve, nVerticesSolve, nEdgesSolve
   use module_mpasmodel_config, only : domain_ptr => domain
   use module_mpasmodel_config, only : frestart

   implicit none

   private

   public :: ufs_mpas_wgc_output_initialize
   public :: ufs_mpas_wgc_output_update
   public :: ufs_mpas_wgc_output_finalize

   public :: ufs_mpas_get_esmf_mesh

   public :: ufs_mpas_create_restart_bundle
   public :: ufs_mpas_update_restart_bundle

   integer, parameter, public :: max_num_output_vars = 1000

   type, public :: ufs_mpas_output_type

      ! Unique name of this output.
      ! There must be no other instance of this type using the same name
      character(len=64) :: name = ''
      ! Grid type. Currentlly supported: 'regridded', 'mpas_mesh'
      character(len=64) :: grid_type = ''
      character(len=64) :: list_of_vars_fname = ''
      character(len=64) :: mpas_stream = ''
      real(ESMF_KIND_R8), allocatable, dimension(:) :: output_fh

      character(len=64) :: grid_projection = ''
      real :: cen_lon, cen_lat, stdlat1, stdlat2, lon1, lat1, lon2, lat2, dlon, dlat, dx, dy
      integer :: nx, ny, imo, jmo

      ! Field bundles for 5 different remappings
      type(ESMF_FieldBundle) :: bilinear_field_bundle
      type(ESMF_FieldBundle) :: nearest_dtos_field_bundle
      type(ESMF_FieldBundle) :: nearest_stod_field_bundle
      type(ESMF_FieldBundle) :: patch_field_bundle
      type(ESMF_FieldBundle) :: conserve_field_bundle
      ! List of variables for 5 different remappings
      character(len=64) :: bilinear_vars(max_num_output_vars)
      character(len=64) :: nearest_dtos_vars(max_num_output_vars)
      character(len=64) :: nearest_stod_vars(max_num_output_vars)
      character(len=64) :: patch_vars(max_num_output_vars)
      character(len=64) :: conserve_vars(max_num_output_vars)
      ! Number of actual variables for 5 differenr rempappings
      integer :: num_bilinear_vars = 0
      integer :: num_nearest_dtos_vars = 0
      integer :: num_nearest_stod_vars = 0
      integer :: num_patch_vars = 0
      integer :: num_conserve_vars = 0

      ! Array bundle if this output is on native mesh
      type(ESMF_ArrayBundle) :: native_mesh_array_bundle

   end type ufs_mpas_output_type

   type(ufs_mpas_output_type), dimension(:), allocatable, public :: ufs_mpas_outputs
   integer, public :: num_streams

   type(ESMF_Mesh), private :: the_mpas_esmf_mesh

   ! FIXME: Temporary fix to get bit-identical outputs from both PIO and SMIOL
   ! Use this value instead the one defined in mpas_io.F to be consistent between PIO and SMIOL
   integer, parameter :: MPAS_INT_FILLVAL_NEG_HUGE = -huge(0)

contains

   subroutine ufs_mpas_wgc_output_initialize(exportState, ngrids)

      use mpas_dmpar,         only : mpas_dmpar_bcast_int, mpas_dmpar_bcast_char, IO_NODE

      implicit none

      type(ESMF_State), intent(inout) :: exportState
      integer, intent(inout) :: ngrids

      ! Local
      integer :: i, rc
      type(ESMF_Info) :: info
      integer :: localpet
      logical :: asOkay
      integer :: ufs_mpas_streams_content_len
      character(len=:), allocatable :: ufs_mpas_streams_content
      logical :: success
      type(ESMF_HConfig) :: hconfig, streams_hconfig, stream_hconfig, grid_spec_hconfig

      localpet = domain_ptr % dminfo % my_proc_id

      ! read the content of the ufs mpas streams config file into a character string
      ! only on the IO_NODE, then bradcast it to all ranks in the communicator

      if (localpet == IO_NODE) then
         call read_entire_file('ufs_mpas_streams.yaml', ufs_mpas_streams_content, success)
         if (.not. success) then
            ESMF_ERR(1)
         end if
         ufs_mpas_streams_content_len = len(ufs_mpas_streams_content)
      end if

      call mpas_dmpar_bcast_int(domain_ptr % dminfo, ufs_mpas_streams_content_len, IO_NODE)

      if (.not. allocated(ufs_mpas_streams_content)) then
         allocate(character(len=ufs_mpas_streams_content_len) :: ufs_mpas_streams_content)
      end if
      call mpas_dmpar_bcast_char(domain_ptr % dminfo, ufs_mpas_streams_content, IO_NODE)

      ! hconfig = ESMF_HConfigCreate(filename='ufs_mpas_streams.yaml', rc=rc); ESMF_ERR(rc)
      hconfig = ESMF_HConfigCreate(content=ufs_mpas_streams_content, rc=rc); ESMF_ERR(rc)
      if (localpet == IO_NODE) then
         call ESMF_HConfigFileSave(hconfig, filename="saveMe.yml", rc=rc); ESMF_ERR(rc)
      end if

      num_streams = ESMF_HConfigGetSize(hconfig, keyString='streams', rc=rc); ESMF_ERR(rc)

      allocate(ufs_mpas_outputs(num_streams))

      streams_hconfig = ESMF_HConfigCreateAt(hconfig, keyString='streams', rc=rc); ESMF_ERR(rc)

      do i=1,num_streams
         stream_hconfig = ESMF_HConfigCreateAt(streams_hconfig, index=i, rc=rc); ESMF_ERR(rc)

         ufs_mpas_outputs(i) % name = ESMF_HConfigAsString(stream_hconfig, keyString='name',rc=rc); ESMF_ERR(rc)
         ufs_mpas_outputs(i) % grid_type = ESMF_HConfigAsString(stream_hconfig, keyString='grid_type',rc=rc); ESMF_ERR(rc)
         if (trim(ufs_mpas_outputs(i) % grid_type) /= 'regridded' .and. trim(ufs_mpas_outputs(i) % grid_type) /= 'mpas_mesh') then
            write(0,*)"ERROR: Unknown 'grid_type' for stream ",trim(ufs_mpas_outputs(i) % name), ' grid_type: ',trim(ufs_mpas_outputs(i) % grid_type)
            ESMF_ERR(1)
         end if

         ufs_mpas_outputs(i) % list_of_vars_fname = ESMF_HConfigAsString(stream_hconfig, keyString='list_of_vars_fname', asOkay=asOkay, rc=rc); ESMF_ERR(rc)
         ufs_mpas_outputs(i) % mpas_stream = ESMF_HConfigAsString(stream_hconfig, keyString='mpas_stream', asOkay=asOkay, rc=rc); ESMF_ERR(rc)

         if (trim(ufs_mpas_outputs(i) % grid_type) == 'regridded') then
            grid_spec_hconfig = ESMF_HConfigCreateAt(stream_hconfig, keyString='grid_spec', rc=rc); ESMF_ERR(rc)
            ufs_mpas_outputs(i) % grid_projection = ESMF_HConfigAsString(grid_spec_hconfig, keyString='projection', rc=rc); ESMF_ERR(rc)
            if (trim(ufs_mpas_outputs(i) % grid_projection) == 'global_latlon') then
               ufs_mpas_outputs(i) % imo     = ESMF_HConfigAsI4(grid_spec_hconfig, keyString='imo',     rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % jmo     = ESMF_HConfigAsI4(grid_spec_hconfig, keyString='jmo',     rc=rc); ESMF_ERR(rc)
            else if (trim(ufs_mpas_outputs(i) % grid_projection) == 'regional_latlon') then
               ufs_mpas_outputs(i) % lon1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lon1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lat1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lat1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lon2    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lon2',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lat2    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lat2',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dlon    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dlon',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dlat    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dlat',    rc=rc); ESMF_ERR(rc)
            else if (trim(ufs_mpas_outputs(i) % grid_projection) == 'rotated_latlon') then
               ufs_mpas_outputs(i) % cen_lon = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='cen_lon', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % cen_lat = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='cen_lat', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lon1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lon1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lat1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lat1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lon2    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lon2',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lat2    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lat2',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dlon    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dlon',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dlat    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dlat',    rc=rc); ESMF_ERR(rc)
            else if (trim(ufs_mpas_outputs(i) % grid_projection) == 'lambert_conformal') then
               ufs_mpas_outputs(i) % cen_lon = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='cen_lon', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % cen_lat = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='cen_lat', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % stdlat1 = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='stdlat1', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % stdlat2 = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='stdlat2', rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lon1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lon1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % lat1    = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='lat1',    rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dx      = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dx',      rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % dy      = ESMF_HConfigAsR4(grid_spec_hconfig, keyString='dy',      rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % nx      = ESMF_HConfigAsI4(grid_spec_hconfig, keyString='nx',      rc=rc); ESMF_ERR(rc)
               ufs_mpas_outputs(i) % ny      = ESMF_HConfigAsI4(grid_spec_hconfig, keyString='ny',      rc=rc); ESMF_ERR(rc)
            end if
            call ESMF_HConfigDestroy(grid_spec_hconfig, rc=rc); ESMF_ERR(rc)
         end if
         ufs_mpas_outputs(i) % output_fh = ESMF_HConfigAsR4Seq(stream_hconfig, keyString='output_fh', rc=rc); ESMF_ERR(rc)

         call ESMF_HConfigDestroy(stream_hconfig, rc=rc); ESMF_ERR(rc)
      end do

      call ESMF_HConfigDestroy(streams_hconfig, rc=rc); ESMF_ERR(rc)

      call ESMF_HConfigDestroy(hconfig, rc=rc); ESMF_ERR(rc)

      do i=1,num_streams

         if (trim(ufs_mpas_outputs(i) % grid_type) /= 'mpas_mesh') then

            ngrids = ngrids + 1  ! count only remapped output grids

            call parse_output_list_vars(ufs_mpas_outputs(i), rc)

            if (ufs_mpas_outputs(i) % num_bilinear_vars > 0) then
               call ufs_mpas_create_output_bundle(ufs_mpas_outputs(i) % bilinear_field_bundle, &
                                                  trim(ufs_mpas_outputs(i) % name)//'_bilinear', &
                                                  trim(ufs_mpas_outputs(i) % name), &
                                                  ufs_mpas_outputs(i) % bilinear_vars(1:ufs_mpas_outputs(i) % num_bilinear_vars), &
                                                  ngrids, &
                                                  ufs_mpas_output = ufs_mpas_outputs(i), &
                                                  rc=rc); ESMF_ERR(rc)
               call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % bilinear_field_bundle], rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_nearest_dtos_vars > 0) then
               call ufs_mpas_create_output_bundle(ufs_mpas_outputs(i) % nearest_dtos_field_bundle, &
                                                  trim(ufs_mpas_outputs(i) % name)//'_nearest_dtos', &
                                                  trim(ufs_mpas_outputs(i) % name), &
                                                  ufs_mpas_outputs(i) % nearest_dtos_vars(1:ufs_mpas_outputs(i) % num_nearest_dtos_vars), &
                                                  ngrids, &
                                                  ufs_mpas_output = ufs_mpas_outputs(i), &
                                                  rc=rc); ESMF_ERR(rc)
               call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % nearest_dtos_field_bundle], rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_nearest_stod_vars > 0) then
               call ufs_mpas_create_output_bundle(ufs_mpas_outputs(i) % nearest_stod_field_bundle, &
                                                  trim(ufs_mpas_outputs(i) % name)//'_nearest_stod', &
                                                  trim(ufs_mpas_outputs(i) % name), &
                                                  ufs_mpas_outputs(i) % nearest_stod_vars(1:ufs_mpas_outputs(i) % num_nearest_stod_vars), &
                                                  ngrids, &
                                                  ufs_mpas_output = ufs_mpas_outputs(i), &
                                                  rc=rc); ESMF_ERR(rc)
               call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % nearest_stod_field_bundle], rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_patch_vars > 0) then
               call ufs_mpas_create_output_bundle(ufs_mpas_outputs(i) % patch_field_bundle, &
                                                  trim(ufs_mpas_outputs(i) % name)//'_patch', &
                                                  trim(ufs_mpas_outputs(i) % name), &
                                                  ufs_mpas_outputs(i) % patch_vars(1:ufs_mpas_outputs(i) % num_patch_vars), &
                                                  ngrids, &
                                                  ufs_mpas_output = ufs_mpas_outputs(i), &
                                                  rc=rc); ESMF_ERR(rc)
               call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % patch_field_bundle], rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_conserve_vars > 0) then
               call ufs_mpas_create_output_bundle(ufs_mpas_outputs(i) % conserve_field_bundle, &
                                                  trim(ufs_mpas_outputs(i) % name)//'_conserve', &
                                                  trim(ufs_mpas_outputs(i) % name), &
                                                  ufs_mpas_outputs(i) % conserve_vars(1:ufs_mpas_outputs(i) % num_conserve_vars), &
                                                  ngrids, &
                                                  ufs_mpas_output = ufs_mpas_outputs(i), &
                                                  rc=rc); ESMF_ERR(rc)
               call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % conserve_field_bundle], rc=rc); ESMF_ERR(rc)
            end if
         else  !
            ! 'mpas_mesh' stream; 'restart' or other native mesh streams
            if (trim(ufs_mpas_outputs(i) % name) == 'restart') then
               call ufs_mpas_create_restart_array_bundle(ufs_mpas_outputs(i) % native_mesh_array_bundle, &
                                                         bundle_name='restart_mpas_array', &
                                                         stream_name='restart', &
                                                         rc=rc); ESMF_ERR(rc)
               call ESMF_InfoGetFromHost(ufs_mpas_outputs(i) % native_mesh_array_bundle, info=info, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(info, key='/NetCDF/FV3-nooutput/frestart', values=frestart, rc=rc); ESMF_ERR(rc)
            else
               if (trim(ufs_mpas_outputs(i) % mpas_stream) == '') then
                  write(0,*)"ERROR: mpas_stream must be specified for outputs on 'mpas_mesh' grid trype"
                  rc = 1
                  ESMF_ERR(rc)
               end if
               call ufs_mpas_create_restart_array_bundle(ufs_mpas_outputs(i) % native_mesh_array_bundle, &
                                                         bundle_name=trim(ufs_mpas_outputs(i) % name), &
                                                         stream_name=trim(ufs_mpas_outputs(i) % mpas_stream), &
                                                         rc=rc); ESMF_ERR(rc)
            end if
            call ESMF_StateAdd(exportState, [ufs_mpas_outputs(i) % native_mesh_array_bundle], rc=rc); ESMF_ERR(rc)
         end if

      end do

   end subroutine ufs_mpas_wgc_output_initialize

   subroutine ufs_mpas_wgc_output_update(seconds)

      implicit none

      integer, intent(in) :: seconds

      integer :: i, rc

      do i=1,num_streams

         if (trim(ufs_mpas_outputs(i) % grid_type) /= 'mpas_mesh') then

            if (ufs_mpas_outputs(i) % num_bilinear_vars > 0) then
               call ufs_mpas_update_output_bundle(ufs_mpas_outputs(i) % bilinear_field_bundle, &
                                                  ufs_mpas_outputs(i) % bilinear_vars(1:ufs_mpas_outputs(i) % num_bilinear_vars), &
                                                  rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_nearest_dtos_vars > 0) then
               call ufs_mpas_update_output_bundle(ufs_mpas_outputs(i) % nearest_dtos_field_bundle, &
                                                  ufs_mpas_outputs(i) % nearest_dtos_vars(1:ufs_mpas_outputs(i) % num_nearest_dtos_vars), &
                                                  rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_nearest_stod_vars > 0) then
               call ufs_mpas_update_output_bundle(ufs_mpas_outputs(i) % nearest_stod_field_bundle, &
                                                  ufs_mpas_outputs(i) % nearest_stod_vars(1:ufs_mpas_outputs(i) % num_nearest_stod_vars), &
                                                  rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_patch_vars > 0) then
               call ufs_mpas_update_output_bundle(ufs_mpas_outputs(i) % patch_field_bundle, &
                                                  ufs_mpas_outputs(i) % patch_vars(1:ufs_mpas_outputs(i) % num_patch_vars), &
                                                  rc=rc); ESMF_ERR(rc)
            end if

            if (ufs_mpas_outputs(i) % num_conserve_vars > 0) then
               call ufs_mpas_update_output_bundle(ufs_mpas_outputs(i) % conserve_field_bundle, &
                                                  ufs_mpas_outputs(i) % conserve_vars(1:ufs_mpas_outputs(i) % num_conserve_vars), &
                                                  rc=rc); ESMF_ERR(rc)
            end if
         else
            ! 'mpas_mesh' stream; 'restart' or other native mesh streams
            if (trim(ufs_mpas_outputs(i) % name) == 'restart') then
               if (ANY(frestart(:) == seconds)) then
                  call ufs_mpas_update_restart_array_bundle(ufs_mpas_outputs(i) % native_mesh_array_bundle, stream_name='restart', rc=rc); ESMF_ERR(rc)
               end if
            else
               if (trim(ufs_mpas_outputs(i) % mpas_stream) == '') then
                  write(0,*)"ERROR: mpas_stream must be specified for outputs on 'mpas_mesh' grid type"
                  rc = 1
                  ESMF_ERR(rc)
               end if
               call ufs_mpas_update_restart_array_bundle(ufs_mpas_outputs(i) % native_mesh_array_bundle, &
                                                         stream_name=trim(ufs_mpas_outputs(i) % mpas_stream), &
                                                         rc=rc); ESMF_ERR(rc)
            end if
         end if

      end do

   end subroutine ufs_mpas_wgc_output_update

   subroutine ufs_mpas_wgc_output_finalize()

      deallocate(ufs_mpas_outputs)

   end subroutine ufs_mpas_wgc_output_finalize

   subroutine ufs_mpas_create_output_bundle(output_bundle, bundle_name, output_file, output_vars, ngrids, ufs_mpas_output, rc)

      use mpas_attlist,       only : att_list_type, att_lists_type, &
                                     MPAS_ATT_INT, MPAS_ATT_REAL, MPAS_ATT_TEXT, &
                                     MPAS_LOG_CRIT, MPAS_LOG_WARN
      use mpas_derived_types, only : field1dinteger, field2dinteger, field1dreal, field2dreal, field3dreal, &
                                     mpas_pool_type, mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, block_type
      use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_dimension
      use mpas_log,           only : mpas_log_write

      type(ESMF_FieldBundle), intent(out) :: output_bundle
      character(len=*), intent(in)        :: bundle_name
      character(len=*), intent(in)        :: output_file
      character(len=*), intent(in)        :: output_vars(:)
      integer, intent(in)                 :: ngrids
      type(ufs_mpas_output_type), optional, intent(in) :: ufs_mpas_output
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
      integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:)

      integer :: frestart(1)
      integer :: i,j,k,n
      integer :: localpet

      type :: dim_info_t
         character(64) :: dimName
         integer :: dimSize
      end type dim_info_t

      type(dim_info_t), allocatable :: dim_info_arr(:)

      type(block_type), pointer :: block
      type(att_list_type), pointer :: att_cursor
      type(att_lists_type), dimension(:), pointer :: attLists
      character(len=StrKIND), dimension(5) :: dimNames
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
         if (.not. mpas_pool_field_info % isActive) then
             cycle
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
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=MPAS_INT_FILLVAL_NEG_HUGE, rc=rc); ESMF_ERR(rc)
               nullify(field_1d_integer)
            case (2)
               call mpas_pool_get_field(allFields, trim(field_name), field_2d_integer, timelevel=1)
               attLists => field_2d_integer % attLists
               dimNames(1:nDims) = field_2d_integer % dimNames
               block => field_2d_integer % block

               field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(field_2d_integer%array,dim=1)], &
                                        meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
               call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
               ptr_i4_d2 = field_2d_integer%array(:,1:nCellsSolve)

               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=MPAS_INT_FILLVAL_NEG_HUGE, rc=rc); ESMF_ERR(rc)
               nullify(field_2d_integer)
            case default
               call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_WARN, intArgs=[mpas_pool_field_info % ndims])
               cycle
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
                  cycle
               end if

               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
               ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_1d_real % missingValue, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=9.99e20, rc=rc); ESMF_ERR(rc)
               nullify(field_1d_real)

            case (2)
               call mpas_pool_get_field(allFields, trim(field_name), field_2d_real, timelevel=1)
               attLists => field_2d_real % attLists
               dimNames(1:nDims) = field_2d_real % dimNames
               block => field_2d_real % block

               if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(field_2d_real%array,dim=1)], &
                                           meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                  ptr_r4_d2 = field_2d_real%array(:,1:nCellsSolve)
               else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(field_2d_real%array,dim=1)], &
                                           meshloc=ESMF_MESHLOC_NODE, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                  ptr_r4_d2 = field_2d_real%array(:,1:nVerticesSolve)
               else
                  if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                  cycle
               end if

               call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
               ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_2d_real % missingValue, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=9.99e20, rc=rc); ESMF_ERR(rc)
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

                     field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(field_3d_real%array,dim=2)], &
                                              meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_3d_real % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_r4_d2, rc=rc); ESMF_ERR(rc)
                     ptr_r4_d2 = field_3d_real%array(k,:,1:nCellsSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_2d_real % missingValue, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=9.99e20, rc=rc); ESMF_ERR(rc)

                     call add_field_to_bundle(field_3d_real % constituentNames(k), attLists(k) % attList)

                  end do  ! k = 1, size(field_3d_real % constituentNames)

               else

                  dimNames(1:nDims) = field_3d_real % dimNames
                  field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_R4, gridToFieldMap = [3], ungriddedLBound=[1,1], ungriddedUBound=[size(field_3d_real%array,dim=1), size(field_3d_real%array,dim=2)], &
                                           meshloc=ESMF_MESHLOC_ELEMENT, name=trim(field_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_FieldGet(field, farrayPtr=ptr_r4_d3, rc=rc); ESMF_ERR(rc)
                  ptr_r4_d3 = field_3d_real%array(:,:,1:nCellsSolve)

                  call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                  ! call ESMF_InfoSet(info, key="/NetCDF/FV3/missing_value", value=field_3d_real % missingValue, rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(field_info, key='/NetCDF/FV3/missing_value', value=9.99e20, rc=rc); ESMF_ERR(rc)

               end if

               nullify(field_3d_real)

            case default
               write(0,*)'Unsupported field rank ', trim(field_name), mpas_pool_field_info % ndims
               call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_WARN, intArgs=[mpas_pool_field_info % ndims])
               cycle
            end select
         case default
            write(0,*)'Unsupported field type (Must be one of: integer, real)', trim(field_name), mpas_pool_field_info % fieldtype
            call mpas_log_write(subname//' Unsupported field type (Must be one of: integer, real)', MPAS_LOG_WARN)
            cycle
         end select

         if (.not. isVarArray) then
            call add_field_to_bundle(field_name, attLists(1) % attList)
         end if

      end do

      ! bundle attributes
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/grid_id', value=ngrids, rc=rc); ESMF_ERR(rc)  !grid_id is current value of ngrids counter
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/frestart', values=frestart, rc=rc); ESMF_ERR(rc)

      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_type', value=trim(ufs_mpas_output % grid_type), rc=rc); ESMF_ERR(rc)
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/projection', value=trim(ufs_mpas_output % grid_projection), rc=rc); ESMF_ERR(rc)
      if (trim(ufs_mpas_output % grid_projection) == 'global_latlon') then
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/imo',     value=ufs_mpas_output % imo,     rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/jmo',     value=ufs_mpas_output % jmo,     rc=rc); ESMF_ERR(rc)
      else if (trim(ufs_mpas_output % grid_projection) == 'regional_latlon') then
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lon1',    value=ufs_mpas_output % lon1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lat1',    value=ufs_mpas_output % lat1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lon2',    value=ufs_mpas_output % lon2,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lat2',    value=ufs_mpas_output % lat2,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dlon',    value=ufs_mpas_output % dlon,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dlat',    value=ufs_mpas_output % dlat,    rc=rc); ESMF_ERR(rc)
      else if (trim(ufs_mpas_output % grid_projection) == 'rotated_latlon') then
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/cen_lon', value=ufs_mpas_output % cen_lon, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/cen_lat', value=ufs_mpas_output % cen_lat, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lon1',    value=ufs_mpas_output % lon1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lat1',    value=ufs_mpas_output % lat1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lon2',    value=ufs_mpas_output % lon2,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lat2',    value=ufs_mpas_output % lat2,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dlon',    value=ufs_mpas_output % dlon,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dlat',    value=ufs_mpas_output % dlat,    rc=rc); ESMF_ERR(rc)
      else if (trim(ufs_mpas_output % grid_projection) == 'lambert_conformal') then
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/cen_lon', value=ufs_mpas_output % cen_lon, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/cen_lat', value=ufs_mpas_output % cen_lat, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/cen_lat', value=ufs_mpas_output % cen_lat, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/stdlat1', value=ufs_mpas_output % stdlat1, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/stdlat2', value=ufs_mpas_output % stdlat2, rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lon1',    value=ufs_mpas_output % lon1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/lat1',    value=ufs_mpas_output % lat1,    rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dx',      value=ufs_mpas_output % dx,      rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/dy',      value=ufs_mpas_output % dy,      rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/nx',      value=ufs_mpas_output % nx,      rc=rc); ESMF_ERR(rc)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/grid_spec/ny',      value=ufs_mpas_output % ny,      rc=rc); ESMF_ERR(rc)
      else
         write(0,*)'ERROR: grid_type must be set'
         rc = 1
         ESMF_ERR(rc)
      end if

      ! dimensions attributes
      do i = 1, size(dim_info_arr)
         call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/ungridded_dimensions/'//trim(dim_info_arr(i) % dimName), value=dim_info_arr(i) % dimSize, rc=rc); ESMF_ERR(rc)
      end do

      return

   contains

      subroutine add_field_to_bundle(varName, attList)

         character(len=*), intent(in) :: varName
         type(att_list_type), pointer, intent(in) :: attList

         integer :: i
         logical :: is_unique
         integer, pointer :: dimSize_ptr

         call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value=trim(output_file), rc=rc); ESMF_ERR(rc)

         do i = 1, nDims
            call mpas_pool_get_dimension(block % dimensions, trim(dimNames(i)), dimSize_ptr)

            if (associated(dimSize_ptr)) then
               dimSize = dimSize_ptr
            else
               dimSize = -1
            end if

            if (i == nDims) then  ! last dimension should be one of the decomposed nCells, nEdges, nVertices
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

         call ESMF_InfoSet(bundle_info, key='/NetCDF/MPAS/variables/'//trim(varName), values=dimNames(1:nDims-1), rc=rc); ESMF_ERR(rc)  ! last nDims is distributed dimension

         att_cursor => attList
         do while (associated(att_cursor))
            if (att_cursor % attType == MPAS_ATT_INT) then
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/'//trim(att_cursor % attName), value=att_cursor % attValueInt, rc=rc); ESMF_ERR(rc)
            else if (att_cursor % attType == MPAS_ATT_REAL) then
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/'//trim(att_cursor % attName), value=att_cursor % attValueReal, rc=rc); ESMF_ERR(rc)
            else if (att_cursor % attType == MPAS_ATT_TEXT) then
               call ESMF_InfoSet(field_info, key='/NetCDF/FV3/'//trim(att_cursor % attName), value=trim(att_cursor % attValueText), rc=rc); ESMF_ERR(rc)
            else
               ! write(0,*) i, '"'//trim(att_cursor % attName)//'" unknown type ', att_cursor % attType
            end if
            att_cursor => att_cursor % next
         end do

         nullify(att_cursor)

         call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)

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

      use mpas_attlist,       only : MPAS_LOG_CRIT, MPAS_LOG_WARN
      use mpas_derived_types, only : field1dinteger, field2dinteger, field1dreal, field2dreal, field3dreal
      use mpas_derived_types, only : mpas_pool_type, mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, block_type
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
      integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:)

      integer :: k,n
      integer :: localpet

      character(len=StrKIND), dimension(5) :: dimNames
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
         if (.not. mpas_pool_field_info % isActive) then
             cycle
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
               call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_WARN, intArgs=[mpas_pool_field_info % ndims])
               cycle
            end select

         case (mpas_pool_real)
            select case (mpas_pool_field_info % ndims)

            case (1)
               call mpas_pool_get_field(allFields, trim(field_name), field_1d_real, timelevel=1)
               dimNames(1:nDims) = field_1d_real % dimNames
               if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                  continue
               else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                  continue
               else
                  if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                  cycle
               end if
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
               if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                  continue
               else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                  continue
               else
                  if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                  cycle
               end if
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
               if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nCells') then
                  continue
               else if (trim(dimNames(mpas_pool_field_info%nDims)) == 'nVertices') then
                  continue
               else
                  if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(mpas_pool_field_info%nDims)), ' ', trim(field_name)
                  cycle
               end if

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
                  end do  ! k = 1, size(field_3d_real % constituentNames)
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
               call mpas_log_write(subname//' Unsupported field rank $i', MPAS_LOG_WARN, intArgs=[mpas_pool_field_info % ndims])
               cycle
            end select
         case default
            call mpas_log_write(subname//' Unsupported field type (Must be one of: integer, real)', MPAS_LOG_WARN)
            cycle
         end select

      end do

   end subroutine ufs_mpas_update_output_bundle


   subroutine ufs_mpas_create_restart_bundle(output_bundle, rc)

      use mpas_derived_types,  only : MPAS_stream_list_type, field_list_type
      use mpas_stream_list,    only : MPAS_stream_list_query
      use mpas_log,            only : mpas_log_write
      use mpas_pool_routines,  only : pool_print_members
      use mpas_attlist,        only : att_list_type, att_lists_type, &
                                      MPAS_ATT_INT, MPAS_ATT_REAL, MPAS_ATT_TEXT
      use mpas_derived_types,  only : MPAS_stream_list_type, MPAS_Pool_type, MPAS_Pool_iterator_type, mpas_pool_field_info_type, block_type,  &
                                      field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                      field3DInteger, field2DInteger, field1DInteger, field0DInteger, &
                                      field1DChar, field0DChar, &
                                      MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, MPAS_POOL_LOGICAL, &
                                      MPAS_POOL_SILENT, MPAS_POOL_CONFIG
      use mpas_pool_routines,  only : mpas_pool_get_next_member, mpas_pool_get_config, mpas_pool_get_error_level, &
                                      mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_set_error_level, &
                                      mpas_pool_begin_iteration, mpas_pool_get_dimension
      use mpas_stream_manager, only : prewrite_reindex, postwrite_reindex

      type(ESMF_FieldBundle), intent(out) :: output_bundle
      integer, intent(out)                :: rc


      integer :: localpet, nprocs

      type(MPAS_stream_list_type), pointer :: stream
      type(MPAS_Pool_type), pointer :: allFields
      type(MPAS_Pool_type), pointer :: allPackages
      integer :: timeLevelIn
      integer :: ierr

      type(ESMF_Mesh) :: mesh
      type(ESMF_Field) :: field
      type(ESMF_Info) :: field_info, bundle_info
      type(ESMF_TypeKind_Flag) :: rkind_typekind  ! Default ESMF real typekind that corresponds to default MPAS real kind

      real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:)  ! Default MPAS real kind
      integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:), ptr_i4_d2(:,:)

      type(MPAS_Pool_iterator_type) :: itr
      type(mpas_pool_field_info_type) :: info
      integer :: timeLevel

      type(field3DReal), pointer :: real3d
      type(field2DReal), pointer :: real2d
      type(field1DReal), pointer :: real1d
      type(field0DReal), pointer :: real0d

      type(field3DInteger), pointer :: int3d
      type(field2DInteger), pointer :: int2d
      type(field1DInteger), pointer :: int1d
      type(field0DInteger), pointer :: int0d

      type(field1DChar), pointer :: char1d
      type(field0DChar), pointer :: char0d

      integer, pointer :: intAtt
      logical, pointer :: logAtt
      character(len=StrKIND), pointer :: charAtt
      real(kind=RKIND), pointer :: realAtt

      character(len=StrKIND), pointer :: packages
      logical :: active_field
      integer :: err_level

      type(block_type), pointer :: block
      character(len=StrKIND), dimension(5+1) :: dimNames  ! +1 for Time
      type(att_lists_type), dimension(:), pointer :: attLists
      integer :: i, j, k, total_unique, nDims, dimSize
      integer, pointer :: dimSize_ptr
      logical :: hasTimeDimension
      logical :: is_unique
      logical :: isVarArray
      character(len=8) :: typeName
      character(len=64) :: decomp_dim_name
      character(len=64), allocatable :: dimension_names(:)
      character(len=64) :: variable_names(3000)
      character(len=256), allocatable :: global_att_names(:)
      integer :: numAtts
      integer :: numVars

      type :: dim_info_t
         character(64) :: dimName
         integer :: dimSize
      end type dim_info_t

      type(dim_info_t), allocatable :: dim_info_arr(:)

      rc = 0

      block => null()

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
      FIELD_LOOP: do while (mpas_pool_get_next_member(stream % field_pool, itr))

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
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='real', rc=rc); ESMF_ERR(rc)
                  else if (RKIND == R8KIND) then
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='double', rc=rc); ESMF_ERR(rc)
                  end if
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

               case (1)
                  call mpas_pool_get_field(allFields, itr % memberName, real1d, timeLevel)
                  hasTimeDimension = real1d % hasTimeDimension
                  attLists => real1d % attLists
                  dimNames(1:info%nDims) = real1d % dimNames
                  block => real1d % block

                  if (trim(dimNames(info%nDims)) == 'nCells') then
                     field = ESMF_FieldCreate(mesh, rkind_typekind, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                     ptr_rm_d1 = real1d % array(1:nCellsSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_rm_d1)
                  else if (trim(dimNames(info%nDims)) == 'nVertices') then
                     field = ESMF_FieldCreate(mesh, rkind_typekind, meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                     ptr_rm_d1 = real1d % array(1:nVerticesSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_rm_d1)
                  else if (trim(dimNames(info%nDims)) == 'nEdges') then
                     if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                     cycle FIELD_LOOP
                  else  ! Field has no distributed dimension
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=real1d % array, rc=rc); ESMF_ERR(rc)
                     if (RKIND == R4KIND) then
                        call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='real', rc=rc); ESMF_ERR(rc)
                     else if (RKIND == R8KIND) then
                        call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='double', rc=rc); ESMF_ERR(rc)
                     end if
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
                  end if

               case (2)
                  call mpas_pool_get_field(allFields, itr % memberName, real2d, timeLevel)
                  hasTimeDimension = real2d % hasTimeDimension
                  attLists => real2d % attLists
                  dimNames(1:info%nDims) = real2d % dimNames
                  block => real2d % block

                  if (trim(dimNames(info%nDims)) == 'nCells') then
                     field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(real2d%array,dim=1)], &
                                              meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                     ptr_rm_d2 = real2d % array(:,1:nCellsSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_rm_d2)
                  else if (trim(dimNames(info%nDims)) == 'nVertices') then
                     field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(real2d%array,dim=1)], &
                                              meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                     ptr_rm_d2 = real2d % array(:,1:nVerticesSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
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
                  decomp_dim_name = trim(real3d % dimNames(info%nDims))  ! save decomp name before stripping num_scalar

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
                           field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(real3d%array,dim=2)], &
                                                    meshloc=ESMF_MESHLOC_ELEMENT, name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d2 = real3d % array(k,:,1:nCellsSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                           nullify(ptr_rm_d2)
                        else if (trim(decomp_dim_name) == 'nVertices') then
                           field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(real3d%array,dim=2)], &
                                                    meshloc=ESMF_MESHLOC_NODE, name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldGet(field, farrayPtr=ptr_rm_d2, rc=rc); ESMF_ERR(rc)
                           ptr_rm_d2 = real3d % array(k,:,1:nVerticesSolve)

                           call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                           call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                           call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
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
                           else if (trim(dimNames(i)) == 'nVertices') then
                              dimSize = nVerticesGlobal
                           else if (trim(dimNames(i)) == 'StrLen') then
                              dimSize = 64
                           else if (trim(dimNames(i)) == 'Time') then
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
                     dimNames(1:info%nDims) = real3d % dimNames
                     if (trim(dimNames(info%nDims)) == 'nCells') then
                        field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [3], ungriddedLBound=[1,1], ungriddedUBound=[size(real3d%array,dim=1), size(real3d%array,dim=2)], &
                                                 meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                        call ESMF_FieldGet(field, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                        ptr_rm_d3 = real3d % array(:,:,1:nCellsSolve)

                        call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                        call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                        call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                        nullify(ptr_rm_d3)
                     else if (trim(dimNames(info%nDims)) == 'nVertices') then
                        field = ESMF_FieldCreate(mesh, rkind_typekind, gridToFieldMap = [3], ungriddedLBound=[1,1], ungriddedUBound=[size(real3d%array,dim=1), size(real3d%array,dim=2)], &
                                                 meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                        call ESMF_FieldGet(field, farrayPtr=ptr_rm_d3, rc=rc); ESMF_ERR(rc)
                        ptr_rm_d3 = real3d % array(:,:,1:nVerticesSolve)

                        call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                        call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                        call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                        nullify(ptr_rm_d3)
                     else if (trim(dimNames(info%nDims)) == 'nEdges') then
                        if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                        cycle FIELD_LOOP
                     end if
                  end if

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for real field: ', trim(itr % memberName)
                  ESMF_ERR(1)
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
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='int', rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
               case (1)
                  call mpas_pool_get_field(allFields, itr % memberName, int1d, timeLevel)
                  hasTimeDimension = int1d % hasTimeDimension
                  attLists => int1d % attLists
                  dimNames(1:info%nDims) = int1d % dimNames
                  block => int1d % block

                  if (trim(dimNames(info%nDims)) == 'nCells') then
                     field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                     ptr_i4_d1 = int1d % array(1:nCellsSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_i4_d1)
                  else if (trim(dimNames(info%nDims)) == 'nVertices') then
                     field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_i4_d1, rc=rc); ESMF_ERR(rc)
                     ptr_i4_d1 = int1d % array(1:nVerticesSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_i4_d1)
                  else if (trim(dimNames(info%nDims)) == 'nEdges') then
                     if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                     cycle FIELD_LOOP
                  end if

               case (2)
                  call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)
                  hasTimeDimension = int2d % hasTimeDimension
                  attLists => int2d % attLists
                  dimNames(1:info%nDims) = int2d % dimNames
                  block => int2d % block

                  if (trim(dimNames(info%nDims)) == 'nCells') then
                     field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(int2d%array,dim=1)], &
                                              meshloc=ESMF_MESHLOC_ELEMENT, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
                     ptr_i4_d2 = int2d%array(:,1:nCellsSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_i4_d2)
                  else if (trim(dimNames(info%nDims)) == 'nVertices') then
                     field = ESMF_FieldCreate(mesh, ESMF_TYPEKIND_I4, gridToFieldMap = [2], ungriddedLBound=[1], ungriddedUBound=[size(int2d%array,dim=1)], &
                                              meshloc=ESMF_MESHLOC_NODE, name=trim(itr % memberName), rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldGet(field, farrayPtr=ptr_i4_d2, rc=rc); ESMF_ERR(rc)
                     ptr_i4_d2 = int2d% array(:,1:nVerticesSolve)

                     call ESMF_InfoGetFromHost(field, info=field_info, rc=rc); ESMF_ERR(rc)
                     call ESMF_InfoSet(field_info, key='/NetCDF/FV3/output_file', value='restart_mpas', rc=rc); ESMF_ERR(rc)
                     call ESMF_FieldBundleAdd(output_bundle,[field], rc=rc); ESMF_ERR(rc)
                     nullify(ptr_i4_d2)
                  else if (trim(dimNames(info%nDims)) == 'nEdges') then
                     if (localpet == 0) write(0,*)'Unsupported dim: ', trim(dimNames(info%nDims)), ' ', trim(itr % memberName)
                     cycle FIELD_LOOP
                  end if

               case (3)
                  call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)
                  hasTimeDimension = int3d % hasTimeDimension
                  attLists => int3d % attLists
                  dimNames(1:info%nDims) = int3d % dimNames
                  block => int3d % block

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for integer field: ', trim(itr % memberName)
                  ESMF_ERR(1)
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
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_type', value='char', rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

               case (1)
                  call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                  hasTimeDimension = char1d % hasTimeDimension
                  attLists => char1d % attLists
                  dimNames(1:info%nDims) = char1d % dimNames
                  nDims = nDims + 1
                  dimNames(nDims:nDims) = 'StrLen'
                  block => char1d % block

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for character field: ', trim(itr % memberName)
                  ESMF_ERR(1)
               end select

            case default
               write(0,*)'ERROR: Unsupported fieldType: ', info % fieldType, ' for field: ', trim(itr % memberName)
               ESMF_ERR(1)
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
                  else if (trim(dimNames(i)) == 'nVertices') then
                     dimSize = nVerticesGlobal
                  else if (trim(dimNames(i)) == 'StrLen') then
                     dimSize = 64
                  else if (trim(dimNames(i)) == 'Time') then
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
            end if  ! .not. isVarArray

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
         if (itr % memberType == MPAS_POOL_CONFIG) then
            numAtts = numAtts + 1
         end if
      end do

      allocate(global_att_names(numAtts))
      global_att_names = ''
      numAtts = 0

      call mpas_pool_begin_iteration(stream % att_pool)
      do while (mpas_pool_get_next_member(stream % att_pool, itr))
         if (itr % memberType == MPAS_POOL_CONFIG) then
            numAtts = numAtts + 1
            global_att_names(numAtts)=trim(itr % memberName)
            if (itr % dataType == MPAS_POOL_REAL) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, realAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', realAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=realAtt, rc=rc); ESMF_ERR(rc)

            else if (itr % dataType == MPAS_POOL_INTEGER) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, intAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', intAtt
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=intAtt, rc=rc); ESMF_ERR(rc)

            else if (itr % dataType == MPAS_POOL_CHARACTER) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, charAtt)
               ! write(0,*)'config: ', trim(itr % memberName), ' ', trim(charAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=trim(charAtt), rc=rc); ESMF_ERR(rc)
            else if (itr % dataType == MPAS_POOL_LOGICAL) then
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
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/grid_id', value=1, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/output_grid', value='restart_grid', rc=rc); ESMF_ERR(rc)

      call postwrite_reindex(allFields, stream % field_pool)

   contains

      ! subroutine add_field_to_bundle(varName, attList)

      !    character(len=*), intent(in) :: varName
      !    type(att_list_type), pointer, intent(in) :: attList

      !    integer :: i
      !    logical :: is_unique
      !    integer, pointer :: dimSize_ptr

      !    call ESMF_InfoSet(field_info, key="/NetCDF/FV3/output_file", value="atm", rc=rc); ESMF_ERR(rc)

      !    call ESMF_FieldBundleAdd(output_bundle,(/field/), rc=rc); ESMF_ERR(rc)

      ! end subroutine add_field_to_bundle

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

         type(att_list_type), pointer, intent(in) :: attList
         character(len=*), intent(in) :: varName
         integer, intent(out) :: rc

         character(len=64) :: att_names(256)
         integer :: numAtts
         type(att_list_type), pointer :: att_cursor

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

      type(mpas_pool_type), intent(in) :: package_pool
      character(len=*), intent(in) :: packages
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
      use mpas_dmpar,         only : mpas_dmpar_sum_int_array, mpas_dmpar_bcast_ints, IO_NODE

      type(ESMF_Mesh), intent(out) :: mesh
      integer, intent(out) :: rc

      logical :: mesh_is_created
      integer :: numNodes
      integer, dimension(:), allocatable :: nodeIds, nodeOwners
      real(ESMF_KIND_R8), dimension(:), allocatable :: nodeCoords

      integer :: numElems, numElemsConn, ielemConn
      integer, dimension(:), allocatable :: elemIds, elemTypes, elemConn, locNodeID
      real(ESMF_KIND_R8), dimension(:), allocatable :: elemCoords

      type(mpas_pool_type), pointer :: meshPool
      integer, dimension(:), pointer :: indexToCellID, indexToVertexID
      integer, dimension(:), pointer :: nEdgesOnCell
      real(kind=RKIND), dimension(:), pointer :: latCell, lonCell, latVertex, lonVertex
      integer, dimension(:,:), pointer :: cellsOnVertex, verticesOnCell
      integer, dimension(:), pointer :: nCellsOwnedIndices, nEdgesOwnedIndices, nVerticesOwnedIndices
      integer, pointer :: nCells, nVertices, vertexDegree
      integer :: i, j, localpet, nprocs, iloc, istat
      integer, dimension(:), pointer :: part_ids
      integer :: nVertex_on_pet
      character(len=StrKIND), pointer :: config_block_decomp_file_prefix
      character(len=StrKIND) :: fname

      integer, allocatable :: verticesOwnedByThisPET(:), new_verticesOwnedByThisPET(:)

      rc = 0

      mesh_is_created = ESMF_MeshIsCreated(the_mpas_esmf_mesh, rc=rc); ESMF_ERR(rc)
      if (mesh_is_created) then
         mesh = the_mpas_esmf_mesh
         return
      end if

      localpet = domain_ptr % dminfo % my_proc_id
      nprocs = domain_ptr % dminfo % nprocs

      call mpas_pool_get_config(domain_ptr % blocklist % configs, 'config_block_decomp_file_prefix', config_block_decomp_file_prefix)
      write(fname,'(A,I0)') trim(config_block_decomp_file_prefix), nprocs

      allocate(part_ids(nCellsGlobal))
      if (localpet == IO_NODE) then
         open(unit=10,file=trim(fname), status='old', action='read', iostat=istat)
         if (istat /= 0) then
            write(0,*)'FIXME: ', trim(fname), ' is not present'
            stop 1  !FIXME
         end if
         do i =1,nCellsGlobal
            read(10,*,iostat=istat) part_ids(i)
            if (istat /= 0) then
               write(0,*)'FIXME: error reading ', trim(fname), ' line = ', j
               stop 1  !FIXME
            end if
         end do
         close(10)
      end if
      call mpas_dmpar_bcast_ints(domain_ptr % dminfo, nCellsGlobal, part_ids)

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
      call mpas_dmpar_sum_int_array(domain_ptr % dminfo, nVerticesGlobal, verticesOwnedByThisPET, new_verticesOwnedByThisPET)
      verticesOwnedByThisPET = new_verticesOwnedByThisPET - 1

      nVertex_on_pet = 0
      do i = 1, nVertices
         do j = 1, vertexDegree
            if (indexToCellID(cellsOnVertex(j,i)) > 0) then
               if (localpet == part_ids(indexToCellID(cellsOnVertex(j,i)))) then
                  nVertex_on_pet = nVertex_on_pet + 1
                  goto 90
               end if
            end if
         end do
90       continue
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
                  goto 92
               end if
            end if
         end do
92       continue
      end do
      ASSERT(iloc == nVertex_on_pet)

      allocate(elemConn(numElemsConn))
      ielemConn = 0
      do i = 1, nCellsSolve
         do j = 1, nEdgesOnCell(i)
            ielemConn = ielemConn + 1
            ASSERT (locNodeID(indexToVertexID(verticesOnCell(j,i))) > 0)
            elemConn(ielemConn) = locNodeID(indexToVertexID(verticesOnCell(j,i)))
         end do
      end do
      ASSERT(ielemConn == numElemsConn)
      ! write(0,*) 'numElemsConn = ', numElemsConn

      the_mpas_esmf_mesh = ESMF_MeshCreate(parametricDim=2, &
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

      mesh = the_mpas_esmf_mesh

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

      type(ESMF_Field) :: field
      type(ESMF_Info) :: bundle_info

      type(mpaS_stream_list_type), pointer :: stream
      type(mpas_pool_iterator_type) :: itr

      real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:)  ! Default MPAS real kind
      integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:)

      integer :: k, ierr
      character(len=ESMF_MAXSTR) :: fieldName
      logical :: IsPresent
      type(MPAS_Pool_type), pointer :: allFields
      type(MPAS_Pool_type), pointer :: allPackages
      integer :: timeLevelIn

      type(mpas_pool_field_info_type) :: info
      integer :: timeLevel

      type(field3DReal), pointer :: real3d
      type(field2DReal), pointer :: real2d
      type(field1DReal), pointer :: real1d
      type(field0DReal), pointer :: real0d

      type(field3DInteger), pointer :: int3d
      type(field2DInteger), pointer :: int2d
      type(field1DInteger), pointer :: int1d
      type(field0DInteger), pointer :: int0d

      type(field1DChar), pointer :: char1d
      type(field0DChar), pointer :: char0d

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

      call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

      call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

      call mpas_pool_begin_iteration(stream % field_pool)
      FIELD_LOOP: do while (mpas_pool_get_next_member(stream % field_pool, itr))

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

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for real field: ', trim(itr % memberName)
                  ESMF_ERR(1)
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
                  else  ! Array has no distributed dimension
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=int1d % array, rc=rc); ESMF_ERR(rc)
                  end if

               case (2)
                  call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)
                  write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

               case (3)
                  call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)
                  write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for integer field: ', trim(itr % memberName)
                  ESMF_ERR(1)
               end select

            case (MPAS_POOL_CHARACTER)
               select case (info % nDims)
               case (0)
                  call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)

               case (1)
                  call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)
                  write(0,'(A,A,A,I0,A)') 'FIXME: ',__FILE__,':',__LINE__, ' unimplemented'

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for character field: ', trim(itr % memberName)
                  ESMF_ERR(1)
               end select

            case default
               write(0,*)'ERROR: Unsupported fieldType: ', info % fieldType, ' for field: ', trim(itr % memberName)
               ESMF_ERR(1)
            end select

         end if

      end do FIELD_LOOP

      call postwrite_reindex(allFields, stream % field_pool)

   end subroutine ufs_mpas_update_restart_bundle

   subroutine ufs_mpas_create_restart_array_bundle(output_bundle, bundle_name, stream_name, rc)

      use mpas_derived_types,  only : MPAS_stream_list_type, field_list_type
      use mpas_stream_list,    only : MPAS_stream_list_query
      use mpas_log,            only : mpas_log_write
      use mpas_pool_routines,  only : pool_print_members
      use mpas_attlist,        only : att_list_type, att_lists_type, &
                                      MPAS_ATT_INT, MPAS_ATT_REAL, MPAS_ATT_TEXT
      use mpas_derived_types,  only : MPAS_stream_list_type, MPAS_Pool_type, MPAS_Pool_iterator_type, mpas_pool_field_info_type, block_type,  &
                                      field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                      field3DInteger, field2DInteger, field1DInteger, field0DInteger, &
                                      field1DChar, field0DChar, &
                                      MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, MPAS_POOL_LOGICAL, &
                                      MPAS_POOL_SILENT, MPAS_POOL_CONFIG
      use mpas_pool_routines,  only : mpas_pool_get_next_member, mpas_pool_get_config, mpas_pool_get_error_level, &
                                      mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_set_error_level, &
                                      mpas_pool_begin_iteration, mpas_pool_get_dimension,mpas_pool_get_subpool
      use mpas_stream_manager, only : prewrite_reindex, postwrite_reindex

      implicit none

      type(ESMF_ArrayBundle), intent(out) :: output_bundle
      character(len=*), intent(in) :: bundle_name
      character(len=*), intent(in) :: stream_name
      integer, intent(out) :: rc

      ! Local
      type(MPAS_stream_list_type), pointer :: stream
      type(MPAS_Pool_type), pointer :: allFields
      type(MPAS_Pool_type), pointer :: allPackages
      integer :: timeLevelIn

      type(ESMF_Array) :: array
      type(ESMF_Info) :: bundle_info
      type(ESMF_TypeKind_Flag) :: rkind_typekind  ! Default ESMF real typekind that corresponds to default MPAS real kind

      real(RKIND), pointer           :: ptr_rm_d1(:)  ! Default MPAS real kind

      type(MPAS_Pool_iterator_type) :: itr
      type(mpas_pool_field_info_type) :: info
      integer :: timeLevel

      type(field3DReal), pointer :: real3d
      type(field2DReal), pointer :: real2d
      type(field1DReal), pointer :: real1d
      type(field0DReal), pointer :: real0d

      type(field3DInteger), pointer :: int3d
      type(field2DInteger), pointer :: int2d
      type(field1DInteger), pointer :: int1d
      type(field0DInteger), pointer :: int0d

      type(field1DChar), pointer :: char1d
      type(field0DChar), pointer :: char0d

      integer, pointer :: intAtt
      logical, pointer :: logAtt
      character(len=StrKIND), pointer :: charAtt
      real(kind=RKIND), pointer :: realAtt

      character(len=StrKIND), pointer :: packages
      logical :: active_field
      integer :: err_level

      type(block_type), pointer :: block
      character(len=StrKIND), dimension(5+1) :: dimNames  ! +1 for Time
      type(att_lists_type), dimension(:), pointer :: attLists
      integer :: i, j, k, total_unique, nDims, dimSize
      integer, pointer :: dimSize_ptr
      logical :: hasTimeDimension
      logical :: is_unique
      logical :: isVarArray
      character(len=8) :: typeName
      integer :: frestart(1)
      character(len=64) :: decomp_dim_name
      character(len=64), allocatable :: dimension_names(:)
      character(len=64) :: variable_names(3000)
      character(len=256), allocatable :: global_att_names(:)
      integer :: numVars
      integer :: numGlobalAtts

      type :: dim_info_t
         character(64) :: dimName
         integer :: dimSize
      end type dim_info_t

      type(dim_info_t), allocatable :: dim_info_arr(:)

      integer :: ierr
      integer :: nprocs, localpet, minIndexPTileCells, maxIndexPTileCells, dimCount, deCount
#ifdef MPAS_USE_MPI_F08
      type(mpi_comm) :: comm
#else
      integer :: comm
#endif
      integer, allocatable :: cell_counts(:)
      integer, allocatable :: deBlockList(:,:,:)
      type(ESMF_DistGrid) :: distgridCells, distgridVertices, distgridEdges
      integer :: nSolve, nGlobal

      integer, parameter :: max_fields_to_write = 1000
      character(len=64), dimension(max_fields_to_write) :: fields_to_write
      character(len=64) :: var_name
      integer :: n, nfields
      logical :: indexToCellID_present, indexToVertexID_present, indexToEdgeID_present

      rc = 0

      block => null()
      frestart(:) = -1

      nullify(stream)
      if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, trim(stream_name), stream, ierr=ierr)) then
         write(0,*)'ERROR, Unknown stream name: ', trim(stream_name)
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

      call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

      localpet = domain_ptr % dminfo % my_proc_id
      nprocs = domain_ptr % dminfo % nprocs
      comm = domain_ptr % dminfo % comm

      allocate(cell_counts(nprocs))
      dimCount = 1
      deCount = nprocs
      allocate(deBlockList(dimCount, 2, deCount))  ! (dimCount, 2, deCount)

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

      distgridCells = ESMF_DistGridCreate(minIndex=[1], maxIndex=[nGlobal], &
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

      distgridVertices = ESMF_DistGridCreate(minIndex=[1], maxIndex=[nGlobal], &
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

      distgridEdges = ESMF_DistGridCreate(minIndex=[1], maxIndex=[nGlobal], &
                                          deBlockList=deBlockList, rc=rc); ESMF_ERR(rc)

      output_bundle = ESMF_ArrayBundleCreate(name=trim(bundle_name), rc=rc); ESMF_ERR(rc)

      call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

      allocate(dim_info_arr(0))

      variable_names = ''
      numVars = 0

      nfields = 0
      indexToCellID_present = .false.
      indexToVertexID_present = .false.
      indexToEdgeID_present = .false.
      call mpas_pool_begin_iteration(stream % field_pool)
      SEARCH_LOOP: do while (mpas_pool_get_next_member(stream % field_pool, itr))

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

            if (.not. active_field) cycle SEARCH_LOOP

            nfields = nfields + 1

            fields_to_write(nfields) = trim(itr % memberName)

            if (trim(itr % memberName) == 'indexToCellID')   indexToCellID_present =.true.
            if (trim(itr % memberName) == 'indexToVertexID') indexToVertexID_present =.true.
            if (trim(itr % memberName) == 'indexToEdgeID')   indexToEdgeID_present =.true.
         end if

      end do SEARCH_LOOP

      ! Add three indexTo....ID fields explicitly because they are needed in write routines
      ! to specify arrays decomposition
      if (.not. indexToCellID_present) then
         nfields = nfields + 1
         fields_to_write(nfields) = 'indexToCellID'
      end if

      if (.not. indexToVertexID_present) then
         nfields = nfields + 1
         fields_to_write(nfields) = 'indexToVertexID'
      end if

      if (.not. indexToEdgeID_present) then
         nfields = nfields + 1
         fields_to_write(nfields) = 'indexToEdgeID'
      end if


      do n = 1, nfields

         var_name = fields_to_write(n)

         info % fieldType = -1

         call mpas_pool_get_field_info(allFields, var_name, info)

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
               call mpas_pool_get_field(allFields, var_name, real0d, timeLevel)
               hasTimeDimension = real0d % hasTimeDimension
               attLists => real0d % attLists
               ! no dimNames in 0d
               block => real0d % block

               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name), value=real0d % scalar, rc=rc); ESMF_ERR(rc)
               if (RKIND == R4KIND) then
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='real', rc=rc); ESMF_ERR(rc)
               else if (RKIND == R8KIND) then
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='double', rc=rc); ESMF_ERR(rc)
               end if
               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

            case (1)
               call mpas_pool_get_field(allFields, var_name, real1d, timeLevel)
               hasTimeDimension = real1d % hasTimeDimension
               attLists => real1d % attLists
               dimNames(1:info%nDims) = real1d % dimNames
               block => real1d % block

               if (trim(dimNames(1)) == 'nCells') then
                  array = ESMF_ArrayCreate(distgridCells, rkind_typekind, name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                  ptr_rm_d1 = real1d % array(1:nCellsSolve)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  nullify(ptr_rm_d1)
               else if (trim(dimNames(1)) == 'nVertices') then
                  array = ESMF_ArrayCreate(distgridVertices, rkind_typekind, name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                  ptr_rm_d1 = real1d % array(1:nVerticesSolve)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  nullify(ptr_rm_d1)
               else if (trim(dimNames(1)) == 'nEdges') then
                  array = ESMF_ArrayCreate(distgridEdges, rkind_typekind, name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayGet(array, farrayPtr=ptr_rm_d1, rc=rc); ESMF_ERR(rc)
                  ptr_rm_d1 = real1d % array(1:nEdgesSolve)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  nullify(ptr_rm_d1)
               else  ! Array has no distributed dimension
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name), values=real1d % array, rc=rc); ESMF_ERR(rc)
                  if (RKIND == R4KIND) then
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='real', rc=rc); ESMF_ERR(rc)
                  else if (RKIND == R8KIND) then
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='double', rc=rc); ESMF_ERR(rc)
                  end if
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
               end if

            case (2)
               call mpas_pool_get_field(allFields, var_name, real2d, timeLevel)
               hasTimeDimension = real2d % hasTimeDimension
               attLists => real2d % attLists
               dimNames(1:info%nDims) = real2d % dimNames
               block => real2d % block

               if (trim(dimNames(info%nDims)) == 'nCells') then
                  array = ESMF_ArrayCreate(distgridCells, farray=real2d % array(:,1:nCellsSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               else if (trim(dimNames(info%nDims)) == 'nVertices') then
                  array = ESMF_ArrayCreate(distgridVertices, farray=real2d % array(:,1:nVerticesSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               else if (trim(dimNames(info%nDims)) == 'nEdges') then
                  array = ESMF_ArrayCreate(distgridEdges, farray=real2d % array(:,1:nEdgesSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               end if

            case (3)
               call mpas_pool_get_field(allFields, var_name, real3d, timeLevel)
               hasTimeDimension = real3d % hasTimeDimension
               attLists => real3d % attLists
               block => real3d % block
               decomp_dim_name = trim(real3d % dimNames(info%nDims))  ! save decomp name before stripping num_scalar

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
                                                 distgridToArrayMap = [2], &
                                                 indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                 name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                        call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                     else if (trim(decomp_dim_name) == 'nVertices') then
                        array = ESMF_ArrayCreate(distgridVertices, farray=real3d % array(k,:,1:nVerticesSolve), &
                                                 distgridToArrayMap = [2], &
                                                 indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                 name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                        call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                     else if (trim(decomp_dim_name) == 'nEdges') then
                        array = ESMF_ArrayCreate(distgridEdges, farray=real3d % array(k,:,1:nEdgesSolve), &
                                                 distgridToArrayMap = [2], &
                                                 indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                                 name=trim(real3d % constituentNames(k)), rc=rc); ESMF_ERR(rc)
                        call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
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
                        else if (trim(dimNames(i)) == 'nVertices') then
                           dimSize = nVerticesGlobal
                        else if (trim(dimNames(i)) == 'StrLen') then
                           dimSize = 64
                        else if (trim(dimNames(i)) == 'Time') then
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

                  dimNames(1:info%nDims) = real3d % dimNames
                  if (trim(dimNames(info%nDims)) == 'nCells') then
                     array = ESMF_ArrayCreate(distgridCells, farray=real3d % array(:,:,1:nCellsSolve), &
                                              distgridToArrayMap = [3], &
                                              indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                              name=trim(var_name), rc=rc); ESMF_ERR(rc)
                     call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  else if (trim(dimNames(info%nDims)) == 'nVertices') then
                     array = ESMF_ArrayCreate(distgridVertices, farray=real3d % array(:,:,1:nVerticesSolve), &
                                              distgridToArrayMap = [3], &
                                              indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                              name=trim(var_name), rc=rc); ESMF_ERR(rc)
                     call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  else if (trim(dimNames(info%nDims)) == 'nEdges') then
                     array = ESMF_ArrayCreate(distgridEdges, farray=real3d % array(:,:,1:nEdgesSolve), &
                                              distgridToArrayMap = [3], &
                                              indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                              name=trim(var_name), rc=rc); ESMF_ERR(rc)
                     call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
                  end if
               end if

            case default
               write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for real field: ', trim(var_name)
               ESMF_ERR(1)
            end select

         case (MPAS_POOL_INTEGER)
            typeName = 'int'
            select case (info % nDims)
            case (0)
               call mpas_pool_get_field(allFields, var_name, int0d, timeLevel)
               hasTimeDimension = int0d % hasTimeDimension
               attLists => int0d % attLists
               ! no dimNames in 0d
               block => int0d % block

               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name), value=int0d % scalar, rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='int', rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
            case (1)
               call mpas_pool_get_field(allFields, var_name, int1d, timeLevel)
               hasTimeDimension = int1d % hasTimeDimension
               attLists => int1d % attLists
               dimNames(1:info%nDims) = int1d % dimNames
               block => int1d % block

               if (trim(dimNames(info%nDims)) == 'nCells') then
                  array = ESMF_ArrayCreate(distgridCells, farray=int1d % array(1:nCellsSolve), &
                                           distgridToArrayMap = [1], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)

               else if (trim(dimNames(info%nDims)) == 'nVertices') then
                  array = ESMF_ArrayCreate(distgridVertices, farray=int1d % array(1:nVerticesSolve), &
                                           distgridToArrayMap = [1], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)

               else if (trim(dimNames(info%nDims)) == 'nEdges') then
                  array = ESMF_ArrayCreate(distgridEdges, farray=int1d % array(1:nEdgesSolve), &
                                           distgridToArrayMap = [1], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)

               else  ! Array has no distributed dimension
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name), values=int1d % array, rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='int', rc=rc); ESMF_ERR(rc)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)
               end if

            case (2)
               call mpas_pool_get_field(allFields, var_name, int2d, timeLevel)
               hasTimeDimension = int2d % hasTimeDimension
               attLists => int2d % attLists
               dimNames(1:info%nDims) = int2d % dimNames
               block => int2d % block

               if (trim(dimNames(info%nDims)) == 'nCells') then
                  array = ESMF_ArrayCreate(distgridCells, farray=int2d % array(:,1:nCellsSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               else if (trim(dimNames(info%nDims)) == 'nVertices') then
                  array = ESMF_ArrayCreate(distgridVertices, farray=int2d % array(:,1:nVerticesSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               else if (trim(dimNames(info%nDims)) == 'nEdges') then
                  array = ESMF_ArrayCreate(distgridEdges, farray=int2d % array(:,1:nEdgesSolve), &
                                           distgridToArrayMap = [2], &
                                           indexflag=ESMF_INDEX_DELOCAL, datacopyflag=ESMF_DATACOPY_VALUE, &
                                           name=trim(var_name), rc=rc); ESMF_ERR(rc)
                  call ESMF_ArrayBundleAdd(output_bundle,[array], rc=rc); ESMF_ERR(rc)
               end if

            case (3)
               call mpas_pool_get_field(allFields, var_name, int3d, timeLevel)
               hasTimeDimension = int3d % hasTimeDimension
               attLists => int3d % attLists
               dimNames(1:info%nDims) = int3d % dimNames
               block => int3d % block

            case default
               write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for integer field: ', trim(var_name)
               ESMF_ERR(1)
            end select

         case (MPAS_POOL_CHARACTER)
            typeName = 'char'
            select case (info % nDims)
            case (0)
               call mpas_pool_get_field(allFields, var_name, char0d, timeLevel)
               hasTimeDimension = char0d % hasTimeDimension
               attLists => char0d % attLists
               ! no dimNames in 0d
               nDims = nDims + 1
               dimNames(nDims:nDims) = 'StrLen'
               block => char0d % block

               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_type', value='char', rc=rc); ESMF_ERR(rc)
               call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(var_name)//'_rank', value=info % nDims, rc=rc); ESMF_ERR(rc)

            case (1)
               call mpas_pool_get_field(allFields, var_name, char1d, timeLevel)
               hasTimeDimension = char1d % hasTimeDimension
               attLists => char1d % attLists
               dimNames(1:info%nDims) = char1d % dimNames
               nDims = nDims + 1
               dimNames(nDims:nDims) = 'StrLen'
               block => char1d % block

            case default
               write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for characeter field: ', trim(var_name)
               ESMF_ERR(1)
            end select

         case default
            write(0,*)'ERROR: Unsupported fieldType: ', info % fieldType, ' for field: ', trim(var_name)
            ESMF_ERR(1)
         end select

         if (.not. isVarArray) then

            if (hasTimeDimension) then
               nDims = nDims + 1
               dimNames(nDims:nDims) = 'Time'
            end if

            ! write(0,'(A,A,A,A,A)',advance='no') '   ',trim(typeName), ' ', trim(var_name),'('
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
               else if (trim(dimNames(i)) == 'nVertices') then
                  dimSize = nVerticesGlobal
               else if (trim(dimNames(i)) == 'StrLen') then
                  dimSize = 64
               else if (trim(dimNames(i)) == 'Time') then
                  dimSize = 1
               end if

               if (dimSize >= 0) then
                  is_unique = .true.
                  do j = 1, size(dim_info_arr)
                     if (trim(dimNames(i)) == trim(dim_info_arr(j) % dimName)) then
                        if (dimSize /= dim_info_arr(j) % dimSize) then
                           write(0,*)'conflictiing dimSize for ', trim(var_name), ' dimension ', trim(dimNames(i)), ' ', dimSize, dim_info_arr(j) % dimSize
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

            call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(var_name), values=dimNames(1:nDims), rc=rc); ESMF_ERR(rc)

            numVars = numVars + 1
            variable_names(numVars) = trim(var_name)

            call put_variable_attributes(attLists(1) % attList, trim(var_name), rc=rc); ESMF_ERR(rc)
         end if  ! .not. isVarArray

         nullify(attLists)

      end do  ! n = 1, nfields

      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variable_names', values=variable_names(1:numVars), rc=rc); ESMF_ERR(rc)

      ! dimensions attributes
      allocate(dimension_names(size(dim_info_arr)))
      do i = 1, size(dim_info_arr)
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
         if (itr % memberType == MPAS_POOL_CONFIG) then
            numGlobalAtts = numGlobalAtts + 1
         end if
      end do

      allocate(global_att_names(numGlobalAtts))
      global_att_names = ''
      numGlobalAtts = 0

      call mpas_pool_begin_iteration(stream % att_pool)
      do while (mpas_pool_get_next_member(stream % att_pool, itr))
         if (itr % memberType == MPAS_POOL_CONFIG) then
            numGlobalAtts = numGlobalAtts + 1
            global_att_names(numGlobalAtts)=trim(itr % memberName)
            if (itr % dataType == MPAS_POOL_REAL) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, realAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=realAtt, rc=rc); ESMF_ERR(rc)

            else if (itr % dataType == MPAS_POOL_INTEGER) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, intAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=intAtt, rc=rc); ESMF_ERR(rc)

            else if (itr % dataType == MPAS_POOL_CHARACTER) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, charAtt)
               call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value=trim(charAtt), rc=rc); ESMF_ERR(rc)
            else if (itr % dataType == MPAS_POOL_LOGICAL) then
               call mpas_pool_get_config(stream % att_pool, itr % memberName, logAtt)
               if (logAtt) then
                  call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='YES', rc=rc); ESMF_ERR(rc)
               else
                  call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att/'//trim(itr % memberName), value='NO', rc=rc); ESMF_ERR(rc)
               end if
            end if
         end if
      end do
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/global_att_names', values=global_att_names, rc=rc); ESMF_ERR(rc)

      ! bundle attributes
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/grid_id', value=1, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/frestart', values=frestart, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3-nooutput/output_grid', value='restart_grid', rc=rc); ESMF_ERR(rc)

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

         type(att_list_type), pointer, intent(in) :: attList
         character(len=*), intent(in) :: varName
         integer, intent(out) :: rc

         character(len=64) :: att_names(256)
         integer :: numAtts
         type(att_list_type), pointer :: att_cursor

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
               end if
               numAtts = numAtts + 1
               att_names(numAtts) = trim(att_cursor % attName)
            end if
            att_cursor => att_cursor % next
         end do

         call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//'_att_names', values=att_names(1:numAtts), rc=rc); ESMF_ERR(rc)

         nullify(att_cursor)

         if ((trim(varName) == 'indexToCellID' .and. .not. indexToCellID_present) .or. &
            (trim(varName) == 'indexToVertexID' .and. .not. indexToVertexID_present) .or. &
            (trim(varName) == 'indexToEdgeID' .and. .not. indexToEdgeID_present)) then
            call ESMF_InfoSet(bundle_info, key='/NetCDF/FV3/variables/'//trim(varName)//':'//'do_not_write', value='true', rc=rc); ESMF_ERR(rc)
         end if

      end subroutine put_variable_attributes

   end subroutine ufs_mpas_create_restart_array_bundle

   subroutine ufs_mpas_update_restart_array_bundle(output_bundle, stream_name, rc)

      use mpas_derived_types, only : mpas_pool_field_info_type, mpas_pool_real, mpas_pool_integer, &
                                     mpas_stream_list_type, mpas_pool_type, mpas_pool_iterator_type, &
                                     MPAS_POOL_SILENT, MPAS_POOL_CONFIG, MPAS_POOL_REAL, MPAS_POOL_INTEGER, MPAS_POOL_CHARACTER, &
                                     field5DReal, field4DReal, field3DReal, field2DReal, field1DReal, field0DReal, &
                                     field3DInteger, field2DInteger, field1DInteger, field0DInteger, field1DChar, field0DChar
      use mpas_pool_routines, only : pool_print_members, mpas_pool_get_field, mpas_pool_get_field_info, mpas_pool_get_subpool, &
                                     mpas_pool_get_config, mpas_pool_set_error_level, mpas_pool_get_error_level, &
                                     mpas_pool_get_next_member, mpas_pool_begin_iteration
      use mpas_stream_manager,only : prewrite_reindex, postwrite_reindex
      use mpas_stream_list,   only : MPAS_stream_list_query
      use mpas_log,           only : mpas_log_write

      type(ESMF_ArrayBundle), intent(inout) :: output_bundle
      character(len=*), intent(in) :: stream_name
      integer, intent(out) :: rc

      type(ESMF_Array) :: array
      type(ESMF_Info) :: bundle_info

      type(MPAS_stream_list_type), pointer :: stream
      type(mpas_pool_iterator_type) :: itr

      real(RKIND), pointer           :: ptr_rm_d1(:), ptr_rm_d2(:,:), ptr_rm_d3(:,:,:)  ! Default MPAS real kind
      integer(ESMF_KIND_I4), pointer :: ptr_i4_d1(:)

      integer :: k, ierr
      character(len=ESMF_MAXSTR) :: arrayName
      type(MPAS_Pool_type), pointer :: allFields
      type(MPAS_Pool_type), pointer :: allPackages
      integer :: timeLevelIn

      type(mpas_pool_field_info_type) :: info
      integer :: timeLevel

      type(field3DReal), pointer :: real3d
      type(field2DReal), pointer :: real2d
      type(field1DReal), pointer :: real1d
      type(field0DReal), pointer :: real0d

      type(field3DInteger), pointer :: int3d
      type(field2DInteger), pointer :: int2d
      type(field1DInteger), pointer :: int1d
      type(field0DInteger), pointer :: int0d

      type(field1DChar), pointer :: char1d
      type(field0DChar), pointer :: char0d

      character(len=StrKIND), pointer :: packages
      logical :: active_field
      integer :: err_level

      rc = 0

      nullify(stream)
      if (.not. MPAS_stream_list_query(domain_ptr % streamManager % streams, trim(stream_name), stream, ierr=ierr)) then
         rc = 1
         return
      end if

      allFields => domain_ptr % streamManager % allFields
      allPackages => domain_ptr % streamManager % allPackages
      timeLevelIn = 1

      call prewrite_reindex(allFields, allPackages, stream % field_pool, stream % field_pkg_pool)

      call ESMF_InfoGetFromHost(output_bundle, info=bundle_info, rc=rc); ESMF_ERR(rc)

      call mpas_pool_begin_iteration(stream % field_pool)
      FIELD_LOOP: do while (mpas_pool_get_next_member(stream % field_pool, itr))

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

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for real field: ', trim(itr % memberName)
                  ESMF_ERR(1)
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
                  else  ! Array has no distributed dimension
                     call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), values=int1d % array, rc=rc); ESMF_ERR(rc)
                  end if

               case (2)
                  call mpas_pool_get_field(allFields, itr % memberName, int2d, timeLevel)

               case (3)
                  call mpas_pool_get_field(allFields, itr % memberName, int3d, timeLevel)

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for integer field: ', trim(itr % memberName)
                  ESMF_ERR(1)
               end select

            case (MPAS_POOL_CHARACTER)
               select case (info % nDims)
               case (0)
                  call mpas_pool_get_field(allFields, itr % memberName, char0d, timeLevel)
                  call ESMF_InfoSet(bundle_info, key='/MPAS/'//trim(itr % memberName), value=trim(char0d % scalar), rc=rc); ESMF_ERR(rc)

               case (1)
                  call mpas_pool_get_field(allFields, itr % memberName, char1d, timeLevel)

               case default
                  write(0,*)'ERROR: Unsupported rank: ', info % nDims, ' for character field: ', trim(itr % memberName)
                  ESMF_ERR(1)
               end select

            case default
               write(0,*)'ERROR: Unsupported fieldType: ', info % fieldType, ' for field: ', trim(itr % memberName)
               ESMF_ERR(1)
            end select

         end if

      end do FIELD_LOOP

      call postwrite_reindex(allFields, stream % field_pool)

   end subroutine ufs_mpas_update_restart_array_bundle

   subroutine parse_output_list_vars(ufs_mpas_outputs, rc)

      type(ufs_mpas_output_type), intent(inout) :: ufs_mpas_outputs
      integer, intent(out) :: rc

      integer :: file_unit, i, io_status
      character(len=256) :: filename
      logical :: file_too_long
      character(len=64) :: var_name, var_interp_method

      rc = 0

      if (trim(ufs_mpas_outputs % name) == 'restart') then
         return
      end if

      filename = ufs_mpas_outputs % list_of_vars_fname
      if (trim(filename) == '') then
         write(0,*)'ERROR: list_of_vars_fname for stream '//trim(ufs_mpas_outputs % name)//' is empty '
         rc = 1
         return
      end if
      open(newunit=file_unit, file=trim(filename), status='old', action='read', iostat=io_status)
      if (io_status /= 0) then
         write(0, '(A,A,A)') "Error: Cannot open file '", trim(filename), "'. Check if the file exists."
         rc = 1
         return
      end if

      file_too_long = .false.

      do i = 1, max_num_output_vars + 1  ! Add 1 to explicitly detect overflow

         read(file_unit, *, iostat=io_status) var_name, var_interp_method

         if (io_status < 0) then
            exit  ! Normal end of file
         else if (io_status > 0) then
            write(0, '(A,I0,A)') 'Error reading line ', i, ' in file '//trim(filename)
            rc = 1
            return
         end if

         if (i > max_num_output_vars) then
            file_too_long = .true.
            exit
         end if

         ! Skip other interpolation methods
         if (trim(var_interp_method) == 'bilinear') then
            ufs_mpas_outputs % num_bilinear_vars = ufs_mpas_outputs % num_bilinear_vars + 1
            ufs_mpas_outputs % bilinear_vars(ufs_mpas_outputs % num_bilinear_vars) = trim(var_name)
         else if (trim(var_interp_method) == 'nearest_dtos') then
            ufs_mpas_outputs % num_nearest_dtos_vars = ufs_mpas_outputs % num_nearest_dtos_vars + 1
            ufs_mpas_outputs % nearest_dtos_vars(ufs_mpas_outputs % num_nearest_dtos_vars) = trim(var_name)
         else if (trim(var_interp_method) == 'nearest_stod') then
            ufs_mpas_outputs % num_nearest_stod_vars = ufs_mpas_outputs % num_nearest_stod_vars + 1
            ufs_mpas_outputs % nearest_stod_vars(ufs_mpas_outputs % num_nearest_stod_vars) = trim(var_name)
         else if (trim(var_interp_method) == 'patch') then
            ufs_mpas_outputs % num_patch_vars = ufs_mpas_outputs % num_patch_vars + 1
            ufs_mpas_outputs % patch_vars(ufs_mpas_outputs % num_patch_vars) = trim(var_name)
         else if (trim(var_interp_method) == 'conserve') then
            ufs_mpas_outputs % num_conserve_vars = ufs_mpas_outputs % num_conserve_vars + 1
            ufs_mpas_outputs % conserve_vars(ufs_mpas_outputs % num_conserve_vars) = trim(var_name)
         else
            write(0, '(A,I0,A)') 'Error on line ', i, ' in file '//trim(filename)//', unknown interp_method'
            rc = 1
            return
         end if

      end do

      if (file_too_long) then
         write(0, '(A)') 'Error file '//trim(filename)//' too long. Increase max_num_output_vars'
         rc = 1
         return
      end if

      close(file_unit)

   end subroutine parse_output_list_vars

   subroutine read_entire_file(filename, file_contents, success)
      implicit none
      character(len=*), intent(in) :: filename
      character(len=:), allocatable, intent(out) :: file_contents
      logical, intent(out) :: success
      integer :: file_size, unit_num, ios

      success = .false.

      ! Get file size
      inquire(file=filename, size=file_size)

      if (file_size == -1) then
         print *, "Error: File not found or cannot be accessed"
         return
      end if

      ! Allocate character array
      allocate(character(len=file_size) :: file_contents)

      ! Open and read the file
      open(newunit=unit_num, file=filename, status='old', action='read', &
         access='stream', form='unformatted', iostat=ios)

      if (ios /= 0) then
         print *, "Error opening file"
         return
      end if

      read(unit_num, iostat=ios) file_contents

      if (ios /= 0) then
         print *, "Error reading file"
         close(unit_num)
         return
      end if

      close(unit_num)
      success = .true.

   end subroutine read_entire_file

end module ufs_mpas_wgc_output
