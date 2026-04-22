#define ESMF_ERR(rc) \
  if (rc /= ESMF_SUCCESS) write(0,'(A,A,I0,A,I0)') __FILE__,':',__LINE__, ' ESMF rc: ', rc; \
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

#define ASSERT(a) \
  if ((a) .neqv. .true. ) write(0,'(A,A,I0,A)') __FILE__,':',__LINE__, ' assertion failed'; \
  if ((a) .neqv. .true. ) stop 1

! ###########################################################################################
!> \file module_fcst_grid_comp.F90
!>
!> ESMF forecast gridded component for MPAS ATMosphere.
!>
! ###########################################################################################
module module_fcst_grid_comp

  use mpi_f08
  use esmf
  use nuopc

  use mpas_subdriver

  use module_mpasmodel_config, only : fcst_mpi_comm, dt_atmos, output_fh, quilting, quilting_restart, calendar

  use module_mpasmodel_config, only : corelist, domain
  use module_mpasmodel_config, only : nCellsSolve, nEdgesSolve, nVerticesSolve, nVertLevels
  use module_mpasmodel_config, only : nCellsGlobal, nEdgesGlobal, nVerticesGlobal

  use ufs_mpas_wgc_output,     only : ufs_mpas_create_history_bundle, ufs_mpas_update_history_bundle
  use ufs_mpas_wgc_output,     only : ufs_mpas_create_restart_bundle, ufs_mpas_update_restart_bundle
  use ufs_mpas_wgc_output,     only : ufs_mpas_create_restart_array_bundle, ufs_mpas_update_restart_array_bundle

  use module_cplfields,        only: nExportFields, exportFields, exportFieldsInfo, &
                                     nImportFields, importFields, importFieldsInfo

  use module_write_mpas_restart_array_bundle_pio, only : write_mpas_restart_array_bundle_pio

  implicit none
  private

  ! module variables
  integer                        :: n_atmsteps
  integer                        :: seconds
  integer, allocatable           :: frestart(:)
  type(ESMF_Time)                :: StartTime

  integer :: mype = 0

  type(ESMF_FieldBundle) :: history_bilinear_field_bundle
  type(ESMF_FieldBundle) :: history_nearest_dtos_field_bundle
  type(ESMF_FieldBundle) :: history_nearest_stod_field_bundle
  type(ESMF_FieldBundle) :: history_patch_field_bundle
  type(ESMF_FieldBundle) :: history_conserve_field_bundle
  integer, parameter :: max_num_history_vars = 1000
  character(len=64) :: history_bilinear_vars(max_num_history_vars)
  character(len=64) :: history_nearest_dtos_vars(max_num_history_vars)
  character(len=64) :: history_nearest_stod_vars(max_num_history_vars)
  character(len=64) :: history_patch_vars(max_num_history_vars)
  character(len=64) :: history_conserve_vars(max_num_history_vars)
  integer :: num_history_bilinear_vars = 0
  integer :: num_history_nearest_dtos_vars = 0
  integer :: num_history_nearest_stod_vars = 0
  integer :: num_history_patch_vars = 0
  integer :: num_history_conserve_vars = 0

  type(ESMF_FieldBundle) :: restart_field_bundle
  type(ESMF_ArrayBundle) :: restart_array_bundle
  type(ESMF_ArrayBundle) :: history_array_bundle
  type(ESMF_ArrayBundle) :: diag_bundle

  public SetServices

contains

  ! #########################################################################################
  ! ESMF entrypoints for forecast grid-component.
  ! #########################################################################################
  subroutine SetServices(fcst_comp, rc)
    type(ESMF_GridComp)  :: fcst_comp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! Initialize
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_INITIALIZE, &
                                    userRoutine=fcst_initialize, phase=1, rc=rc); ESMF_ERR(rc)

    ! Advertise
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_INITIALIZE, &
                                    userRoutine=fcst_advertise, phase=2, rc=rc); ESMF_ERR(rc)

    ! Realize
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_INITIALIZE, &
                                    userRoutine=fcst_realize, phase=3, rc=rc); ESMF_ERR(rc)

    ! Run Phase 1
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_RUN, &
                                    userRoutine=fcst_run_phase_1, phase=1, rc=rc); ESMF_ERR(rc)

    ! Run Phase 2
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_RUN, &
                                    userRoutine=fcst_run_phase_2, phase=2, rc=rc); ESMF_ERR(rc)

    ! Finalize
    call ESMF_GridCompSetEntryPoint(fcst_comp, ESMF_METHOD_FINALIZE, &
                                    userRoutine=fcst_finalize, rc=rc); ESMF_ERR(rc)

  end subroutine SetServices

  ! #########################################################################################
  ! Initialize the ESMF forecast grid component.
  ! #########################################################################################
  subroutine fcst_initialize(fcst_comp, importState, exportState, clock, rc)

    use mpas_derived_types, only : mpas_pool_type
    use mpas_dmpar,         only : mpas_dmpar_sum_int

    type(esmf_GridComp)     :: fcst_comp
    type(ESMF_State)        :: importState, exportState
    type(esmf_Clock)        :: clock
    integer, intent(out)    :: rc

    ! Locals
    integer :: i, j, k, n
    type(ESMF_VM) :: VM
    type(ESMF_Time) :: CurrTime, StopTime
    type(ESMF_Config) :: CF
    real(kind=8) :: tbeg1
    integer,dimension(6) :: date_init, date, date_end
    character(4) dateSY
    character(2) dateSM,dateSD,dateSH,dateSN,dateSS
    character(len=80) :: dateS
    integer :: fcst_ntasks

    type (mpas_pool_type), pointer :: mesh
    type(ESMF_Info) :: info
    integer :: ngrids=1
    logical, allocatable :: is_moving(:)
    integer                       :: num_restart_fh
    real,dimension(:),allocatable :: restart_fh

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call ESMF_VMGetCurrent(vm=vm,rc=rc); ESMF_ERR(rc)

    call ESMF_VMGet(vm=vm, localPet=mype, mpiCommunicator=fcst_mpi_comm%mpi_val, &
                    petCount=fcst_ntasks, rc=rc); ESMF_ERR(rc)
    if (mype == 0) write(*,*)'in fcst_initialize, fcst_ntasks=',fcst_ntasks

    CF = ESMF_ConfigCreate(rc=rc); ESMF_ERR(rc)

    call ESMF_ConfigLoadFile(config=CF ,filename='model_configure' ,rc=rc); ESMF_ERR(rc)

    num_restart_fh = ESMF_ConfigGetLen(config=CF, label ='restart_interval:',rc=rc); ESMF_ERR(rc)

    if (num_restart_fh<=0) num_restart_fh = 1
    allocate(restart_fh(num_restart_fh))
    restart_fh = 0
    call ESMF_ConfigGetAttribute(CF,valueList=restart_fh,label='restart_interval:', &
                                 count=num_restart_fh, rc=rc); ESMF_ERR(rc)
    if (mype == 0) print *,'restart_fh=',restart_fh
!
    !
    ! Set atmos time.
    !
    call ESMF_ClockGet(clock, CurrTime=CurrTime, StartTime=StartTime, StopTime=StopTime, rc=rc); ESMF_ERR(rc)

    date_init = 0
    call ESMF_TimeGet (StartTime,                      &
                       YY=date_init(1), MM=date_init(2), DD=date_init(3), &
                       H=date_init(4),  M =date_init(5), S =date_init(6), rc=rc); ESMF_ERR(rc)

    date = 0
    call ESMF_TimeGet (CurrTime,                           &
                       YY=date(1), MM=date(2), DD=date(3), &
                       H=date(4),  M =date(5), S =date(6), rc=rc); ESMF_ERR(rc)

    date_end = 0
    call ESMF_TimeGet (StopTime,                                       &
                       YY=date_end(1), MM=date_end(2), DD=date_end(3), &
                       H=date_end(4),  M =date_end(5), S =date_end(6), rc=rc); ESMF_ERR(rc)

    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, StartTime=',date_init
    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, CurrTime =',date
    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, StopTime =',date_end


    ! Initialize frestart array
    call init_frestart(StartTime, StopTime, num_restart_fh, restart_fh)

    ! #######################################################################################
    ! Initialize component models.
    ! mpas_init() calls the MPAS initialization.
    ! #######################################################################################
#ifdef MPAS_USE_MPI_F08
    call mpas_init(corelist, domain, external_comm=fcst_mpi_comm)
#else
    call mpas_init(corelist, domain, external_comm=fcst_mpi_comm%mpi_val)
#endif

    call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
    call mpas_pool_get_dimension(mesh, 'nCellsSolve',    nCellsSolve)
    call mpas_pool_get_dimension(mesh, 'nVerticesSolve', nVerticesSolve)
    call mpas_pool_get_dimension(mesh, 'nEdgesSolve',    nEdgesSolve)
    call mpas_pool_get_dimension(mesh, 'nVertLevels',    nVertLevels)

    call mpas_dmpar_sum_int(domain % dminfo, nVerticesSolve, nVerticesGlobal)
    call mpas_dmpar_sum_int(domain % dminfo, nCellsSolve, nCellsGlobal)
    call mpas_dmpar_sum_int(domain % dminfo, nEdgesSolve, nEdgesGlobal)

    if (quilting) then
      ! History bundles
      call parse_history_list_vars(rc=rc); ESMF_ERR(rc)

      if (num_history_bilinear_vars > 0) then
        call ufs_mpas_create_history_bundle(history_bilinear_field_bundle, history_bilinear_vars(1:num_history_bilinear_vars), 'bilinear', rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ history_bilinear_field_bundle /), rc=rc); ESMF_ERR(rc)
      end if

      if (num_history_nearest_dtos_vars > 0) then
        call ufs_mpas_create_history_bundle(history_nearest_dtos_field_bundle, history_nearest_dtos_vars(1:num_history_nearest_dtos_vars), 'nearest_dtos', rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ history_nearest_dtos_field_bundle /), rc=rc); ESMF_ERR(rc)
      end if

      if (num_history_nearest_stod_vars > 0) then
        call ufs_mpas_create_history_bundle(history_nearest_stod_field_bundle, history_nearest_stod_vars(1:num_history_nearest_stod_vars), 'nearest_stod', rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ history_nearest_stod_field_bundle /), rc=rc); ESMF_ERR(rc)
      end if

      if (num_history_patch_vars > 0) then
        call ufs_mpas_create_history_bundle(history_patch_field_bundle, history_patch_vars(1:num_history_patch_vars), 'patch', rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ history_patch_field_bundle /), rc=rc); ESMF_ERR(rc)
      end if

      if (num_history_conserve_vars > 0) then
        call ufs_mpas_create_history_bundle(history_conserve_field_bundle, history_conserve_vars(1:num_history_conserve_vars), 'conserve', rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ history_conserve_field_bundle /), rc=rc); ESMF_ERR(rc)
      end if

      ! Test history type bundle on mesh, using restart array bundle
      call ufs_mpas_create_restart_array_bundle(history_array_bundle, bundle_name='history_native', stream_name='output', rc=rc); ESMF_ERR(rc)
      call ESMF_InfoGetFromHost(history_array_bundle, info=info, rc=rc); ESMF_ERR(rc)
      call ESMF_InfoSet(info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)
      call ESMF_StateAdd(exportState, (/ history_array_bundle /), rc=rc); ESMF_ERR(rc)

      ! call ufs_mpas_create_restart_array_bundle(diag_bundle, bundle_name='diag_native', stream_name='diagnostics', rc=rc); ESMF_ERR(rc)
      ! call ESMF_InfoGetFromHost(diag_bundle, info=info, rc=rc); ESMF_ERR(rc)
      ! call ESMF_InfoSet(info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)
      ! call ESMF_StateAdd(exportState, (/ diag_bundle /), rc=rc); ESMF_ERR(rc)

      ! Restart bundle
      if (quilting_restart) then
        ! use FieldBundle
        ! call ufs_mpas_create_restart_bundle(restart_field_bundle, rc=rc); ESMF_ERR(rc)
        ! call ESMF_InfoGetFromHost(restart_field_bundle, info=info, rc=rc); ESMF_ERR(rc)
        ! call ESMF_InfoSet(info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)
        ! call ESMF_StateAdd(exportState, (/ restart_field_bundle /), rc=rc); ESMF_ERR(rc)

        ! use ArrayBundle
        call ufs_mpas_create_restart_array_bundle(restart_array_bundle, bundle_name='restart_mpas_array', stream_name='restart', rc=rc); ESMF_ERR(rc)
        call ESMF_InfoGetFromHost(restart_array_bundle, info=info, rc=rc); ESMF_ERR(rc)
        call ESMF_InfoSet(info, key="/NetCDF/FV3-nooutput/frestart", values=frestart, rc=rc); ESMF_ERR(rc)
        call ESMF_StateAdd(exportState, (/ restart_array_bundle /), rc=rc); ESMF_ERR(rc)
      end if ! quilting_restart
    end if ! quilting

    ngrids = 1
    allocate(is_moving(ngrids))
    is_moving = .false.
    call ESMF_InfoGetFromHost(exportState, info=info, rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/ngrids", value=ngrids, rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/top_parent_is_global", value=.false., rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="is_moving", values=is_moving, rc=rc); ESMF_ERR(rc)
    deallocate(is_moving)

! Add time Attribute to the exportState
    write(dateSY,'(I4.4)')date_init(1)
    write(dateSM,'(I2.2)')date_init(2)
    write(dateSD,'(I2.2)')date_init(3)
    write(dateSH,'(I2.2)')date_init(4)
    write(dateSN,'(I2.2)')date_init(5)
    write(dateSS,'(I2.2)')date_init(6)

    dateS="hours since "//dateSY//'-'//dateSM//'-'//dateSD//' '//dateSH//':'// dateSN//":"//dateSS
    if (mype == 0) write(*,*)'dateS=',trim(dateS)

    call ESMF_InfoGetFromHost(exportState, info=info, rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time", value=real(0,ESMF_KIND_R8), rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time:long_name", value="time", rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time:cartesian_axis", value="T", rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time:units", value=trim(dateS), rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time:calendar_type", value=trim(calendar), rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time:calendar", value=trim(calendar), rc=rc); ESMF_ERR(rc)

! Add time_iso Attribute to the exportState
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time_iso", value="yyyy-mm-ddThh:mm:ssZ", rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time_iso:long_name", value="valid time", rc=rc); ESMF_ERR(rc)
    call ESMF_InfoSet(info, key="/NetCDF/FV3/time_iso:description", value="ISO 8601 Date String", rc=rc); ESMF_ERR(rc)

    ! Timing info (debug mode)
    if (mype == 0) write(*,*)'PASS(fcst_initialize): Time is ', mpi_wtime() - tbeg1

  end subroutine fcst_initialize

  ! ###########################################################################################
  ! Advertise the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_advertise(fcst_comp, importState, exportState, clock, rc)
    type(esmf_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(esmf_Clock)    :: clock
    integer,intent(out) :: rc

    ! Locals
    integer :: i

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! importable fields:
    do i = 1, size(importFieldsInfo)
      call NUOPC_Advertise(importState, &
                           StandardName=trim(importFieldsInfo(i)%name), &
                           SharePolicyField='share', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end do

    ! exportable fields:
    do i = 1, size(exportFieldsInfo)
      call NUOPC_Advertise(exportState, &
                           StandardName=trim(exportFieldsInfo(i)%name), &
                           SharePolicyField='share', rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    end do

  end subroutine fcst_advertise

  ! ###########################################################################################
  ! Realize the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_realize(fcst_comp, importState, exportState, clock, rc)

    use module_cplscalars,  only : flds_scalar_name, flds_scalar_num, SetScalarField
    use ufs_mpas_wgc_output, only : ufs_mpas_get_esmf_mesh

    type(esmf_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(esmf_Clock)    :: clock
    integer,intent(out) :: rc

    ! Locals
    integer :: i, n
    type(ESMF_StateIntent_Flag) :: stateintent
    integer          :: item
    logical          :: isConnected
    type(ESMF_Field) :: field
    type(ESMF_Mesh)  :: mesh
    real(ESMF_KIND_R8) :: l_fill_value
    real(ESMF_KIND_R8), parameter :: d_fill_value = 0._ESMF_KIND_R8

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    call ufs_mpas_get_esmf_mesh(mesh, rc=rc); ESMF_ERR(rc)

    call ESMF_StateGet(exportState, stateintent=stateintent, rc=rc); ESMF_ERR(rc)

    if (stateintent == ESMF_STATEINTENT_EXPORT) then
    end if

    do item = 1, size(exportFieldsInfo)
      isConnected = NUOPC_IsConnected(exportState, fieldName=trim(exportFieldsInfo(item)%name), rc=rc); ESMF_ERR(rc)
      if (isConnected) then
        if (trim(exportFieldsInfo(item)%name) == trim(flds_scalar_name)) then
          ! Create the scalar field
          call SetScalarField(field, flds_scalar_name, flds_scalar_num, rc=rc); ESMF_ERR(rc)
        else
          call ESMF_StateGet(exportState, field=field, itemName=trim(exportFieldsInfo(item)%name), rc=rc); ESMF_ERR(rc)
          call ESMF_FieldEmptySet(field, mesh=mesh, meshloc=ESMF_MESHLOC_ELEMENT, rc=rc); ESMF_ERR(rc)

          select case (exportFieldsInfo(item)%type)
          ! case ('l','layer')
          !   call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
          !        ungriddedLBound=(/1/), ungriddedUBound=(/numLevels/), rc=rc); ESMF_ERR(rc)
          ! case ('i','interface')
          !   call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
          !        ungriddedLBound=(/1/), ungriddedUBound=(/numLevels+1/), rc=rc); ESMF_ERR(rc)
          ! case ('t','tracer')
          !   call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
          !        ungriddedLBound=(/1, 1/), ungriddedUBound=(/numLevels, numTracers/), rc=rc); ESMF_ERR(rc)
          !   if (allocated(tracerNames)) then
          !     call addFieldMetadata(field, 'tracerNames', tracerNames, rc=rc); ESMF_ERR(rc)
          !   end if
          !   if (allocated(tracerUnits)) then
          !     call addFieldMetadata(field, 'tracerUnits', tracerUnits, rc=rc); ESMF_ERR(rc)
          !   end if
          case ('s','surface')
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc); ESMF_ERR(rc)
          ! case ('g','soil')
          !   call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
          !        ungriddedLBound=(/1/), ungriddedUBound=(/numSoilLayers/), rc=rc); ESMF_ERR(rc)
          case default
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                 msg="exportFieldType = '"//trim(exportFieldsInfo(item)%type)//"' not recognized", &
                 line=__LINE__, file=__FILE__, rcToReturn=rc); ESMF_ERR(rc)
            return
          end select
        end if
        call NUOPC_Realize(exportState, field=field, rc=rc); ESMF_ERR(rc)

        ! -- initialize field value
        call ESMF_FieldFill(field, dataFillScheme="const", const1=l_fill_value, rc=rc); ESMF_ERR(rc)

        ! -- save field
        exportFields(item) = field
        call ESMF_LogWrite('MPAS Export Field '//trim(exportFieldsInfo(item)%name)  &
             // ' is connected ', ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=rc); ESMF_ERR(rc)
      else
        ! remove a not connected Field from State
        call ESMF_StateRemove(exportState, (/trim(exportFieldsInfo(item)%name)/), rc=rc); ESMF_ERR(rc)
        call ESMF_LogWrite('MPAS Export Field '//trim(exportFieldsInfo(item)%name)  &
             // ' is not connected ', ESMF_LOGMSG_INFO, line=__LINE__, file=__FILE__, rc=rc); ESMF_ERR(rc)
      end if
    end do

    ! -- initialize export fields if applicable
    call setup_exportdata(rc=rc); ESMF_ERR(rc)

  end subroutine fcst_realize

  ! ###########################################################################################
  ! Run phase(1) for the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_run_phase_1(fcst_comp, importState, exportState, clock, rc)

    type(ESMF_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    ! Locals
    integer             :: ierr
    integer             :: fcst_seconds, fcst_days
    real(kind=8)        :: tbeg1
    character(19)       :: xtime        ! "YYYY-MM-DD_hh:mm:ss"

    type (ESMF_Time) :: ufsCurrTime
    type (MPAS_Time_Type) :: mpasStartTime, mpasCurrTime, stepStopTime
    type (MPAS_Clock_type), pointer :: mpas_clock
    type (MPAS_TimeInterval_type) :: atmTimeStep

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call ESMF_ClockGet(clock, CurrTime=ufsCurrTime, rc=rc); ESMF_ERR(rc)

    mpas_clock => domain % clock

    mpasStartTime = mpas_get_clock_time(mpas_clock, MPAS_START_TIME, ierr)
    mpasCurrTime = mpas_get_clock_time(mpas_clock, MPAS_NOW, ierr=ierr)

    ! Assert that the UFS and MPAS clocks are in sync
    ASSERT (ufsCurrTime == mpasCurrTime % t)

    ! Set MPAS's clock stop dt_atmos seconds from current time
    ! This will make MPAS run dt_atmos/config_dt steps, then return
    call mpas_set_timeInterval(atmTimeStep, S=dt_atmos, ierr=ierr)
    stepStopTime = mpasCurrTime + atmTimeStep
    call mpas_set_clock_time(mpas_clock, stepStopTime, MPAS_STOP_TIME, ierr=ierr)

    call mpas_run(domain)

    ! The MPAS's clock has advanced in mpas_run, look at the MPAS's current time,
    ! and compute number of seconds since the start time (original non-restarted run)
    ! saved in StartTime module variable, to determine if it's time for output
    mpasCurrTime = mpas_get_clock_time(mpas_clock, MPAS_NOW, ierr=ierr)

    n_atmsteps = (mpasCurrTime % t - StartTime) / atmTimeStep % ti

    call ESMF_TimeIntervalGet(mpasCurrTime % t - StartTime, s=seconds, rc=rc)

    if (quilting) then
      if (ANY(nint(output_fh(:)*3600.0) == seconds)) then
        if (num_history_bilinear_vars > 0) then
         call ufs_mpas_update_history_bundle(history_bilinear_field_bundle, history_bilinear_vars(1:num_history_bilinear_vars), rc=rc); ESMF_ERR(rc)
        end if
        if (num_history_nearest_dtos_vars > 0) then
          call ufs_mpas_update_history_bundle(history_nearest_dtos_field_bundle, history_nearest_dtos_vars(1:num_history_nearest_dtos_vars), rc=rc); ESMF_ERR(rc)
        end if
        if (num_history_nearest_stod_vars > 0) then
          call ufs_mpas_update_history_bundle(history_nearest_stod_field_bundle, history_nearest_stod_vars(1:num_history_nearest_stod_vars), rc=rc); ESMF_ERR(rc)
        end if
        if (num_history_patch_vars > 0) then
          call ufs_mpas_update_history_bundle(history_patch_field_bundle, history_patch_vars(1:num_history_patch_vars), rc=rc); ESMF_ERR(rc)
        end if
        if (num_history_conserve_vars > 0) then
          call ufs_mpas_update_history_bundle(history_conserve_field_bundle, history_conserve_vars(1:num_history_conserve_vars), rc=rc); ESMF_ERR(rc)
        end if

        ! Test history type bundle on mesh, using restart array bundle
        call ufs_mpas_update_restart_array_bundle(history_array_bundle, stream_name='output', rc=rc); ESMF_ERR(rc)
        ! call ufs_mpas_update_restart_array_bundle(diag_bundle, stream_name='diagnostics', rc=rc); ESMF_ERR(rc)
      end if

      ! Update restart bundle
      if (quilting_restart) then
        if (ANY(frestart(:) == seconds)) then
          ! call ufs_mpas_update_restart_bundle(restart_field_bundle, rc=rc); ESMF_ERR(rc)

          call ufs_mpas_update_restart_array_bundle(restart_array_bundle, stream_name='restart', rc=rc); ESMF_ERR(rc)

        end if
      end if ! quilting
    end if ! quilting_restart

    ! Timing info (debug mode)
    if (mype == 0) write(*,'(A,I8,A,F8.3,A,F8.3)') &
                                       'atm phase1: atmsteps: ',  n_atmsteps, &
                                       ' fcst time: ',(seconds/3600.), &
                                       ' elapsed time per step: ',  mpi_wtime()-tbeg1
  end subroutine fcst_run_phase_1

  ! ###########################################################################################
  ! Run phase(2) for the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_run_phase_2(fcst_comp, importState, exportState, clock, rc)
    type(ESMF_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(ESMF_Clock)    :: clock
    integer,intent(out) :: rc

    ! Locals
    real(kind=8)        :: tbeg1

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call setup_exportdata(rc=rc); ESMF_ERR(rc)

    ! Timing info (debug mode)
    if (mype == 0) write(*,'(A,I8,A,F8.3,A,F8.3)') &
                                       'atm phase2: atmsteps: ',  n_atmsteps, &
                                       ' fcst time: ',(seconds/3600.), &
                                       ' elapsed time per step: ',  mpi_wtime()-tbeg1
  end subroutine fcst_run_phase_2

  ! ###########################################################################################
  ! Finalize the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_finalize(fcst_comp, importState, exportState, clock, rc)
    type(esmf_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(esmf_Clock)    :: clock
    integer,intent(out) :: rc

    ! Locals
    real(kind=8)        :: tbeg1

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

     ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call mpas_finalize(corelist, domain)

    ! Timing info (debug mode)
    if (mype == 0) write(*,*)'PASS(fcst_finalize): total is ', mpi_wtime() - tbeg1

  end subroutine fcst_finalize

  subroutine parse_history_list_vars(rc)

   integer, intent(out) :: rc

   integer :: file_unit, i, io_status
   character(len=256) :: filename
   logical :: file_too_long
   character(len=64) :: var_name, var_interp_method

   rc = 0

   filename = 'ufs_mpasmodel_wgc_history_list'
   open(newunit=file_unit, file=trim(filename), status='old', action='read', iostat=io_status)
   if (io_status /= 0) then
       write(0, '(A,A,A)') "Error: Cannot open file '", trim(filename), "'. Check if the file exists."
       rc = 1
       return
   end if

   file_too_long = .false.

   do i = 1, max_num_history_vars + 1  ! Add 1 to explicitly detect overflow

      read(file_unit, *, iostat=io_status) var_name, var_interp_method

      if (io_status < 0) then
          exit  ! Normal end of file
      else if (io_status > 0) then
          write(0, '(A,I0,A)') "Error reading line ", i, " in file "//trim(filename)
          rc = 1
          return
      end if

      if (i > max_num_history_vars) then
          file_too_long = .true.
          exit
      end if

      ! Skip other interpolation methods
      if (trim(var_interp_method) == 'bilinear') then
          num_history_bilinear_vars = num_history_bilinear_vars + 1
          history_bilinear_vars(num_history_bilinear_vars) = trim(var_name)
      else if (trim(var_interp_method) == 'nearest_dtos') then
          num_history_nearest_dtos_vars = num_history_nearest_dtos_vars + 1
          history_nearest_dtos_vars(num_history_nearest_dtos_vars) = trim(var_name)
      else if (trim(var_interp_method) == 'nearest_stod') then
          num_history_nearest_stod_vars = num_history_nearest_stod_vars + 1
          history_nearest_stod_vars(num_history_nearest_stod_vars) = trim(var_name)
      else if (trim(var_interp_method) == 'patch') then
          num_history_patch_vars = num_history_patch_vars + 1
          history_patch_vars(num_history_patch_vars) = trim(var_name)
      else if (trim(var_interp_method) == 'conserve') then
          num_history_conserve_vars = num_history_conserve_vars + 1
          history_conserve_vars(num_history_conserve_vars) = trim(var_name)
      else
          write(0, '(A,I0,A)') "Error on line ", i, " in file "//trim(filename)//", unknown interp_method"
          rc = 1
          return
      end if

   end do

   if (file_too_long) then
       write(0, '(A)') "Error file "//trim(filename)//" too long. Increase max_num_history_vars"
       rc = 1
       return
   endif

   close(file_unit)

  end subroutine parse_history_list_vars

  ! Same as 'fcst_time_array_setup' in fv3, but using ESMF time types instead of FMS types
  subroutine init_frestart(Time_init, Time_end, num_restart_fh, restart_fh)

    type(ESMF_Time), intent(in)                 :: Time_init, Time_end
    integer,         intent(in)                 :: num_restart_fh
    real, dimension(:), allocatable, intent(in) :: restart_fh

    ! local variables
    integer         :: tmpvar, i, rc
    logical         :: freq_restart
    type(ESMF_Time) :: Time_restart
    type(ESMF_TimeInterval) :: Time_step_restart
    integer         :: n_restart

    ! set up forecast time array that controls when to write out restart files

    ! if the second item is -1, the first number is frequency
    freq_restart = .false.
    if(num_restart_fh == 2) then
      if(restart_fh(2)== -1) freq_restart = .true.
    endif
    if(freq_restart) then
      if(restart_fh(1) >= 0) then
        tmpvar = nint(restart_fh(1) * 3600)
        call ESMF_TimeIntervalSet(Time_step_restart, s=tmpvar, rc=rc); ESMF_ERR(rc)
        Time_restart = Time_init + Time_step_restart
        if(restart_fh(1) > 0) then
          n_restart = ( Time_end - Time_init ) / Time_step_restart
          allocate(frestart(n_restart))
          frestart(1) = tmpvar
          i = 1
          do while ( Time_restart + Time_step_restart <= Time_end )
            i = i + 1
            frestart(i) = frestart(i-1) + tmpvar
            Time_restart = Time_restart + Time_step_restart
          enddo
        else
         allocate(frestart(1))
         frestart(1) = tmpvar
        endif
      endif
    ! otherwise it is an array with forecast time at which the restart files will be written out
    else if(num_restart_fh >= 1) then
      allocate(frestart(num_restart_fh))
      if(num_restart_fh == 1 .and. restart_fh(1) == 0 ) then
        call ESMF_TimeIntervalGet(Time_end - Time_init, s=frestart(1), rc=rc); ESMF_ERR(rc)
      else
        do i=1,num_restart_fh
          frestart(i) = nint(restart_fh(i) * 3600.)
        enddo
      endif
    endif

    if (mype == 0) print *,'frestart=',frestart(1:min(10,size(frestart)))/3600
  end subroutine init_frestart

  subroutine setup_exportdata(rc)

    use ESMF

    use module_cplfields,  only : exportFields
    use module_cplscalars, only : flds_scalar_name

    use mpas_kind_types,   only : RKIND
    use mpas_constants,    only : rvord

    !--- arguments
    integer, optional, intent(out) :: rc

    !--- local variables
    integer                :: i, j

    integer                                     :: localrc
    integer                                     :: n,rank
    logical                                     :: isFound
    type(ESMF_TypeKind_Flag)                    :: datatype
    character(len=ESMF_MAXSTR)                  :: fieldName
    real(kind=ESMF_KIND_R4), dimension(:,:), pointer   :: datar42d
    real(kind=ESMF_KIND_R8), dimension(:), pointer     :: datar81d
    real(kind=ESMF_KIND_R8), dimension(:,:), pointer   :: datar82d
    real(kind=ESMF_KIND_R8), dimension(:,:,:), pointer :: datar83d

    !--- local parameters
    real(kind=ESMF_KIND_R8), parameter :: zeror8 = 0._ESMF_KIND_R8

    type (mpas_pool_type), pointer :: mesh
    type (mpas_pool_type), pointer :: state
    type (mpas_pool_type), pointer :: diag
    type (mpas_pool_type), pointer :: diag_physics
    real (kind=RKIND), dimension(:),   pointer :: r_1d
    real (kind=RKIND), dimension(:,:), pointer :: r_2d, r_2d_2
    real (kind=RKIND), dimension(:,:), pointer :: theta_m, exner
    real (kind=RKIND), dimension(:,:,:), pointer :: scalars
    integer, pointer :: index_qv

    !--- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
    call mpas_pool_get_subpool(domain % blocklist % structs, 'state', state)
    call mpas_pool_get_subpool(domain % blocklist % structs, 'diag', diag)
    call mpas_pool_get_subpool(domain % blocklist % structs, 'diag_physics', diag_physics)

    do n=1, size(exportFields)

      datar42d => null()
      datar81d => null()
      datar82d => null()
      datar83d => null()
      r_1d => null()
      r_2d => null()
      r_2d_2 => null()

      isFound = ESMF_FieldIsCreated(exportFields(n), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return

      if (isFound) then
        call ESMF_FieldGet(exportFields(n), name=fieldname, rank=rank, typekind=datatype, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return
        if (trim(fieldname) == trim(flds_scalar_name)) then
          isFound = .false.
        else
          if (datatype == ESMF_TYPEKIND_R8) then
            select case (rank)
            case (1)
              call ESMF_FieldGet(exportFields(n),farrayPtr=datar81d,localDE=0, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return
            case (2)
              call ESMF_FieldGet(exportFields(n),farrayPtr=datar82d,localDE=0, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return
            case (3)
              call ESMF_FieldGet(exportFields(n),farrayPtr=datar83d,localDE=0, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return
            case default
              !--- skip field
              isFound = .false.
            end select
          else if (datatype == ESMF_TYPEKIND_R4) then
            select case (rank)
            case (2)
              call ESMF_FieldGet(exportFields(n),farrayPtr=datar42d,localDE=0, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__, rcToReturn=rc)) return
            case default
              !--- skip field
              isFound = .false.
            end select
          else
            !--- skip field
            isFound = .false.
          end if
        end if
      end if
      ! if (isFound .and. GFS_control%cplchm) isFound = .not.any(trim(fieldname) == chemistryFieldNames)

      if (isFound) then
          select case (trim(fieldname))
            !--- Instantaneous quantities
            ! bottom layer zonal wind (u)
            case('inst_zonal_wind_height_lowest')
              call mpas_pool_get_array(diag, 'uReconstructZonal', r_2d)
              datar81d = 0.5 * r_2d(1,1:nCellsSolve)
              continue
            ! bottom layer meridional wind (v)
            case('inst_merid_wind_height_lowest')
              call mpas_pool_get_array(diag, 'uReconstructMeridional', r_2d)
              datar81d = 0.5 * r_2d(1,1:nCellsSolve)
              continue
            ! bottom layer height (z)
            case('inst_height_lowest')
              call mpas_pool_get_array(mesh, 'zgrid', r_2d)
              datar81d = 0.5 * (r_2d(2,1:nCellsSolve) - r_2d(1,1:nCellsSolve))
              continue
            ! Instantaneous u wind (m/s) 10 m above ground
            case ('inst_zonal_wind_height10m')
              call mpas_pool_get_array(diag_physics, 'u10', r_1d)
              datar81d = r_1d(1:nCellsSolve)
              continue
            ! Instantaneous v wind (m/s) 10 m above ground
            case ('inst_merid_wind_height10m')
              call mpas_pool_get_array(diag_physics, 'v10', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            ! bottom layer temperature (t)
            case('inst_temp_height_lowest')
              ! calculation of temperature at cell centers
              call mpas_pool_get_array(state, 'theta_m', theta_m, 1)
              call mpas_pool_get_array(state, 'scalars', scalars, 1)
              call mpas_pool_get_dimension(state, 'index_qv', index_qv)
              call mpas_pool_get_array(diag, 'exner', exner)
              datar81d = (theta_m(1,1:nCellsSolve)/(1._RKIND+rvord*scalars(index_qv,  1,1:nCellsSolve)))*exner(1,1:nCellsSolve)
            ! bottom layer pressure (p)
            case('inst_pres_height_lowest')
              call mpas_pool_get_array(diag, 'pressure_base', r_2d)
              call mpas_pool_get_array(diag, 'pressure_p', r_2d_2)
              datar81d = r_2d(1,1:nCellsSolve) + r_2d_2(1,1:nCellsSolve)
            ! Instantaneous Pressure (Pa) land and sea surface
            case ('inst_pres_height_surface')
              call mpas_pool_get_array(diag, 'surface_pressure', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_spec_humid_height_lowest')
              ! call mpas_pool_get_array(diag_physics, 'qsfc', r_1d)
              call mpas_pool_get_array(diag_physics, 'q2', r_1d) ! FIXME
              datar81d = r_1d(1:nCellsSolve)
            case ('air_density_height_lowest')
              call mpas_pool_get_array(diag, 'rho', r_2d)
              datar81d = r_2d(1,1:nCellsSolve)
            case ('inst_temp_height2m')
              call mpas_pool_get_array(diag_physics, 't2m', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_spec_humid_height2m')
              call mpas_pool_get_array(diag_physics, 'q2', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_prec_rate') ! Faxa_rain
              call mpas_pool_get_array(diag_physics,'rainncv', r_1d)
              datar81d = r_1d(1:nCellsSolve) / dt_atmos
            case ('inst_fprec_rate') ! Faxa_snow
              call mpas_pool_get_array(diag_physics,'snowncv', r_1d)
              datar81d = r_1d(1:nCellsSolve) / dt_atmos
            case ('inst_down_lw_flx') ! Faxa_lwdn
              call mpas_pool_get_array(diag_physics,'glw', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_down_sw_ir_dir_flx') ! Faxa_swndr
              call mpas_pool_get_array(diag_physics,'swddir', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_down_sw_ir_dif_flx') ! Faxa_swndf
              call mpas_pool_get_array(diag_physics,'swddif', r_1d)
              datar81d = r_1d(1:nCellsSolve)
            case ('inst_down_sw_vis_dir_flx') ! Faxa_swvdr
              continue
            case ('inst_down_sw_vis_dif_flx') ! Faxa_swvdf
              continue
            case default
              localrc = ESMF_RC_NOT_FOUND
          end select

        if (ESMF_LogFoundError(rcToCheck=localrc, msg="Failure to populate exported field: "//trim(fieldname), &
          line=__LINE__, file=__FILE__, rcToReturn=rc)) return
      endif

    enddo ! exportFields

  end subroutine setup_exportdata

end module  module_fcst_grid_comp
