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
  use mpas_derived_types, only : core_type, domain_type

  use module_mpasmodel_config
  use ufs_mpas_wgc_output

  implicit none
  private

  !---- model defined-types ----
  integer                        :: n_atmsteps

  !----- coupled model data -----
  integer :: calendar_type = -99
  integer :: date_init(6)

  integer :: mype = 0
  type(ESMF_FieldBundle) :: history_field_bundle

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
    type(ESMF_Time) :: CurrTime, StartTime, StopTime
    real(kind=8) :: tbeg1
    ! integer :: initClock, io_unit, calendar_type_res, date_res(6), date_init_res(6)
    integer,dimension(6) :: date, date_end, days
    character(4) dateSY
    character(2) dateSM,dateSD,dateSH,dateSN,dateSS
    character(len=80) :: dateS
    integer :: fcst_ntasks

    type (mpas_pool_type), pointer :: mesh
    type(ESMF_Info) :: info
    integer :: ngrids=1
    logical, allocatable :: is_moving(:)

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call ESMF_VMGetCurrent(vm=vm,rc=rc); ESMF_ERR(rc)

    call ESMF_VMGet(vm=vm, localPet=mype, mpiCommunicator=fcst_mpi_comm%mpi_val, &
                    petCount=fcst_ntasks, rc=rc); ESMF_ERR(rc)
    if (mype == 0) write(*,*)'in fcst_initialize, fcst_ntasks=',fcst_ntasks

    !
    ! Set atmos time.
    !
    call ESMF_ClockGet(clock, CurrTime=CurrTime, StartTime=StartTime, StopTime=StopTime, rc=rc); ESMF_ERR(rc)

    date_init = 0
    call ESMF_TimeGet (StartTime,                      &
                       YY=date_init(1), MM=date_init(2), DD=date_init(3), &
                       H=date_init(4),  M =date_init(5), S =date_init(6), rc=rc); ESMF_ERR(rc)

    date=0
    call ESMF_TimeGet (CurrTime,                           &
                       YY=date(1), MM=date(2), DD=date(3), &
                       H=date(4),  M =date(5), S =date(6), rc=rc); ESMF_ERR(rc)

    date_end=0
    call ESMF_TimeGet (StopTime,                                       &
                       YY=date_end(1), MM=date_end(2), DD=date_end(3), &
                       H=date_end(4),  M =date_end(5), S =date_end(6), rc=rc); ESMF_ERR(rc)

    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, StartTime=',date_init
    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, CurrTime =',date
    if (mype == 0) write(*,'(A,6I5)') 'in fcst_initialize, StopTime =',date_end

    ! #######################################################################################
    ! Initialize component models.
    ! mpas_init() calls the MPAS initialization.
    ! #######################################################################################
    call mpas_init(corelist, domain, external_comm=fcst_mpi_comm)

    call mpas_pool_get_subpool(domain % blocklist % structs, 'mesh', mesh)
    call mpas_pool_get_dimension(mesh, 'nCellsSolve',    nCellsSolve)
    call mpas_pool_get_dimension(mesh, 'nVerticesSolve', nVerticesSolve)
    call mpas_pool_get_dimension(mesh, 'nEdgesSolve',    nEdgesSolve)
    call mpas_pool_get_dimension(mesh, 'nVertLevels',    nVertLevels)

    call mpas_dmpar_sum_int(domain % dminfo, nVerticesSolve, nVerticesGlobal)
    call mpas_dmpar_sum_int(domain % dminfo, nCellsSolve, nCellsGlobal)
    call mpas_dmpar_sum_int(domain % dminfo, nEdgesSolve, nEdgesGlobal)

    ! if (mype == 0) then
    !    write(0,*)'nCellsSolve     = ', nCellsSolve
    !    write(0,*)'nVerticesSolve  = ', nVerticesSolve
    !    write(0,*)'nVertLevels     = ', nVertLevels
    !    write(0,*)'nVerticesGlobal = ', nVerticesGlobal
    !    write(0,*)'nCellsGlobal    = ', nCellsGlobal
    !    write(0,*)'nEdgesGlobal    = ', nEdgesGlobal
    ! end if

    call ufs_mpas_create_history_bundle(history_field_bundle, rc=rc); ESMF_ERR(rc)
    call ESMF_StateAdd(exportState, (/ history_field_bundle /), rc=rc); ESMF_ERR(rc)

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

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

  end subroutine fcst_advertise

  ! ###########################################################################################
  ! Realize the ESMF forecast grid component.
  ! ###########################################################################################
  subroutine fcst_realize(fcst_comp, importState, exportState, clock, rc)
    type(esmf_GridComp) :: fcst_comp
    type(ESMF_State)    :: importState, exportState
    type(esmf_Clock)    :: clock
    integer,intent(out) :: rc

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

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
    integer             :: seconds
    integer             :: fcst_seconds, fcst_days
    real(kind=8)        :: mpi_wtime, tbeg1

    type (ESMF_Time) :: ufsCurrTime
    type (MPAS_Time_Type) :: startTime, currTime, stepStopTime
    type (MPAS_Clock_type), pointer :: mpas_clock
    type (MPAS_TimeInterval_type) :: atmTimeStep
    character(len=StrKIND) :: timeStamp
    character(len=64) :: fname

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call ESMF_ClockGet(clock, CurrTime=ufsCurrTime, rc=rc); ESMF_ERR(rc)

    mpas_clock => domain % clock

    currTime = mpas_get_clock_time(mpas_clock, MPAS_NOW, ierr=ierr)
    call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)

    ! Assert that the UFS and MPAS clocks are in sync
    ASSERT (ufsCurrTime == currTime % t)

    ! Set MPAS's clock stop dt_atmos seconds from current time
    ! This will make MPAS run dt_atmos/config_dt steps, then return
    call mpas_set_timeInterval(atmTimeStep, S=dt_atmos, ierr=ierr)
    stepStopTime = currTime + atmTimeStep
    call mpas_set_clock_time(mpas_clock, stepStopTime, MPAS_STOP_TIME, ierr=ierr)

    call mpas_run(domain)

    ! The MPAS's clock has advanced in mpas_run, look at the MPAS's current time,
    ! and compute number of seconds since the start time, to determine if
    ! it's time for output
    currTime = mpas_get_clock_time(mpas_clock, MPAS_NOW, ierr=ierr)
    startTime = mpas_get_clock_time(mpas_clock, MPAS_START_TIME, ierr)
    call mpas_get_timeInterval(currTime-startTime, DD=fcst_days, S=fcst_seconds, ierr=ierr)
    call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)

    seconds = (fcst_days*86400 + fcst_seconds)

    if (ANY(nint(output_fh(:)*3600.0) == seconds)) then
       call ufs_mpas_update_history_bundle(history_field_bundle, rc=rc); ESMF_ERR(rc)
    end if

    ! Timing info (debug mode)
    if (mype == 0) write(*,'(A,I16,A,F16.6)')'PASS(fcstRUN phase 1), n_atmsteps = ', &
                                               n_atmsteps,' time is ',mpi_wtime()-tbeg1
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
    integer             :: seconds
    real(kind=8)        :: mpi_wtime, tbeg1

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

    ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    ! Timing info (debug mode)
    if (mype == 0) write(*,'(A,I16,A,F16.6)')'PASS(fcstRUN phase 2), n_atmsteps = ', &
                                               n_atmsteps,' time is ',mpi_wtime()-tbeg1
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
    real(kind=8)        :: mpi_wtime, tbeg1

    ! Initialize ESMF error message.
    rc = ESMF_SUCCESS

     ! Timing info (debug mode)
    tbeg1 = mpi_wtime()

    call mpas_finalize(corelist, domain)

    ! Timing info (debug mode)
    if (mype == 0) write(*,*)'PASS(fcst_finalize): total is ', mpi_wtime() - tbeg1

  end subroutine fcst_finalize
end module  module_fcst_grid_comp
