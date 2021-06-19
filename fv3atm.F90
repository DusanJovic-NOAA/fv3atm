#define ESMF_ERR_ABORT(rc)  if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
#define ESMF_ERR_RETURN(rc) if (ESMF_LogFoundError(rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

module fv3atm_driver

  use ESMF
  use NUOPC
  use NUOPC_Driver, fv3atm_driverSS => SetServices
  use fv3gfs_cap_mod, only: fv3atmSS=> SetServices

  implicit none

  private
  public :: SetServices

  contains

  subroutine SetServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Driver
    call NUOPC_CompDerive(driver, fv3atm_driverSS, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! specialize driver
    call NUOPC_CompSpecialize(driver, specLabel=label_SetModelServices, &
                                      specRoutine=SetModelServices, rc=rc)
    ESMF_ERR_RETURN(rc)

    ! Load the required entries from the fd_nems.yaml file
    call NUOPC_FieldDictionarySetup("fd_nems.yaml", rc=rc)
    ESMF_ERR_RETURN(rc)

  end subroutine SetServices

  subroutine SetModelServices(driver, rc)
    type(ESMF_GridComp)  :: driver
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! SetServices for FV3ATM
    call NUOPC_DriverAddComp(driver, "FV3ATM Model", fv3atmSS, rc=rc)
    ESMF_ERR_RETURN(rc)

  end subroutine SetModelServices

end module fv3atm_driver
!-------------------------------------------------------------------------------
program fv3atm

    use ESMF
    use fv3atm_driver, only: fv3atm_driverSS => SetServices

    implicit none

    integer :: nseconds_fcst, yy, mm, dd, hh, mns, sec, fhrot
    real :: nhours_fcst

    type(ESMF_LogKind_Flag) :: logkindflag = ESMF_LOGKIND_NONE
    type(ESMF_TimeInterval) :: runDuration, timeStep, restartOffset
    type(ESMF_Time) :: currTime, startTime
    type(ESMF_GridComp) :: fv3atmComp
    type(ESMF_Clock) :: clock_fv3atm
    type(ESMF_Config) :: config

    logical :: print_esmf
    integer :: rc, urc

    call check_esmf_pet(print_esmf)
    if (print_esmf) logkindflag = ESMF_LOGKIND_MULTI

!** Initialize the ESMF framework.
    call ESMF_Initialize(defaultCalKind=ESMF_CALKIND_GREGORIAN &
                        ,logkindflag   =logkindflag            &
                        ,rc            =rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_LogSet(flush=.false., rc=rc)
    ESMF_ERR_ABORT(rc)

!** Create, load and parse main configuration file
    config = ESMF_ConfigCreate(rc=rc)

    call ESMF_ConfigLoadFile(config=config, filename='model_configure', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=yy, label='start_year:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=mm, label='start_month:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=dd, label='start_day:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=hh, label='start_hour:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=mns, label='start_minute:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=sec, label='start_second:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=nhours_fcst, label='nhours_fcst:', rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigGetAttribute(config=config, value=fhrot, label='fhrot:', default=0, rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_ConfigDestroy(config=config, rc=rc)
    ESMF_ERR_ABORT(rc)

!** Create the Main Clock
    call ESMF_TimeSet(time=startTime, yy=yy, mm=mm, dd=dd, h=hh, m=mns, s=sec, rc=rc)
    ESMF_ERR_ABORT(rc)

    nseconds_fcst=NINT(nhours_fcst*3600.)

    call ESMF_TimeIntervalSet(timeinterval=runDuration, s=nseconds_fcst, rc=rc)
    ESMF_ERR_ABORT(rc)

    ! NUOPC requires a correct timeStep to function. Here timeStep was just created
    ! pro forma, but now is replaced with runDuration to become meaningful.
    timeStep = runDuration

    clock_fv3atm = ESMF_ClockCreate(name='clock_fv3atm'     &
                                   ,timeStep   =timeStep    &
                                   ,startTime  =startTime   &
                                   ,runDuration=runDuration &
                                   ,rc         =rc)
    ESMF_ERR_ABORT(rc)

    ! Adjust the currTime of the clock_fv3atm if the fhrot is > 0
    ! This will correctly set the FV3ATM clock in case of Restart-From-History
    if (fhrot > 0) then
      call ESMF_TimeIntervalSet(restartOffset, h=fhrot, rc=rc)
      ESMF_ERR_ABORT(rc)
      currTime = startTime + restartOffset
      call ESMF_ClockSet(clock_fv3atm, currTime=currTime, rc=rc)
      ESMF_ERR_ABORT(rc)
    endif

!** Create FV3ATM Component and execute Init, Run and Finalize phases
    fv3atmComp = ESMF_GridCompCreate(name='FV3ATM Grid Comp', rc= rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_GridCompSetServices(fv3atmComp, fv3atm_driverSS, rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_GridCompInitialize(fv3atmComp, clock=clock_fv3atm, userRc=urc, rc=rc)
    ESMF_ERR_ABORT(rc)
    ESMF_ERR_ABORT(urc)

    call ESMF_GridCompRun       (fv3atmComp, clock=clock_fv3atm, userRc=urc, rc=rc)
    ESMF_ERR_ABORT(rc)
    ESMF_ERR_ABORT(urc)

    call ESMF_GridCompFinalize  (fv3atmComp, clock=clock_fv3atm, userRc=urc, rc=rc)
    ESMF_ERR_ABORT(rc)
    ESMF_ERR_ABORT(urc)

!** Destroy clock, FV3ATM Component and finalize ESMF
    call ESMF_ClockDestroy(clock_fv3atm, rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_GridCompDestroy(fv3atmComp, rc=rc)
    ESMF_ERR_ABORT(rc)

    call ESMF_Finalize()

!** That's all folks! Have a nice day!

end program fv3atm
!-------------------------------------------------------------------------------
subroutine check_esmf_pet(print_esmf)

    use ESMF
    implicit none
    logical,intent(out) :: print_esmf
    integer :: i, n, rc
    character(len=256) :: c1,c2

    print_esmf = .false.
    call ESMF_UtilIOUnitGet(unit=n, rc=rc)
    ESMF_ERR_ABORT(rc)

    open(unit=n,file='model_configure',status='old',action='read')

    do i=1,10000
      read(n,*,end=22)c1,c2
      if(c1(1:10) == 'print_esmf') then
        if( c2=='true' .or. c2=='.true.' .or. c2=='TRUE' .or. c2=='.TRUE.' ) print_esmf=.true.
        exit
      endif
    enddo
22  close(n)
    return

end subroutine check_esmf_pet
