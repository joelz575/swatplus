      subroutine mgt_harvgrainop (jj, iplant, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest grain only operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnop        |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    hru_dafr(:) |km2/km2        |fraction of watershed in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    ihru        |none           |HRU number
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!                                |organic (residue) pool0
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hiad1       |
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    wur         |
!!    yield       |kg            |yield (dry weight)
!!    yieldn      |
!!    yieldp      |
!!    yldpst      |kg pst/ha     |pesticide removed in yield
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : ihru, cnop, harveff, ipl, yield
      use plant_module
      use plant_data_module
      use mgt_operations_module
      use carbon_module
      
      implicit none
 
      integer :: j                     !none           |HRU number
      integer :: k                     !none           |counter
      integer :: idp                   !               |
      integer, intent (in) :: jj       !none           |counter
      integer, intent (in) :: iplant   !               |plant number xwalked from hlt_db()%plant and plants.plt
      integer, intent (in) :: iharvop  !               |harvest operation type
      real :: xx                       !varies         |variable to hold calculation results 
      real :: hiad1                    !none           |actual harvest index (adj for water/growth)
      real :: wur                      !none           |water deficiency factor
      real :: resnew                   !               |
      real :: yieldn                   !               |
      real :: yieldp                   !               |
      real :: yldpst                   !kg pst/ha      |pesticide removed in yield

      j = jj
      ipl = iplant
      idp = pcom(j)%plcur(ipl)%idplt
      harveff = harvop_db(iharvop)%eff

      if (pcom(j)%plg(ipl)%plpet < 10.) then
        wur = 100.
      else
        wur = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plpet
      endif

      hiad1 = (pcom(j)%plg(ipl)%hvstiadj - pldb(idp)%wsyf) *           &   
            (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf
      if (hiad1 > pldb(idp)%hvsti) hiad1 = pldb(idp)%hvsti

!! check if yield is from above or below ground
      if (pldb(idp)%hvsti > 1.001) then
        yield = pcom(j)%plm(ipl)%mass * (1. - 1. / (1. + hiad1))
      else
        yield = pcom(j)%ab_gr(ipl)%mass * hiad1
      endif
       if (yield < 0.) yield = 0.
      yield = yield * harveff

      !!add by zhang
      !!====================
      if (bsn_cc%cswat == 2) then
        cbn_loss(j)%grainc_d = cbn_loss(j)%grainc_d + yield * 0.42
      end if
      !!add by zhang
      !!====================      
      
!! calculate nutrients removed with yield
      yieldn = yield * pldb(idp)%cnyld
      yieldp = yield * pldb(idp)%cpyld
      yieldn = Min(yieldn, 0.85 * pcom(j)%plm(ipl)%nmass)
      yieldp = Min(yieldp, 0.85 * pcom(ihru)%plm(ipl)%pmass)
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - yieldn
      pcom(j)%plm(ipl)%nmass = Max(0.,pcom(j)%plm(ipl)%nmass)
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - yieldp
      pcom(ihru)%plm(ipl)%pmass = Max(0.,pcom(ihru)%plm(ipl)%pmass)

!! summary calculations
       xx = pcom(j)%plm(ipl)%mass
       pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - yield
       pcom(j)%root(ipl)%mass = pcom(j)%root(ipl)%mass * xx /                &
         (pcom(j)%plm(ipl)%mass + 1.e-6)

!! update curve number
      if (cnop > 0.) call curno(cnop,j)

      return
      end  subroutine mgt_harvgrainop