      subroutine mgt_harvgrainop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest grain only operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    cnop        |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                                |planting
!!    hru_dafr(:) |km2/km2        |fraction of watershed in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    ihru        |none           |HRU number
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
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
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
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
 
      integer :: j, k
      real :: wur, hiad1, resnew,  yieldn, yieldp, yldpst
      type (plant_mass) :: plant

      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt

      hiad1 = 0.
      if (hi_targ(j) > 0.) then
        hiad1 = hi_targ(j)
      else
        if (pcom(j)%plg(ipl)%plpet < 10.) then
          wur = 100.
        else
          wur = 0.
          wur = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plpet
        endif

        hiad1 = (pcom(j)%plg(ipl)%hvstiadj - pldb(idp)%wsyf) *           &   
            (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf

        if (hiad1 > pldb(idp)%hvsti) then 
          hiad1 = pldb(idp)%hvsti
        end if
      end if


!! check if yield is from above or below ground
      yield = 0.
      resnew = 0.
      if (pldb(idp)%hvsti > 1.001) then
        yield = pcom(j)%plm(ipl)%mass * (1. - 1. / (1. + hiad1))
      else
        yield = (1. - pcom(j)%plg(ipl)%rwt)*pcom(j)%plm(ipl)%mass*hiad1
      endif
       if (yield < 0.) yield = 0.
      yield = yield * harveff

      !!add by zhang
      !!====================
      
      if (bsn_cc%cswat == 2) then
        grainc_d(j) = grainc_d(j)+ yield * 0.42
      end if
      !!add by zhang
      !!====================      
      
!! calculate nutrients removed with yield
      yieldn = 0.
      yieldp = 0.
      yieldn = yield * pldb(idp)%cnyld
      yieldp = yield * pldb(idp)%cpyld
      yieldn = Min(yieldn, 0.85 * pcom(j)%plm(ipl)%nmass)
      yieldp = Min(yieldp, 0.85 * pcom(ihru)%plm(ipl)%pmass)
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - yieldn
      pcom(j)%plm(ipl)%nmass = amax1(0.,pcom(j)%plm(ipl)%nmass)
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - yieldp
      pcom(ihru)%plm(ipl)%pmass = amax1(0.,pcom(ihru)%plm(ipl)%pmass)

!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j)=(1. - pcom(j)%plg(ipl)%rwt) * pcom(j)%plm(ipl)%mass*      &
         pcom(j)%plm(ipl)%n_fr * auto_eff(j)

!! summary calculations
       xx = pcom(j)%plm(ipl)%mass
       pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - yield
       pcom(j)%plg(ipl)%rwt = pcom(j)%plg(ipl)%rwt * xx /                &
         pcom(j)%plm(ipl)%mass

!! update curve number
      if (cnop > 0.) call curno(cnop,j)

      return
      end  subroutine mgt_harvgrainop