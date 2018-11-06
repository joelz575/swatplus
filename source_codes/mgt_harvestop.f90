      subroutine mgt_harvestop (jj, iplant, iharvop)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest operation (no kill)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none           |current year in simulation
!!    harveff       |none         |harvest efficiency: fraction of harvested 
!!                                |yield that is removed from HRU; the 
!!                                |remainder becomes residue on the soil
!!                                |surface
!!    hru_dafr(:) |km2/km2        |fraction of watershed area in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    ihru        |none           |HRU number
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    rsr1c(:)    |               |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |               |root to shoot ratio at end of growing season
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : hru, harveff, ihru
      use soil_module
      use plant_module
      use plant_data_module
      use mgt_operations_module
      use constituent_mass_module
      use carbon_module
      
      implicit none
     
      integer :: j                     !none           |HRU number
      integer :: k                     !none           |counter
      integer :: idp                   !               |
      integer, intent (in) :: jj       !none           |counter
      integer, intent (in) :: iplant   !               |plant number xwalked from hlt_db()%plant and plants.plt
      integer, intent (in) :: iharvop  !               |harvest operation type
      integer :: ipl                   !none           |counter
      integer :: orgc_f                !fraction       |fraction of organic carbon in fertilizer
      integer :: l                     !               |
      integer :: icmd                  !               |
      integer :: nssr                  !none           |the new mass of roots
      real :: resnew                   !               |
      real :: resnew_n                 !               |
      real :: hiad1                    !none           |actual harvest index (adj for water/growth)
      real :: wur                      !none           |water deficiency factor
      real :: clip                     !kg/ha          |yield lost during harvesting
      real :: clipn                    !kg N/ha        |nitrogen in clippings
      real :: clipp                    !kg P/ha        |phosphorus in clippings
      real :: clippst                  !kg pst/ha      |pesticide in clippings
      real :: yieldn                   !               |
      real :: yieldp                   !               |
      real :: yldpst                   !kg pst/ha      |pesticide removed in yield
      real :: rtresnew                 !               |
      real :: rtfr                     !none           |root fraction
      real :: rtres                    !               | 
      real :: rtresn                   !               |           
      real :: rtresp                   !               | 
      real :: ff3                      !               | 
      real :: ff4                      !               | 
      real :: xx                       !none           |variable to hold calculation value  
      real :: yield                    !kg             |yield (dry weight)
      real :: ssb                      !               |
      real :: ssp                      !               | 
      real :: ssn                      !               | 
      real :: ssr                      !               | 
      real :: ssabg                    !               | 
      real :: hi_ovr                   !!kg/ha)/(kg/ha)|harvest index target specified at harvest

      j = jj
      ipl = iplant
            
      idp = pcom(j)%plcur(ipl)%idplt
      hi_ovr = harvop_db(iharvop)%hi_ovr
      harveff = harvop_db(iharvop)%eff

      ssb = pcom(j)%plm(ipl)%mass                                       ! Armen 16 Jan 2009 storing info
      ssabg = pcom(j)%plm(ipl)%mass * (1.- pcom(j)%plg(ipl)%root_frac)  ! Armen 16 Jan 2009 storing info
      ssr = ssb * pcom(j)%root(ipl)%mass                                ! Armen 16 Jan 2009 storing info
      ssn = pcom(j)%plm(ipl)%nmass                                      ! Armen 20 May 2006 storing info
      ssp = pcom(ihru)%plm(ipl)%pmass                                   ! Armen 20 May 2006 storing info

      hiad1 = 0.
      if (hi_ovr > 0.) then
        hiad1 = hi_ovr
      else
        if (pcom(j)%plg(ipl)%plet < 10.) then
          wur = 100.
        else
          wur = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plet
        endif
        hiad1 = (pcom(j)%plg(ipl)%hvstiadj - pldb(idp)%wsyf) *          &   
            (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf
        if (hiad1 > pldb(idp)%hvsti) then
          hiad1 = pldb(idp)%hvsti
        end if
      end if

!! check if yield is from above or below ground
      if (pldb(idp)%hvsti > 1.001) then
        yield = pcom(j)%plm(ipl)%mass * (1. - 1. / (1. + hiad1))
      else
        yield = (1. - pcom(j)%plg(ipl)%root_frac) * pcom(j)%plm(ipl)%mass * hiad1
      endif
      if (yield < 0.) yield = 0.

!! determine clippings (biomass left behind) and update yield
      clip = yield * (1. - harveff)
      yield = yield * harveff
      if (yield < 0.) yield = 0.
      if (clip < 0.) clip = 0.

      if (hi_ovr > 0.) then
        !! calculate nutrients removed with yield
        yieldn = yield * pcom(j)%plm(ipl)%n_fr
        yieldp = yield * pcom(j)%plm(ipl)%p_fr
        yieldn = Min(yieldn, 0.80 * pcom(j)%plm(ipl)%nmass)    ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * pcom(ihru)%plm(ipl)%pmass) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = clip * pcom(j)%plm(ipl)%n_fr
        clipp = clip * pcom(j)%plm(ipl)%p_fr
        clipn = Min(clipn, pcom(j)%plm(ipl)%nmass - yieldn)
        clipp = Min(clipp, pcom(ihru)%plm(ipl)%pmass - yieldp)
      else
        !! calculate nutrients removed with yield
        yieldn = yield * pldb(idp)%cnyld
        yieldp = yield * pldb(idp)%cpyld
        yieldn = Min(yieldn, 0.80 * pcom(j)%plm(ipl)%nmass) ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * pcom(ihru)%plm(ipl)%pmass) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = clip * pldb(idp)%cnyld
        clipp = clip * pldb(idp)%cpyld
        clipn = Min(clipn, pcom(j)%plm(ipl)%nmass - yieldn)
        clipp = Min(clipp, pcom(ihru)%plm(ipl)%pmass - yieldp)
      endif

      yieldn = Max(yieldn,0.)
      yieldp = Max(yieldp,0.)
      clipn = Max(clipn,0.)
      clipp = Max(clipp,0.)
      
      resnew = clip 
      resnew_n = clipn
      call pl_leaf_drop (resnew, resnew_n)

	!! Calculation for dead roots allocations, resetting phenology, updating other pools
      if (ssabg > 1.e-6) then
        ff3 = (yield + clip) / ssabg	! Armen 20 May 2008 and 16 Jan 2009
      else
        ff3 = 1.
      endif 
      if (ff3 > 1.0) ff3 = 1.0

	  !! nssr is the new mass of roots
      nssr = pcom(j)%root(ipl)%mass * ssabg * (1. - ff3) / (1. - pcom(j)%plg(ipl)%root_frac)  
      rtresnew = ssr - nssr
      if (ssr > 1.e-6) then
       ff4 = rtresnew / ssr
      else
	   ff4 = 0.
      end if
      rtresn = ff4 * ssn
      rtresp = ff4 * ssp

      !! reset leaf area index and fraction of growing season
      if (ssb > 0.001) then
        pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai * (1. - ff3)
        if (pcom(j)%plg(ipl)%lai < pldb(idp)%alai_min) then   !Sue
          pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
        end if
        pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc * (1. - ff3) 
        pcom(j)%root(ipl)%mass = .4 - .2 * pcom(j)%plcur(ipl)%phuacc
      else
        pcom(j)%plm(ipl)%mass = 0.
        pcom(j)%plg(ipl)%lai = 0.
        pcom(j)%plcur(ipl)%phuacc = 0.
      endif

	  !! remove n and p in harvested yield, clipped biomass, and dead roots 
      pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - yield - rtresnew
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - yieldn - rtresn
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - yieldp - clipp - rtresp 
      if (pcom(j)%plm(ipl)%mass < 0.) pcom(j)%plm(ipl)%mass = 0.
      if (pcom(j)%plm(ipl)%nmass < 0.) pcom(j)%plm(ipl)%nmass = 0.
      if (pcom(ihru)%plm(ipl)%pmass < 0.) pcom(ihru)%plm(ipl)%pmass = 0.

	  !! adjust foliar pesticide for plant removal
        do k = 1, cs_db%num_pests
          !! calculate amount of pesticide removed with yield and clippings
          yldpst = 0.
          clippst = 0.
          if (pldb(idp)%hvsti > 1.001) then
            yldpst = pcom(j)%pest(k)
            pcom(j)%pest(k) = 0.
          else
            yldpst = hiad1 * pcom(j)%pest(k)
            pcom(j)%pest(k) = pcom(j)%pest(k) - yldpst
            if (pcom(j)%pest(k) < 0.)pcom(j)%pest(k) = 0.
          endif
          clippst = yldpst * (1. - harveff)
          if (clippst < 0.) clippst = 0.
          !! add pesticide in clippings to soil surface
          soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + clippst
        end do   

      return
      end subroutine mgt_harvestop