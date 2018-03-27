      subroutine pl_pup

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pup1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    pup2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    ihru        |none           |HRU number
!!    pltpfr(1,:) |kg P/kg biomass|phosphorus uptake parameter #1: normal
!!                                |fraction of P in crop biomass at emergence
!!    pltpfr(3,:) |kg P/kg biomass|phosphorus uptake parameter #3: normal
!!                                |fraction of P in crop biomass at maturity
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~     
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |mm            |lowest depth in layer from which phosphorus
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag for bottom of root zone
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layers)
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    uapl        |kg P/ha       |amount of phosphorus removed from layer
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : pcom, soil, uapd, up2, pplnt, ihru, ipl, rto_solp, sol_rd, uptake

      integer :: j, icrop, l, ir
      real :: uapl, gx

      j = 0
      j = ihru

      pcom(j)%plstr(ipl)%strsp = 1.
      ir = 0
      if (uapd(ipl) < 1.e-6) return

      do l = 1, soil(j)%nly
        if (ir > 0) exit

        gx = 0.
        if (sol_rd <= soil(j)%phys(l)%d) then
          gx = sol_rd
          ir = 1
        else
          gx = soil(j)%phys(l)%d
        end if

        upmx = 0.
        uapl = 0.
        upmx = uapd(ipl) * rto_solp * (1. - Exp(-bsn_prm%p_updis * gx /  &
                                  sol_rd)) / uptake%p_norm
        uapl = Min(upmx - pplnt(j), soil1(j)%mp(l)%lab)
        pplnt(j) = pplnt(j) + uapl
        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab - uapl
      end do
      if (pplnt(j) < 0.) pplnt(j) = 0.

      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass + pplnt(j)

!! compute phosphorus stress
      call nuts(pcom(ihru)%plm(ipl)%pmass, up2(ipl),                     &  
                pcom(j)%plstr(ipl)%strsp)

      return
      end subroutine pl_pup