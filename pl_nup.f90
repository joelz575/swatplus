      subroutine pl_nup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nup1(:)   |none           |1st shape parameter for plant N uptake
!!                                |equation
!!    nup2(:)   |none           |2nd shape parameter for plant N uptake
!!                                |equation
!!    bioday      |kg             |biomass generated on current day in HRU
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    ihru        |none           |HRU number
!!    pltnfr(1,:) |kg N/kg biomass|nitrogen uptake parameter #1: normal fraction
!!                                |of N in crop biomass at emergence
!!    pltnfr(3,:) |kg N/kg biomass|nitrogen uptake parameter #3: normal fraction
!!                                |of N in crop biomass at maturity
!!    uobn        |none           |nitrogen uptake normalization parameter
!!                                |This variable normalizes the nitrogen uptake
!!                                |so that the model can easily verify that
!!                                |upake from the different soil layers sums to
!!                                |1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |mm            |lowest depth in layer from which nitrogen
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag to denote bottom of root zone reached
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    un2         |kg N/ha       |ideal plant nitrogen content
!!    unmx        |kg N/ha       |maximum amount of nitrogen that can be 
!!                               |removed from soil layer
!!    uno3l       |kg N/ha       |amount of nitrogen removed from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nfix, nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module

      integer :: j, icrop, l, ir
      real :: unmx, uno3l, gx

      j = ihru

      pcom(j)%plstr(ipl)%strsn= 1.
      ir = 0
      if (uno3d(ipl) < 1.e-6) return

      do l = 1, soil(j)%nly
        if (ir > 0) exit

        gx = 0.
        if (sol_rd <= soil(j)%phys(l)%d) then
          gx = sol_rd
          ir = 1
        else
          gx = soil(j)%phys(l)%d
        end if

        unmx = uno3d(ipl) * rto_no3 * (1. - Exp(-bsn_prm%n_updis *       &
                                                   gx / sol_rd)) / uobn
        uno3l = Min(unmx - nplnt(j), soil1(j)%mn(l)%no3)
        nplnt(j) = nplnt(j) + uno3l 
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 - uno3l
      end do
      if (nplnt(j) < 0.) nplnt(j) = 0.

!! if crop is a legume, call nitrogen fixation routine
      select case (pldb(idp)%idc)
        case (1,2,3)
          call pl_nfix
      end select

      nplnt(j) = nplnt(j) + fixn
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass + nplnt(j)
 
!! compute nitrogen stress
      select case (pldb(idp)%idc)
        case (1,2,3)
          pcom(j)%plstr(ipl)%strsn = 1.
        case default
         call nuts(pcom(j)%plm(ipl)%nmass,un2(ipl),                      &
                   pcom(j)%plstr(ipl)%strsn)
          if (uno3d(ipl) > 1.e-5) then
            xx = nplnt(j) / uno3d(ipl)
          else
            xx = 1.
          end if
          pcom(j)%plstr(ipl)%strsn = Max(pcom(j)%plstr(ipl)%strsn, xx)
          pcom(j)%plstr(ipl)%strsn = amin1(pcom(j)%plstr(ipl)%strsn, 1.)
      end select

      return
      end subroutine pl_nup