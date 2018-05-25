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
!!    ihru        |none           |HRU number
!!    pltnfr(1,:) |kg N/kg biomass|nitrogen uptake parameter #1: normal fraction
!!                                |of N in crop biomass at emergence
!!    pltnfr(3,:) |kg N/kg biomass|nitrogen uptake parameter #3: normal fraction
!!                                |of N in crop biomass at maturity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    un2         |kg N/ha       |ideal plant nitrogen content
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nfix, nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : uno3d, un2, nplnt, fixn, ihru, ipl, rto_no3, sol_rd, uptake
      use soil_module
      use plant_module

      implicit none

      integer :: icrop       !none      |land cover code
      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      real :: uno3l          !kg N/ha   |plant nitrogen demand
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          |       
      real :: unmx           !kg N/ha   |maximum amount of nitrogen that can be 
                             !          |removed from soil layer
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed
      real :: uobn           !none      |nitrogen uptake normalization parameter
                             !          |This variable normalizes the nitrogen uptake
                             !          |so that the model can easily verify that
                             !          |upake from the different soil layers sums to 1.0
      real :: xx             !          |  
      integer :: max         !          |
  
      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      pcom(j)%plstr(ipl)%strsn = 1.
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

        unmx = uno3d(ipl) * rto_no3 * (1. - Exp(-bsn_prm%n_updis * gx / sol_rd)) / uptake%n_norm
        uno3l = Min(unmx - nplnt(j), soil1(j)%mn(l)%no3)
        nplnt(j) = nplnt(j) + uno3l 
        soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 - uno3l
      end do
      if (nplnt(j) < 0.) nplnt(j) = 0.

!! if crop is a legume, call nitrogen fixation routine
      !select case (pldb(idp)%idc)
        !case (1,2,3)
      if (pldb(idp)%idc == 'warm_annual_legume' .or. pldb(idp)%idc == 'cold_annual_legume' .or.  &
          pldb(idp)%idc == 'perennial_legume') then
          call pl_nfix
      end if

      nplnt(j) = nplnt(j) + fixn
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass + nplnt(j)
 
!! compute nitrogen stress
      !select case (pldb(idp)%idc)
        !case (1,2,3)
      if (pldb(idp)%idc == 'warm_annual_legume' .or. pldb(idp)%idc == 'cold_annual_legume' .or.  &
          pldb(idp)%idc == 'perennial_legume') then
          pcom(j)%plstr(ipl)%strsn = 1.
        !case default
      else
         call nuts (pcom(j)%plm(ipl)%nmass, un2(ipl), pcom(j)%plstr(ipl)%strsn)
          if (uno3d(ipl) > 1.e-5) then
            xx = nplnt(j) / uno3d(ipl)
          else
            xx = 1.
          end if
          pcom(j)%plstr(ipl)%strsn = Max(pcom(j)%plstr(ipl)%strsn, xx)
          pcom(j)%plstr(ipl)%strsn = amin1(pcom(j)%plstr(ipl)%strsn, 1.)
      end if

      return
      end subroutine pl_nup