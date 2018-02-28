      subroutine pl_nupd
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen demand

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_n1(:)   |none           |1st shape parameter for plant N uptake
!!                                |equation
!!    bio_n2(:)   |none           |2nd shape parameter for plant N uptake
!!                                |equation
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

      use jrw_datalib_module, only : pldb, plcp
      use hru_module, only : pcom, un2, uno3d, ihru, ipl, idp 
      
      integer :: j, icrop, l, ir
      real :: unmx, uno3l, gx

      j = ihru

      idp = pcom(j)%plcur(1)%idplt
      
      pcom(j)%plm(ipl)%n_fr = (pldb(idp)%pltnfr1 - pldb(idp)%pltnfr3) *  &
          (1. -pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +  &      
          Exp(plcp(idp)%nup1 - plcp(idp)%nup2 *                          &
          pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltnfr3

      un2(ipl) = pcom(j)%plm(ipl)%n_fr * (1. - pcom(j)%plg(ipl)%rwt) * pcom(j)%plm(ipl)%mass
      if (un2(ipl) < pcom(j)%plm(ipl)%nmass) un2(ipl) = pcom(j)%plm(ipl)%nmass
      uno3d(ipl) = un2(ipl) - pcom(j)%plm(ipl)%nmass
      
      return 
      end subroutine pl_nupd