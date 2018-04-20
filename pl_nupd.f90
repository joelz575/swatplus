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
!!    unmx        |kg N/ha       |maximum amount of nitrogen that can be 
!!                               |removed from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nfix, nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use plant_data_module
      use hru_module, only : pcom, un2, uno3d, ihru, ipl
      
      implicit none      
      
      integer :: icrop       !none      |land cover code
      integer :: j           !none      |hru number
      integer :: l           !none      |counter (soil layer)
      real :: uno3l          !kg N/ha   |plant nitrogen demand
      integer :: ir          !none      |flag to denote bottom of root zone reached
      integer :: idp         !          |       
      real :: gx             !mm        |lowest depth in layer from which nitrogen
                             !          |may be removed

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