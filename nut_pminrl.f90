      subroutine nut_pminrl
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru         |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bk          |
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    rmp1        |kg P/ha       |amount of phosphorus moving from the solution
!!                               |mineral to the active mineral pool in the
!!                               |soil layer
!!    roc         |kg P/ha       |amount of phosphorus moving from the active
!!                               |mineral to the stable mineral pool in the 
!!                               |soil layer
!!    rto         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use parm, only : soil, ihru
      use output_landscape_module, only : hnb_d

      real, parameter :: bk = .0006
      integer :: j, l
      real :: rto, rmp1, roc

      j = ihru

      hnb_d(j)%rmp1 = 0.
      hnb_d(j)%roc = 0.

      rto = bsn_prm%psp / (1.-bsn_prm%psp)

      do l = 1, soil(j)%nly
        rmp1 = (soil1(j)%mp(l)%lab - soil1(j)%mp(l)%act * rto)
        !! mike changed/added per isabelle beaudin's email from 01/21/09
        if (rmp1 > 0.) rmp1 = rmp1 * 0.1
        if (rmp1 < 0.) rmp1 = rmp1 * 0.6
        !! mike changed/added per isabelle beaudin's email from 01/21/09
        rmp1 = Min(rmp1, soil1(j)%mp(l)%lab)

        roc = bk * (4. * soil1(j)%mp(l)%act - soil1(j)%mp(l)%sta)
        if (roc < 0.) roc = roc * .1
        roc = Min(roc, soil1(j)%mp(l)%act)

        soil1(j)%mp(l)%sta = soil1(j)%mp(l)%sta + roc
        if (soil1(j)%mp(l)%sta < 0.) soil1(j)%mp(l)%sta = 0.

        soil1(j)%mp(l)%act = soil1(j)%mp(l)%act - roc + rmp1
        if (soil1(j)%mp(l)%act < 0.) soil1(j)%mp(l)%act = 0.

        soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab - rmp1
        if (soil1(j)%mp(l)%lab < 0.) soil1(j)%mp(l)%lab = 0.

        hnb_d(j)%rmp1 = hnb_d(j)%rmp1 + rmp1
        hnb_d(j)%roc = hnb_d(j)%roc + roc
      end do

      return
      end subroutine nut_pminrl