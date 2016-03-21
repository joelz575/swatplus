      subroutine nut_pminrl
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
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
!!    rmn1        |kg P/ha       |amount of phosphorus moving from the solution
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

      real, parameter :: bk = .0006
      integer :: j, l
      real :: rto, rmn1, roc

      j = 0
      j = ihru

      rto = 0.
      rto = bsn_prm%psp / (1.-bsn_prm%psp)

      do l = 1, hru(j)%sol%nly
        rmn1 = 0.
        rmn1 = (soil(j)%nut(l)%solp - soil(j)%nut(l)%actp * rto)
!!  mike changed/added per isabelle beaudin's email from 01/21/09
        if (rmn1 > 0.) rmn1 = rmn1 * 0.1
        if (rmn1 < 0.) rmn1 = rmn1 * 0.6
!!  mike changed/added per isabelle beaudin's email from 01/21/09
        rmn1 = Min(rmn1, soil(j)%nut(l)%solp)

        roc = 0.
        roc = bk * (4. * soil(j)%nut(l)%actp - soil(j)%nut(l)%stap)
        if (roc < 0.) roc = roc * .1
        roc = Min(roc, soil(j)%nut(l)%actp)

        soil(j)%nut(l)%stap = soil(j)%nut(l)%stap + roc
        if (soil(j)%nut(l)%stap < 0.) soil(j)%nut(l)%stap = 0.

        soil(j)%nut(l)%actp = soil(j)%nut(l)%actp - roc + rmn1
        if (soil(j)%nut(l)%actp < 0.) soil(j)%nut(l)%actp = 0.

        soil(j)%nut(l)%solp = soil(j)%nut(l)%solp - rmn1
        if (soil(j)%nut(l)%solp < 0.) soil(j)%nut(l)%solp = 0.

      end do

      return
      end subroutine nut_pminrl