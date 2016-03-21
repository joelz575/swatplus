      subroutine albedo
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates albedo in the HRU for the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo of ground for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cej         |none          |constant
!!    eaj         |none          |soil cover index
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer :: j
      real :: cej, eaj

      j = 0
      j = ihru

!! calculate albedo
      cej = -5.e-5
      eaj = 0.
      eaj = Exp(cej * (sol_cov(j) + .1))   !! equation 2.2.16 in SWAT manual

      if (sno_hru(j) <= .5) then
        !! equation 2.2.14 in SWAT manual
        albday = hru(j)%sol%crk

        !! equation 2.2.15 in SWAT manual
        if (sumlai > 0.)albday = .23 * (1. - eaj) + hru(j)%sol%crk * eaj
      else
        !! equation 2.2.13 in SWAT manual
        albday = 0.8
      end if

      return
      end subroutine albedo