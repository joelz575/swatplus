      subroutine albedo
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates albedo in the HRU for the day
!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo of ground for day
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface


      use hru_module, only : sol_cov, sno_hru, ihru, albday, sumlai
      use soil_module
      
      implicit none
        
      real :: cej       !none           |constant
      real :: eaj       !none           |soil cover index      
      integer :: j      ! none          |HRU number
      
      j = ihru

!! calculate albedo
      cej = -5.e-5
      eaj = Exp(cej * (sol_cov(j) + .1))   !! equation 2.2.16 in SWAT manual

      if (sno_hru(j) <= .5) then
        !! equation 2.2.14 in SWAT manual
        albday = soil(j)%ly(1)%alb

        !! equation 2.2.15 in SWAT manual
        if (sumlai > 0.) albday = .23 * (1. - eaj) + soil(j)%ly(1)%alb * eaj
      else
        !! equation 2.2.13 in SWAT manual
        albday = 0.8
      end if

      return
      end subroutine albedo