      subroutine hru_sweep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    the subroutine performs the street sweeping operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fr_curb        |none          |availability factor, the fraction of the 
!!                                  |curb length that is sweepable
!!    dirtmx(:)      |kg/curb km    |maximum amount of solids allowed t
!!                                  |build up on impervious surfaces
!!    ihru           |none          |HRU number
!!    sweepeff       |none          |removal efficiency of sweeping
!!                                  |operation
!!    thalf(:)       |days          |time for the amount of solids on
!!                                  |impervious areas to build up to 1/2
!!                                  |the maximum level
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    twash(:)     |days          |time that solids have built-up on streets
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dirt         |kg/curb km    |amount of solids built up on impervious
!!                                |surfaces
!!    j            |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer :: j
      real :: dirt

      j = 0
      j = ihru
     
!! calculate amount of dirt on streets prior to sweeping
      dirt = 0.
      dirt = urbdb(ulu)%dirtmx * twash(j) / (urbdb(ulu)%thalf +twash(j))

!! calculate reduced amount of solid built up on impervious areas
      dirt = dirt * (1. - fr_curb * sweepeff)
      if (dirt < 1.e-6) dirt = 0.

!! set time to correspond to lower amount of dirt
      twash(j) = 0.
      twash(j) = urbdb(ulu)%thalf * dirt / (urbdb(ulu)%dirtmx - dirt)

      return
      end subroutine hru_sweep