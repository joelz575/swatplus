       subroutine pl_irrigate(jj,volmm)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies irrigation water to HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    curyr       |none          |current year of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!                               |day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fcx         |mm H2O        |amount of water stored in soil layer when
!!                               |moisture content is at field capacity
!!    jj          |none          |HRU number
!!    k           |none          |counter (soil layers)
!!    stx         |mm H2O        |amount of water stored in soil layer on 
!!                               |current day
!!    volmm       |mm H2O        |depth irrigation water applied to HRU
!!    yy          |mm H2O        |amount of water added to soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      
      integer, intent (in) :: jj
      real, intent (in out) :: volmm
      integer :: k
      real :: fcx, stx, yy

!! if unlimited source, store volume and runoff to send to swr_percmain and surface
      if (hru(jj)%irrsrc == 0) then
        !!unlimited source
        aird(jj) = volmm * (1. - sq_rto)
        qird(jj) = volmm * sq_rto
      else
        !!set demand for water rights object in water_allocation
        !wat_allo(iwr)%demand() = volmm * hru(jj)%ha * 10.
      end if

!! summary calculations
      if (time%yrs > pco%nyskip) then 
        aairr(jj) = aairr(jj) + aird(jj)
      end if

      return
      end subroutine pl_irrigate