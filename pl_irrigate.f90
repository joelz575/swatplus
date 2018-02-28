       subroutine pl_irrigate(jj, volmm, irrop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies irrigation water to HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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

      use jrw_datalib_module, only : irrop_db
      use hru_module, only : hru, aird, qird
      
      integer, intent (in) :: jj, irrop
      real, intent (in out) :: volmm

      !! if unlimited source, store volume and runoff to send to swr_percmain and surface
      if (hru(jj)%irrsrc == 0) then
        !!unlimited source
        aird(jj) = volmm * (1. - irrop_db(ir)%surq)
        qird(jj) = volmm * irrop_db(ir)%surq
      else
        !!set demand for water rights object in water_allocation
        !wat_allo(iwr)%demand() = volmm * hru(jj)%ha * 10.
      end if

      return
      end subroutine pl_irrigate