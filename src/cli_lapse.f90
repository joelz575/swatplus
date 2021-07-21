      subroutine cli_lapse (iob, iwst)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adjusts precip and temperature for elevation

      use basin_module
      use climate_module
      use hydrograph_module, only : ob
      
      implicit none

      integer, intent (in)  :: iob      !       |object number
      integer, intent (in)  :: iwst     !       |weather station number
      integer :: iwgn                   !       |weather generator station number
      integer :: igage                  !       |number of measured precip or temperature gage
      
      !! adjust precip and temperature for elevation using lapse rates
      if (wst(iwst)%wco_c%pgage == "sim") then
        iwgn = wst(iwst)%wco%wgn
        w%precip = w%precip + bsn_prm%plaps * (ob(iob)%elev - wgn(iwgn)%elev) / 1000.
        w%precip = amax1 (0., w%precip)
      else
        igage = wst(iwst)%wco%pgage
        w%precip = w%precip + bsn_prm%plaps * (ob(iob)%elev - pcp(igage)%elev) / 1000.
        w%precip = amax1 (0., w%precip)
      end if
      if (wst(iwst)%wco_c%tgage == "sim") then
        iwgn = wst(iwst)%wco%wgn
        w%tmax = w%tmax + bsn_prm%tlaps * (ob(iob)%elev - wgn(iwgn)%elev - ob(iob)%elev) / 1000.
        w%tmin = w%tmin + bsn_prm%tlaps * (ob(iob)%elev - wgn(iwgn)%elev - ob(iob)%elev) / 1000.
        w%tave = w%tave + bsn_prm%tlaps * (ob(iob)%elev - wgn(iwgn)%elev - ob(iob)%elev) / 1000.
      else
        igage = wst(iwst)%wco%tgage
        w%tmax = w%tmax + bsn_prm%tlaps * (ob(iob)%elev - tmp(igage)%elev) / 1000.
        w%tmin = w%tmin + bsn_prm%tlaps * (ob(iob)%elev - tmp(igage)%elev) / 1000.
        w%tave = w%tave + bsn_prm%tlaps * (ob(iob)%elev - tmp(igage)%elev) / 1000.
      end if
      
      return
      end subroutine cli_lapse
