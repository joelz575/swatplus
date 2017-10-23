      subroutine nut_nrain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hydrograph_module
      use parm, only : hru, atmodep, ihru, iadep, mo_atmo, no3pcp, precipday 

      real :: nh3pcp

      j = ihru
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
        
      !! calculate nitrogen in precipitation
       if (bsn_cc%atmo == 2) then
            nh3pcp = .01 * atmodep(iadep)%nh4_rfmo(mo_atmo) * precipday
            no3pcp = .01 * atmodep(iadep)%no3_rfmo(mo_atmo) * precipday
            rsd1(j)%mn%nh4 = nh3pcp + atmodep(iadep)%nh4_drymo(mo_atmo)
            rsd1(j)%mn%no3 = rsd1(j)%mn%no3 + .8 * rmn1
       else
            nh3pcp = .01 * atmodep(iadep)%nh4_rf * precipday
            no3pcp = .01 * atmodep(iadep)%no3_rf * precipday
            rsd1(j)%mn%nh4 = rsd1(j)%mn%nh4 + nh3pcp + atmodep(iadep)%nh4_dry / 365.
            rsd1(j)%mn%no3 = rsd1(j)%mn%no3 + no3pcp + atmodep(iadep)%no3_dry / 365.
       endif

      return
      end subroutine nut_nrain