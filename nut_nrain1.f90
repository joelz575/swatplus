      subroutine nut_nrain1
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds nitrate from rainfall to the soil profile
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! NH3PCP
!! NO3PCP     |mm H2O        |precipitation for the day in HRU 
!! PRECIPDAY
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
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

      use parm, only : hru, soil, atmodep, no3pcp
      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module


      j = ihru
      
      !! calculate nitrogen in precipitation
       if (bsn_cc%atmo == 2) then
            nh3pcp = .01 * atmodep(iadep)%nh4_rfmo(mo_atmo) * precipday
            no3pcp = .01 * atmodep(iadep)%no3_rfmo(mo_atmo) * precipday
            soil1(j)%nh4(1) = nh3pcp + atmodep(iadep)%nh4_drymo(mo_atmo)
            soil1(j)%no3(1) = no3pcp + atmodep(iadep)%no3_drymo(mo_atmo)
       else
            nh3pcp = .01 * atmodep(iadep)%nh4_rf * precipday
            no3pcp = .01 * atmodep(iadep)%no3_rf * precipday
            soil1(j)%nh4(1) = soil1(j)%nh4(1) + nh3pcp +          &
                       atmodep(iadep)%nh4_dry / 365.
            soil1(j)%no3(1) = soil1(j)%no3(1) + no3pcp +          &
                       atmodep(iadep)%no3_dry / 365.
       endif

      return
      end subroutine nut_nrain1