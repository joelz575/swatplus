      subroutine pl_burnop (jj, iplant, iburn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ibrn        |none          |counter in readmgt 
!!    iburn(:     |julian date   |date of burning                       
!!    phub        |              |heat units to schedule burning
!!    pburn       |              |amount of phosphorus that burns - removed from plant
!!                               |phosphorus and added to soil organic phosphorus 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use jrw_datalib_module
      use organic_mineral_mass_module
   
      integer :: j
      integer, intent (in) :: jj, iplant, iburn
      real :: fr_burn

      j = jj

      !!update curve number
      cnop = cn2(j) + fire_db(iburn)%cn2_upd
      call curno(cnop,j)
      
      !!burn biomass and residue
      fr_burn = fire_db(iburn)%fr_burn
      pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass * fr_burn
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass * fr_burn
      pburn = pcom(ihru)%plm(ipl)%pmass * fr_burn
      soil1(j)%hp(1)%p = soil1(j)%hp(1)%p + pburn
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - pburn
      soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd * fr_burn
      rsd1(j)%tot(1)%n = rsd1(j)%tot(1)%n * fr_burn
      soil1(jj)%hs(1)%n = soil1(jj)%hs(1)%n * fr_burn
      soil1(j)%hp(1)%n = soil1(j)%hp(1)%n* fr_burn

      !!insert new biomss by zhang	  
      !!=================================
      if (bsn_cc%cswat == 2) then
          rsd1(j)%meta%m = rsd1(j)%meta%m * fr_burn
          rsd1(j)%str%m = rsd1(j)%str%m * fr_burn
          rsd1(j)%str%c = rsd1(j)%str%c * fr_burn
          rsd1(j)%str%n = rsd1(j)%str%n * fr_burn
          rsd1(j)%meta%c = rsd1(j)%meta%c * fr_burn
          rsd1(j)%meta%n = rsd1(j)%meta%n * fr_burn
          rsd1(j)%lig%m = rsd1(j)%lig%m * fr_burn  

          emitc_d(j) = emitc_d(j) + pcom(j)%plm(ipl)%mass * (1. - fr_burn)
          emitc_d(j) = emitc_d(j) + soil(j)%ly(1)%rsd * (1. - fr_burn)  
      end if 
      !!insert new biomss by zhang
      !!=================================

      return
      end subroutine pl_burnop