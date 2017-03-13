      subroutine pl_burnop    
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ibrn        |none          |counter in readmgt 
!!    iburn(:     |julian date   |date of burning  
!!    burn_frlb   |none          |fraction of biomass and residue that burn(input in
!!                               |management file) range (0 - 1.0)                         
!!    phub        |              |heat units to schedule burning
!!    pburn       |              |amount of phosphorus that burns - removed from plant
!!                               |phosphorus and added to soil organic phosphorus 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
   
      integer :: j
      real :: aphu

      j = 0
      j = ihru

      xx = burn_frlb
      pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass * xx
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass * xx
      pburn = pcom(ihru)%plm(ipl)%pmass * xx
      soil1(j)%hp(1)%p = soil1(j)%hp(1)%p + pburn
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - pburn
      soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd * xx
      rsd1(j)%tot(1)%n = rsd1(j)%tot(1)%n * xx
      soil1(jj)%hs(1)%n = soil1(jj)%hs(1)%n * xx
      soil1(j)%hp(1)%n = soil1(j)%hp(1)%n* xx

      !!insert new biomss by zhang	  
      !!=================================
      if (bsn_cc%cswat == 2) then
          rsd1(j)%meta%m = rsd1(j)%meta%m * xx
          rsd1(j)%str%m = rsd1(j)%str%m * xx
          rsd1(j)%str%c = rsd1(j)%str%c * xx
          rsd1(j)%str%n = rsd1(j)%str%n * xx
          rsd1(j)%meta%c = rsd1(j)%meta%c * xx
          rsd1(j)%meta%n = rsd1(j)%meta%n * xx
          rsd1(j)%lig%m = rsd1(j)%lig%m * xx  

          emitc_d(j) = emitc_d(j) + pcom(j)%plm(ipl)%mass * (1.-xx)
          emitc_d(j) = emitc_d(j) + soil(j)%ly(1)%rsd * (1.-xx)  
      end if 
      !!insert new biomss by zhang
      !!=================================

      return
      end subroutine pl_burnop