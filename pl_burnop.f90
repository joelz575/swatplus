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
   
      integer :: j
      real :: aphu

      j = 0
      j = ihru

      xx = burn_frlb
      pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass * xx
      pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass * xx
      pburn = pcom(ihru)%plm(ipl)%pmass * xx
      soil(j)%nut(1)%orgp = soil(j)%nut(1)%orgp + pburn
      pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - pburn
      soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd * xx
      soil(j)%nut(1)%fon = soil(j)%nut(1)%fon * xx
      soil(jj)%nut(1)%aorgn = soil(jj)%nut(1)%aorgn * xx
      soil(j)%nut(1)%orgn = soil(j)%nut(1)%orgn* xx

      !!insert new biomss by zhang	  
      !!=================================
      if (bsn_cc%cswat == 2) then
          soil(j)%cbn(1)%lm = soil(j)%cbn(1)%lm * xx
          soil(j)%cbn(1)%ls = soil(j)%cbn(1)%ls * xx
          soil(j)%cbn(1)%lsc = soil(j)%cbn(1)%lsc * xx
          soil(j)%cbn(1)%lsn = soil(j)%cbn(1)%lsn * xx
          soil(j)%cbn(1)%lmc = soil(j)%cbn(1)%lmc * xx
          soil(j)%cbn(1)%lmn = soil(j)%cbn(1)%lmn * xx
          soil(j)%cbn(1)%lsl = soil(j)%cbn(1)%lsl * xx  

          emitc_d(j) = emitc_d(j) + pcom(j)%plm(ipl)%mass * (1.-xx)
          emitc_d(j) = emitc_d(j) + soil(j)%ly(1)%rsd * (1.-xx)  
      end if 
      !!insert new biomss by zhang
      !!=================================

      return
      end subroutine pl_burnop