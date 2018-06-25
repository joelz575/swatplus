      subroutine ero_cfactor
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cvm(:)      |none          |natural log of USLE_C (the minimum value
!!                               |of the USLE C factor for the land cover)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    peakr       |m^3/s         |peak runoff rate
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : usle_cfac, sol_cov, cvm_com, ihru 
      use plant_module
      
      implicit none

      integer :: j       !none          |HRU number
      real :: c          !              |
      real :: rsd_frcov  !              |fraction of cover by residue
      real :: grcov_fr   !              |fraction of cover by biomass as function of lai 
      real :: bio_frcov  !              |fraction of cover by biomass - adjusted for
                         !              |canopy height
      integer :: max     !              |   ??? should this be real??
    
      j = ihru

      !! HRU sediment calculations
      if (bsn_cc%cfac == 0) then
        if (pcom(j)%npl > 0) then     
          c = Exp((-.2231 - cvm_com(j)) *                                &
       	      Exp(-.00115 * sol_cov(j)) + cvm_com(j))
        else
          if (sol_cov(j) > 1.e-4) then
            c = Exp(-.2231 * Exp(-.00115 * sol_cov(j)))               
          else
            c = .8
          end if
	    end if
      else
        rsd_frcov = Exp(-bsn_prm%rsd_covco * sol_cov(j))
        grcov_fr = pcom(j)%lai_sum / (pcom(j)%lai_sum + Exp(1.748 - 1.748 * pcom(j)%lai_sum))
        bio_frcov = 1. - grcov_fr * Exp(-.01 * pcom(j)%cht_mx)
        c = Max(1.e-10, rsd_frcov * bio_frcov)
      end if

      usle_cfac(ihru) = c
      
      return
      end subroutine ero_cfactor