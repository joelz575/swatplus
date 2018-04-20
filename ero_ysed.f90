      subroutine ero_ysed
      
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_frcov   |              |fraction of cover by biomass - adjusted for
!!                                  canopy height
!!    grcov_fr    |              |fraction of cover by biomass as function of lai
!!    rsd_frcov   |              |fraction of cover by residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, soil, usle_cfac, cklsp, surfq, sedyld, sanyld, silyld, clayld, lagyld, sagyld,  &
         sno_hru, ihru, peakr
      
      implicit none

      integer :: j           !none                   |HRU number
      real :: c              !                       |
      real :: usle           !!metric tons/ha        | daily soil loss predicted with USLE equation
      real :: usle_ei        !100(ft-tn in)/(acre-hr)|USLE rainfall erosion index 

      j = ihru
      
      !! initialize variables
      cklsp(j) = usle_cfac(j) * hru(j)%lumv%usle_mult

      !! compute sediment yield with musle
      sedyld(j) = (surfq(j) * peakr * 1000. * hru(j)%km) ** .56 * cklsp(j)

      if (sedyld(j) < 0.) sedyld(j) = 0.

      !!adjust sediment yield for protection of snow cover
      if (sno_hru(j) > 0.) then
        if (sedyld(j) < 1.e-6) sedyld(j) = 0.0
      else if (sno_hru(j) > 100.) then
        sedyld(j) = 0.
      else
        sedyld(j) = sedyld(j) / Exp(sno_hru(j) * 3. / 25.4)
      end if

	!!Particle size distribution of sediment yield
	  sanyld(j) = sedyld(j) * soil(j)%det_san    !! Sand yield
	  silyld(j) = sedyld(j) * soil(j)%det_sil    !! Silt yield
	  clayld(j) = sedyld(j) * soil(j)%det_cla    !! Clay yield
	  sagyld(j) = sedyld(j) * soil(j)%det_sag    !! Small Aggregate yield
	  lagyld(j) = sedyld(j) * soil(j)%det_lag    !! Large Aggregate yield

      !! compute erosion with usle (written to output for comparison)
      usle = 1.292 * usle_ei * cklsp(j) / 11.8

      return
      end subroutine ero_ysed