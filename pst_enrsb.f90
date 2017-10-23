      subroutine pst_enrsb

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha       |ha            |area of watershed in hectares
!!    ihru        |none          |HRU number
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio     |none          |enrichment ratio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cy          |
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : sedyld, sanyld, silyld, clayld, sagyld, lagyld, ihru, enratio
      
      integer :: j
      real :: cy
 
      j = 0
      j = ihru

        if (sedyld(j) < 1.e-4) then
	      sedyld(j) = 0.0
	      sanyld(j) = 0.0
	      silyld(j) = 0.0
	      clayld(j) = 0.0
	      sagyld(j) = 0.0
	      lagyld(j) = 0.0
	    endif

!! CREAMS method for calculating enrichment ratio
      cy = 0.

      if (cy > 1.e-6) then
        enratio = .78 * cy ** (-.2468)
      else
        enratio = 0.
      endif

      if (enratio > 3.5) enratio = 3.5

      return
      end subroutine pst_enrsb