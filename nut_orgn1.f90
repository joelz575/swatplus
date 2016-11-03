      subroutine nut_orgn1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff
!!!!!!!!!!!!!!!!!!!!!!!!!
!!   ENRATIO (floater)
!!   SEDORGN (output)
!!   SEDYLD (output)
!!!!!!!!!!!!!!!!!!!!!!!!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of organic N in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    orgn1       |kg N/ha       |amount of organic N in first soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : hru, soil, sedorgn, sedyld
      use jrw_datalib_module
      use organic_mineral_mass_module

      integer :: j
      real :: orgn1, wt1, er, conc

      j = ihru

        !! HRU calculations
         orgn1 = soil1(j)%sta(1)%n + soil1(j)%act(1)%n + rsd(j)%rsd_tfl%n
         wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgn > .001) then
          er = hru(j)%hyd%erorgn
        else
          er = enratio
        end if

      conc = orgn1 * er / wt1

      !! HRU calculations
      sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha

	  !! update soil nitrogen pools only for HRU calculations
      if (orgn1 > 1.e-6) then
       soil1(j)%act(1)%n = soil1(j)%act(1)%n - sedorgn(j) *       &
                                      (soil1(j)%act(1)%n / orgn1)
       soil1(j)%sta(1)%n = soil1(j)%sta(1)%n - sedorgn(j) *         &   
                                      (soil1(j)%sta(1)%n / orgn1)
       rsd(j)%rsd_tfl%n = rsd(j)%rsd_tfl%n - sedorgn(j) *           &   
                                      (rsd(j)%rsd_tfl%n / orgn1)

       if (soil1(j)%sta(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%sta(1)%n
         soil1(j)%sta(1)%n = 0.
       end if

       if (soil1(j)%sta(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%sta(1)%n
         soil1(j)%sta(1)%n = 0.
       end if

       if (rsd(j)%rsd_tfl%n < 0.) then
         sedorgn(j) = sedorgn(j) + rsd(j)%rsd_tfl%n
         rsd(j)%rsd_tfl%n = 0.
       end if
      end if

      return
      end subroutine nut_orgn1