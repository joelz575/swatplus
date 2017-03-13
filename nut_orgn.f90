      subroutine nut_orgn(iwave)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha           |area of watershed in hectares
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    sub_bd(:)     |Mg/m^3       |bulk density in subbasin first soil layer
!!    sub_fr(:)     |none         |fraction of watershed area in subbasin
!!    sub_orgn(:)   |kg N/ha      |amount of nitrogen stored in all organic
!!                                |pools 
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
!!    xx          |kg N/ha       |amount of organic N in first soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use organic_mineral_mass_module

      integer, intent (in) :: iwave
      integer :: j
      real :: xx, wt1, er, conc

      j = 0
      j = ihru

      xx = 0.
	wt1 = 0.    !! conversion factor
      er = 0.		!! enrichment ratio
      if (iwave <= 0) then
        !! HRU calculations
        xx = soil1(j)%hp(1)%n + soil1(j)%hs(1)%n + rsd1(j)%tot(1)%n
        wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgn > .001) then
          er = hru(j)%hyd%erorgn
        else
          er = enratio
        end if

      else
        !! subbasin calculations
        xx = sub_orgn(iwave)
        wt1 = sub_bd(iwave) * soil(j)%phys(1)%d / 100.
        er = enratio
      end if

      conc = 0.
      conc = xx * er / wt1

      if (iwave <= 0) then
        !! HRU calculations
        sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha
      else
        !! subbasin calculations
        sedorgn(j) = .001 * conc * sedyld(j) / (da_ha * sub_fr(iwave))
      end if

	!! update soil nitrogen pools only for HRU calculations
      if (iwave <= 0 .and. xx > 1.e-6) then
       soil1(j)%hs(1)%n = soil1(j)%hs(1)%n - sedorgn(j) *       &
                                      (soil1(j)%hs(1)%n / xx)
       soil1(j)%hp(1)%n = soil1(j)%hp(1)%n - sedorgn(j) *       &   
                                      (soil1(j)%hp(1)%n / xx)
       rsd1(j)%tot(1)%n = rsd1(j)%tot(1)%n - sedorgn(j) *   &   
                                      (rsd1(j)%tot(1)%n / xx)

       if (soil1(j)%hs(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%hs(1)%n
         soil1(j)%hs(1)%n = 0.
       end if

       if (soil1(j)%hp(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + soil1(j)%hp(1)%n
         soil1(j)%hp(1)%n = 0.
       end if

       if (rsd1(j)%tot(1)%n < 0.) then
         sedorgn(j) = sedorgn(j) + rsd1(j)%tot(1)%n
         rsd1(j)%tot(1)%n = 0.
       end if
      end if

      return
      end subroutine nut_orgn