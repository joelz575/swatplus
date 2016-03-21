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
        xx = soil(j)%nut(1)%orgn+soil(j)%nut(1)%aorgn+soil(j)%nut(1)%fon
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
       soil(j)%nut(1)%aorgn = soil(j)%nut(1)%aorgn - sedorgn(j) *       &
                                      (soil(j)%nut(1)%aorgn / xx)
       soil(j)%nut(1)%orgn = soil(j)%nut(1)%orgn - sedorgn(j) *         &   
                                      (soil(j)%nut(1)%orgn / xx)
       soil(j)%nut(1)%fon = soil(j)%nut(1)%fon - sedorgn(j) *           &   
                                      (soil(j)%nut(1)%fon / xx)

       if (soil(j)%nut(1)%aorgn < 0.) then
         sedorgn(j) = sedorgn(j) + soil(j)%nut(1)%aorgn
         soil(j)%nut(1)%aorgn = 0.
       end if

       if (soil(j)%nut(1)%orgn < 0.) then
         sedorgn(j) = sedorgn(j) + soil(j)%nut(1)%orgn
         soil(j)%nut(1)%orgn = 0.
       end if

       if (soil(j)%nut(1)%fon < 0.) then
         sedorgn(j) = sedorgn(j) + soil(j)%nut(1)%fon
         soil(j)%nut(1)%fon = 0.
       end if
      end if

      return
      end subroutine nut_orgn