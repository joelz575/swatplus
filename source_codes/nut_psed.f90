      subroutine nut_psed

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic and mineral phosphorus
!!    attached to sediment in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgp(:)     |none         |organic P enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:)  |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)  |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedorgp(:)   |kg P/ha       |amount of organic phosphorus in surface
!!                                |runoff in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

        use hru_module, only : hru, sedyld, sedorgp, sedminpa, sedminps, ihru, enratio,  &
          ihru, ipl 
        use soil_module
        use plant_module
        use organic_mineral_mass_module
      
        implicit none       

        integer :: j              !none          |HRU number
        integer :: sb             !none          |subbasin number
        real :: xx                !kg P/ha       |amount of phosphorus attached to sediment 
                                  !              |in soil
        real :: wt1               !none          |conversion factor (mg/kg => kg/ha)
        real :: er                !none          |enrichment ratio
        real :: conc              !              |concentration of organic N in soil
        real :: xxo               !kg P/ha       |fraction of organic phosphorus in soil
        real :: sedp              !kg P/ha       |total amount of P removed in sediment erosion 
        real :: psedd             !kg P/ha       |total amount of P in mineral sediment pools
                                  !              |prior to sediment removal
        real :: porgg             !kg P/ha       |total amount of P in organic pools prior to
                                  !              |sediment removal 
        real :: xxa               !kg P/ha       |fraction of active mineral phosphorus in soil
        real :: xxs               !kg P/ha       |fraction of stable mineral phosphorus in soil

        j = ihru

        !! HRU sediment calculations
        xx = soil1(j)%hp(1)%p
        if (xx > 1.e-3) then
          xxo = (soil1(j)%hp(1)%p + rsd1(j)%man%p) / xx
        end if

        wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgp > .001) then
         er = hru(j)%hyd%erorgp
        else
         er = enratio
        end if
      
        conc = xx * er / wt1

        sedp = .001 * conc * sedyld(j) / hru(j)%area_ha
        sedorgp(j) = sedp * xxo
        sedminpa(j) = sedp * xxa
        sedminps(j) = sedp * xxs

        psedd = rsd1(j)%mp%act + rsd1(j)%mp%sta
        porgg = soil1(j)%hp(1)%p
        if (porgg > 1.e-3) then
          soil1(j)%hp(1)%p = soil1(j)%hp(1)%p - sedorgp(j) * (soil1(j)%hp(1)%p / porgg)
        end if

        !! Not sure how can this happen but I repeated 
        !! the check for sol_mp(1,j) - Armen March 2009
        if (soil1(j)%hp(1)%p < 0.) then
          sedorgp(j) = sedorgp(j) + soil1(j)%hp(1)%p
          soil1(j)%hp(1)%p = 0.
        end if

      return
      end subroutine nut_psed