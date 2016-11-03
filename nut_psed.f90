      subroutine nut_psed(iwave)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic and mineral phosphorus
!!    attached to sediment in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha           |area of watershed in hectares
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgp(:)     |none         |organic P enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    hru_dafr(:)   |none         |fraction of watershed area in HRU
!!    ihru          |none         |HRU number
!!    inum1         |none         |subbasin number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    sub_fr(:)     |none         |fraction of watershed area in subbasin
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

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of P in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    porgg       |kg P/ha       |total amount of P in organic pools prior to
!!                               |sediment removal
!!    psedd       |kg P/ha       |total amount of P in mineral sediment pools
!!                               |prior to sediment removal
!!    sedp        |kg P/ha       |total amount of P removed in sediment erosion
!!    sb          |none          |subbasin number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    xx          |kg P/ha       |amount of phosphorus attached to sediment 
!!                               |in soil
!!    xxa         |kg P/ha       |fraction of active mineral phosphorus in soil
!!    xxo         |kg P/ha       |fraction of organic phosphorus in soil
!!    xxs         |kg P/ha       |fraction of stable mineral phosphorus in soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module

      integer, intent (in) :: iwave
      integer :: j, sb
      real :: xx, wt1, er, conc, xxo, sedp, psedd, porgg, xxa, xxs

      j = ihru

      hru(j)%rsd = plt_mass_z
      do ipl = 1, pcom(j)%npl
        hru(j)%rsd = hru(j)%rsd + hru(j)%rsd_flt(ipl)
      end do
      
        !! HRU sediment calculations
        xx =soil(j)%nut(1)%orgp + soil(j)%nut(1)%fop + soil(j)%nut(1)%mp   &
              + hru(j)%rsd%pmass+soil(j)%nut(1)%actp+soil(j)%nut(1)%stap
        if (xx > 1.e-3) then
          xxo=(soil(j)%nut(1)%orgp+soil(j)%nut(1)%fop+soil(j)%nut(1)%mp)   &
                                                                 /xx
           xxa = soil(j)%nut(1)%actp / xx
           xxs = soil(j)%nut(1)%stap / xx
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

        psedd = soil(j)%nut(1)%actp + soil(j)%nut(1)%stap
        porgg = soil(j)%nut(1)%orgp+soil(j)%nut(1)%fop+hru(j)%rsd%pmass
        if (porgg > 1.e-3) then
        hru(j)%rsd%pmass = hru(j)%rsd%pmass - sedorgp(j) *                &
               (hru(j)%rsd%pmass / porgg)
        soil(j)%nut(1)%orgp = soil(j)%nut(1)%orgp - sedorgp(j) *          &
               (soil(j)%nut(1)%orgp / porgg)
        soil(j)%nut(1)%fop = soil(j)%nut(1)%fop - sedorgp(j) *            &
               (soil(j)%nut(1)%fop / porgg)
        soil(j)%nut(1)%mp = soil(j)%nut(1)%mp - sedorgp(j) *              &
            (soil(j)%nut(1)%mp / porgg)
        end if
        soil(j)%nut(1)%actp = soil(j)%nut(1)%actp - sedminpa(j)
        soil(j)%nut(1)%stap = soil(j)%nut(1)%stap - sedminps(j)

        !! Not sure how can this happen but I reapeated 
        !! the check for sol_mp(1,j) - Armen March 2009
        if (soil(j)%nut(1)%orgp < 0.) then
          sedorgp(j) = sedorgp(j) + soil(j)%nut(1)%orgp
          soil(j)%nut(1)%orgp = 0.
        end if

        if (soil(j)%nut(1)%fop < 0.) then
          sedorgp(j) = sedorgp(j) + soil(j)%nut(1)%fop
          soil(j)%nut(1)%fop = 0.
        end if

        if (soil(j)%nut(1)%mp < 0.) then
          sedorgp(j) = sedorgp(j) + soil(j)%nut(1)%mp
          soil(j)%nut(1)%mp = 0.
        end if

        if (soil(j)%nut(1)%actp < 0.) then
          sedminpa(j) = sedminpa(j) + soil(j)%nut(1)%actp
          soil(j)%nut(1)%actp = 0.
        end if

        if (soil(j)%nut(1)%stap < 0.) then
          sedminps(j) = sedminps(j) + soil(j)%nut(1)%stap
          soil(j)%nut(1)%stap = 0.
        end if

      return
      end subroutine nut_psed