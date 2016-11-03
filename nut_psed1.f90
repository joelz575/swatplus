      subroutine nut_psed1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic and mineral phosphorus
!!    attached to sediment in surface runoff
!!!!!!!!!!!!!!!!!!!!!!!!
!! PLT_MASS_Z
!! NPL(J)
!! ENRATIO/ER
!! SEDYLD
!! SEDMINPA
!! SEDMINPS
!! SEDORGP
!! CONC
!! PORGG
!! PSEDD
!! SEDP
!! XX renamed to SEDSOILP
!! XXA renamed to PAS
!! XXO renamed to POS
!! XXS renamed to PSS
!!!!!!!!!!!!!!!!!!!!!!!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    ihru          |none         |HRU number
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
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
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    sedsoilp    |kg P/ha       |amount of phosphorus attached to sediment 
!!                               |in soil
!!    pas         |kg P/ha       |fraction of active mineral phosphorus in soil
!!    pos         |kg P/ha       |fraction of organic phosphorus in soil
!!    pss         |kg P/ha       |fraction of stable mineral phosphorus in soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : hru, soil, pcom, sedorgp, sedminpa, sedminps, sedyld
      use jrw_datalib_module
      use organic_mineral_mass_module

      integer :: j
      real :: sedsoilp, wt1, er, conc, pos, sedp, psedd, porgg, pas, pss
      

      j = ihru

      rsd(j)%rsd_tfl = om_mass_z
      
      do ipl = 1, pcom(j)%npl
        rsd(j)%rsd_tfl = rsd(j)%rsd_tfl + rsd(j)%rsd_fl(ipl)
        rsd(j)%rsd_ts(1) = rsd(j)%rsd_ts(1) + rsd(j)%rsd_s(1,ipl)
      end do
      
        !! HRU sediment calculations
        sedsoilp = soil1(j)%sta(1)%p + rsd(j)%rsd_tfl%p + rsd(j)%rsd_ts(1)%p   &
              + rsd(j)%rsd_tfl%p + soil1(j)%act(1)%p + soil1(j)%sta(1)%p
        if (sedsoilp > 1.e-3) then
           pos = (soil1(j)%sta(1)%p + rsd(j)%rsd_tfl%p + rsd(j)%rsd_ts(1)%p)   &
                                                                 /sedsoilp
           pas = soil1(j)%act(1)%p / sedsoilp
           pss = soil1(j)%sta(1)%p / sedsoilp
        end if

        wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgp > .001) then
         er = hru(j)%hyd%erorgp
        else
         er = enratio
        end if
      
        conc = sedsoilp * er / wt1

        sedp = .001 * conc * sedyld(j) / hru(j)%area_ha
        sedorgp(j) = sedp * pos
        sedminpa(j) = sedp * pas
        sedminps(j) = sedp * pss

        psedd = soil1(j)%act(1)%p + soil1(j)%sta(1)%p
        porgg = soil1(j)%sta(1)%p + rsd(j)%rsd_tfl%p + rsd(j)%rsd_tfl%p
        if (porgg > 1.e-3) then
        rsd(j)%rsd_tfl%p = rsd(j)%rsd_tfl%p - sedorgp(j) *                &
               (rsd(j)%rsd_tfl%p / porgg)
        soil1(j)%sta(1)%p = soil1(j)%sta(1)%p - sedorgp(j) *          &
               (soil1(j)%sta(1)%p / porgg)
        rsd(j)%rsd_tfl%p = rsd(j)%rsd_tfl%p - sedorgp(j) *            &
               (rsd(j)%rsd_tfl%p / porgg)
        rsd(j)%rsd_ts(1)%p = rsd(j)%rsd_ts(1)%p - sedorgp(j) *              &
            (rsd(j)%rsd_ts(1)%p / porgg)
        end if
        soil1(j)%act(1)%p = soil1(j)%act(1)%p - sedminpa(j)
        soil1(j)%sta(1)%p = soil1(j)%sta(1)%p - sedminps(j)

        !! Not sure how can this happen but I reapeated 
        !! the check for sol_mp(1,j) - Armen March 2009
        if (soil1(j)%sta(1)%p < 0.) then
          sedorgp(j) = sedorgp(j) + soil1(j)%sta(1)%p
          soil1(j)%sta(1)%p = 0.
        end if

        if (rsd(j)%rsd_tfl%p < 0.) then
          sedorgp(j) = sedorgp(j) + rsd(j)%rsd_tfl%p
          rsd(j)%rsd_tfl%p = 0.
        end if

        if (rsd(j)%rsd_ts(1)%p < 0.) then
          sedorgp(j) = sedorgp(j) + rsd(j)%rsd_ts(1)%p
          rsd(j)%rsd_ts(1)%p = 0.
        end if

        if (soil1(j)%act(1)%p < 0.) then
          sedminpa(j) = sedminpa(j) + soil1(j)%act(1)%p
          soil1(j)%act(1)%p = 0.
        end if

        if (soil1(j)%sta(1)%p < 0.) then
          sedminps(j) = sedminps(j) + soil1(j)%sta(1)%p
          soil1(j)%sta(1)%p = 0.
        end if

      return
      end subroutine nut_psed1