      subroutine et_act
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential plant transpiration for Priestley-
!!    Taylor and Hargreaves ET methods, and potential and actual soil
!!    evaporation. NO3 movement into surface soil layer due to evaporation
!!    is also calculated.


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)   |mm H2O        |amount of water held in canopy storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU 
!!    esco(:)      |none          |soil evaporation compensation factor
!!    ihru         |none          |HRU number
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)   |kg/ha         |amount of residue on soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canev        |mm H2O        |amount of water evaporated from canopy
!!                                |storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    es_day       |mm H2O        |actual amount of evaporation (soil et) that
!!                                |occurs on day in HRU
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    sno3up       |kg N/ha       |amount of nitrate moving upward in the soil
!!                                |profile in watershed
!!    snoev        |mm H2O        |amount of water in snow lost through
!!                                |sublimation on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cej          |
!!    dep          |mm            |soil depth from which evaporation will occur
!!                                |in current soil layer
!!    eaj          |none          |weighting factor to adjust PET for impact of
!!                                |plant cover
!!    effnup       |
!!    eos1         |none          |variable to hold intermediate calculation
!!                                |result
!!    eosl         |mm H2O        |maximum amount of evaporation that can occur
!!                                |from soil profile
!!    es_max       |mm H2O        |maximum amount of evaporation (soil et)
!!                                |that can occur on current day in HRU
!!    esd          |mm            |maximum soil depth from which evaporation
!!                                |is allowed to occur
!!    esleft       |mm H2O        |potenial soil evap that is still available
!!    etco         |
!!    evz          |
!!    evzp         |
!!    ib           |none          |counter
!!    j            |none          |HRU number
!!    ly           |none          |counter
!!    no3up        |kg N/ha       |amount of nitrate moving upward in profile
!!    pet          |mm H2O        |amount of PET remaining after water stored
!!                                |in canopy is evaporated
!!    sev          |mm H2O        |amount of evaporation from soil layer
!!    sumsnoeb     |mm H2O        |amount of snow in elevation bands whose air
!!                                |temperature is greater than 0 degrees C
!!    xx           |none          |variable to hold intermediate calculation 
!!                                |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
 
      use basin_module
      use organic_mineral_mass_module
      use parm, only : hru, soil, tmpav, canstor, sno_hru, sol_cov, hru_dafr, ihru, canev, ep_max,  &
         es_day, pet_day, snoev, sumlai

      integer :: j, ib, ly
!!    real, parameter :: esd = 500., etco = 0.80, effnup = 0.1
      real :: esd, etco, effnup
      real :: no3up, es_max, eos1, xx, cej, eaj, pet, esleft
      real :: sumsnoeb, evzp, eosl, dep, evz, sev

      j = ihru
      pet = pet_day
!!    added statements for test of real statement above
	esd = 500.
	etco = 0.80
	effnup = 0.1

!! evaporate canopy storage first
!! canopy storage is calculated by the model only if the Green & Ampt
!! method is used to calculate surface runoff. The curve number methods
!! take canopy effects into account in the equations. For either of the
!! CN methods, canstor will always equal zero.
      pet = pet - canstor(j)
      if (pet < 0.) then
        canstor(j) = -pet
        canev = pet_day
        pet = 0.
        ep_max = 0.
        es_max = 0.
      else
        canev = canstor(j)
        canstor(j) = 0.
      endif

      if (pet > 1.0e-6) then

        !! compute potential plant evap for methods other that Penman-Monteith
        if (bsn_cc%pet /= 1) then
          if (sumlai <= 3.0) then
            ep_max = sumlai * pet / 3.
          else
            ep_max = pet
          end if
          if (ep_max < 0.) ep_max = 0.
        end if

        !! compute potential soil evaporation
        cej = -5.e-5
        eaj = 0.
        es_max = 0.
        eos1 = 0.
        if (sno_hru(j) >= 0.5) then
          eaj = 0.5
        else
          eaj = Exp(cej * (sol_cov(j)+ 0.1))
        end if
        es_max = pet * eaj
        eos1 = pet / (es_max + ep_max + 1.e-10)
        eos1 = es_max * eos1
        es_max = Min(es_max, eos1)
        es_max = Max(es_max, 0.)

        !! make sure maximum plant and soil ET doesn't exceed potential ET
        if (pet_day < es_max + ep_max) then
          es_max = pet_day - ep_max
          if (pet < es_max + ep_max) then
            es_max = pet * es_max / (es_max + ep_max)
            ep_max = pet * ep_max / (es_max + ep_max)
          end if
        end if

        !! initialize soil evaporation variables
        esleft = es_max

        !! compute sublimation
        if (tmpav(j) > 0.) then
          if (sno_hru(j) >= esleft) then
            !! take all soil evap from snow cover
            sno_hru(j) = sno_hru(j) - esleft
            snoev = snoev + esleft
            esleft = 0.
          else
            !! take all soil evap from snow cover then start taking from soil
            esleft = esleft - sno_hru(j)
            snoev = snoev + sno_hru(j)
            sno_hru(j) = 0.
          endif
        endif

!! take soil evap from each soil layer
      evzp = 0.
      eosl = esleft
      do ly = 1, soil(j)%nly

        !! depth exceeds max depth for soil evap (esd)
        dep = 0.
        if (ly == 1) then
          dep = soil(j)%phys(1)%d
        else
          dep = soil(j)%phys(ly-1)%d
        endif
        
        if (dep < esd) then
          !! calculate evaporation from soil layer
          evz = 0.
          sev = 0.
          xx = 0.
          evz = eosl * soil(j)%phys(ly)%d / (soil(j)%phys(ly)%d +        &
             Exp(2.374 - .00713 * soil(j)%phys(ly)%d))
          sev = evz - evzp * hru(j)%hyd%esco
          evzp = evz
          if (soil(j)%phys(ly)%st < soil(j)%phys(ly)%fc) then
            xx =  2.5 * (soil(j)%phys(ly)%st - soil(j)%phys(ly)%fc) /    &
             soil(j)%phys(ly)%fc
            sev = sev * expo(xx)
          end if
          sev = Min(sev, soil(j)%phys(ly)%st * etco)

          if (sev < 0.) sev = 0.
          if (sev > esleft) sev = esleft

          !! adjust soil storage, potential evap
          if (soil(j)%phys(ly)%st > sev) then
            esleft = esleft - sev
            soil(j)%phys(ly)%st = Max(1.e-6, soil(j)%phys(ly)%st - sev)
          else
            esleft = esleft - soil(j)%phys(ly)%st
            soil(j)%phys(ly)%st = 0.
          endif
        endif

        !! compute no3 flux from layer 2 to 1 by soil evaporation
        if (ly == 2) then
          no3up = 0.
          no3up =effnup * sev * soil1(j)%mn(2)%no3 / (soil(j)%phys(2)%st + 1.e-6)
          no3up = Min(no3up, soil1(j)%mn(2)%no3)
          sno3up = sno3up + no3up * hru_dafr(j)
          soil1(j)%mn(2)%no3 = soil1(j)%mn(2)%no3 - no3up
          rsd1(j)%mn%no3 = rsd1(j)%mn%no3 + no3up
        endif

      end do

      !! update total soil water content
      soil(j)%sw = 0.
      do ly = 1, soil(j)%nly
        soil(j)%sw = soil(j)%sw + soil(j)%phys(ly)%st
      end do

      !! calculate actual amount of evaporation from soil
      es_day = es_max - esleft
      if (es_day < 0.) es_day = 0.

      end if

      return
      end subroutine et_act