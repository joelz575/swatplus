      subroutine res_nutrient (jres, inut)

      use reservoir_data_module
      use time_module
      use reservoir_module
      use hydrograph_module, only : res
      
      implicit none      
      
      !integer :: iseas
      real :: nitrok             !              |
      real :: phosk              !              |
      real :: tpco               !              |
      real :: chlaco             !              |
      integer :: jres            !none          |reservoir number
      integer :: inut            !none          |counter
      integer :: nsetlr          !              |
      real :: psetlr             !              |
      real :: conc_n             !              |
      real :: conc_p             !              |
      real :: pstlr              !              | 
      real :: theta              !              |
      

      !! if reservoir volume less than 1 m^3, set all nutrient levels to
      !! zero and perform no nutrient calculations
      if (res(jres)%flo < 1.) then
        res(jres)%orgn = 0.
        res(jres)%sedp = 0.
        res(jres)%no3 = 0.
        res(jres)%nh3 = 0.
        res(jres)%no2 = 0.
        res(jres)%solp = 0.
        res(jres)%chla = 0.
        res_om_d(jres)%seci = 0.
      end if
      if (res(jres)%flo < 1.) return

      !! if reservoir volume greater than 1 m^3, perform nutrient calculations
      if (time%mo >= res_nut(inut)%ires1 .and. time%mo <= res_nut(inut)%ires2) then
        nsetlr = res_nut(inut)%nsetlr1
        psetlr = res_nut(inut)%psetlr1
      else
        nsetlr = res_nut(inut)%nsetlr2
        psetlr = res_nut(inut)%psetlr2
      endif

      !! n and p concentrations
      conc_n = (res(jres)%orgn + res(jres)%no3 + res(jres)%nh3 + res(jres)%no2) / res(jres)%flo
      conc_p = (res(jres)%sedp + res(jres)%solp) / res(jres)%flo
      
      !! new inputs thetn, thetap, conc_pmin, conc_nmin
      !! new equations from Charles Ikenberry for wetlands
      nitrok = 10000. * res_om_d(jres)%area_ha * (conc_n - res_nut(inut)%conc_nmin) * Theta(pstlr, res_nut(inut)%theta_n, tair_av)
      nitrok = Min(nitrok, 1.)
      phosk = 10000. * res_om_d(jres)%area_ha * (conc_p - res_nut(inut)%conc_pmin) * Theta(pstlr, res_nut(inut)%theta_p, tair_av)
      phosk = Min(phosk, 1.)

      !! remove nutrients from reservoir by settling
      !! other part of equation 29.1.3 in SWAT manual
      res(jres)%solp = res(jres)%solp * (1. - phosk)
      res(jres)%sedp = res(jres)%sedp * (1. - phosk)
      res(jres)%orgn = res(jres)%orgn * (1. - nitrok)
      res(jres)%no3 = res(jres)%no3 * (1. - nitrok)
      res(jres)%nh3 = res(jres)%nh3 * (1. - nitrok)
      res(jres)%no2 = res(jres)%no2 * (1. - nitrok)

      !! calculate chlorophyll-a and water clarity
      chlaco = 0.
      res(jres)%chla = 0.
      res_om_d(jres)%seci = 0.
      tpco = 1.e+6 * (res(jres)%solp + res(jres)%sedp) / (res(jres)%flo + resflwo)
      if (tpco > 1.e-4) then
        !! equation 29.1.6 in SWAT manual
        chlaco = res_nut(inut)%chlar * 0.551 * (tpco**0.76)
        res(jres)%chla = chlaco * (res(jres)%flo + resflwo) * 1.e-6
      endif
      if (chlaco > 1.e-4) then
        !! equation 29.1.8 in SWAT manual
        res_om_d(jres)%seci = res_nut(inut)%seccir * 6.35 * (chlaco ** (-0.473))
      endif

      !! calculate amount of nutrients leaving reservoir
      if (res(jres)%no3 < 1.e-4) res(jres)%no3 = 0.0
      if (res(jres)%orgn < 1.e-4) res(jres)%orgn = 0.0
      if (res(jres)%sedp < 1.e-4) res(jres)%sedp = 0.0
      if (res(jres)%solp < 1.e-4) res(jres)%solp = 0.0
      if (res(jres)%chla < 1.e-4) res(jres)%chla = 0.0
      if (res(jres)%nh3 < 1.e-4) res(jres)%nh3 = 0.0
      if (res(jres)%no2 < 1.e-4) res(jres)%no2 = 0.0
      resno3o = res(jres)%no3 * resflwo / (res(jres)%flo + resflwo)
      resorgno = res(jres)%orgn * resflwo / (res(jres)%flo + resflwo)
      resorgpo = res(jres)%sedp * resflwo / (res(jres)%flo + resflwo)
      ressolpo = res(jres)%solp * resflwo / (res(jres)%flo + resflwo)
      reschlao = res(jres)%chla * resflwo / (res(jres)%flo + resflwo)
      resnh3o = res(jres)%nh3 * resflwo / (res(jres)%flo + resflwo)
      resno2o = res(jres)%no2 * resflwo / (res(jres)%flo + resflwo)
      res(jres)%orgn = res(jres)%orgn - resorgno
      res(jres)%sedp = res(jres)%sedp - resorgpo
      res(jres)%no3 = res(jres)%no3 - resno3o
      res(jres)%nh3 = res(jres)%nh3 - resnh3o
      res(jres)%no2 = res(jres)%no2 - resno2o
      res(jres)%solp = res(jres)%solp - ressolpo
      res(jres)%chla = res(jres)%chla - reschlao

      return
      end subroutine res_nutrient