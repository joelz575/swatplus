      subroutine et_pot
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential evapotranspiration using one
!!    of three methods. If Penman-Monteith is being used, potential plant
!!    transpiration is also calculated.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday     |none           |albedo for the day in HRU
!!    cht(:)     |m              |canopy height
!!    gsi(:)     |m/s            |maximum stomatal conductance
!!    hru_ra(:)  |MJ/m^2         |solar radiation for the day in HRU
!!    hru_rmx(:) |MJ/m^2         |maximum possible radiation for the day in HRU
!!    hru_sub(:) |none           |subbasin in which HRU is located
!!    icr(:)     |none           |sequence number of crop grown within the
!!                               |current year
!!    ihru       |none           |HRU number
!!    petmeas    |mm H2O         |potential ET value read in for day
!!    rhd(:)     |none           |relative humidity for the day in HRU
!!    sno_hru(:) |mm H2O         |amount of water in snow in HRU on current day
!!    u10(:)     |m/s            |wind speed (measured at 10 meters above 
!!                               |surface)
!!    vpd2(:)    |(m/s)*(1/kPa)  |rate of decline in stomatal conductance per
!!               |               |unit increase in vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et) 
!!                               |that can occur on current day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration on current day in
!!                               |HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chz         |cm            |vegetation height
!!    d           |cm            |displacement height for plant type
!!    dlt         |kPa/deg C     |slope of the saturation vapor pressure-
!!                               |temperature curve
!!    ea          |kPa           |saturated vapor pressure
!!    ed          |kPa           |actual vapor pressure
!!    fvpd        |kPa           |amount of vapro pressure deficit over 
!!                               |threshold value
!!    gma         |kPa/deg C     |psychrometric constant
!!    j           |none          |HRU number
!!    pb          |kPa           |mean atmospheric pressure
!!    pet_alpha  |none           |alpha factor in Priestley-Taylor PET equation
!!    ralb        |MJ/m2         |net incoming radiation for PET
!!    ralb1       |MJ/m2         |net incoming radiation
!!    ramm        |MJ/m2         |extraterrestrial radiation
!!    rbo         |none          |net emissivity
!!    rc          |s/m           |canopy resistance
!!    rho         |MJ/(m3*kPa)   |K1*0.622*xl*rho/pb
!!    rn          |MJ/m2         |net radiation
!!    rn_pet      |MJ/m2         |net radiation for continuous crop cover
!!    rout        |MJ/m2         |outgoing radiation
!!    rto         |none          |cloud cover factor
!!    rv          |s/m           |aerodynamic resistance to sensible heat and
!!                               |vapor transfer
!!    tk          |deg K         |average air temperature on current day for HRU
!!    uzz         |m/s           |wind speed at height zz
!!    xl          |MJ/kg         |latent heat of vaporization
!!    xx          |kPa           |difference between vpd and vpthreshold
!!    zom         |cm            |roughness length for momentum transfer
!!    zov         |cm            |roughness length for vapor transfer
!!    zz          |cm            |height at which wind speed is determined
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Sqrt, Max, Min
!!    SWAT: Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module, only : pldb, plcp
      use basin_module
      use parm, only : hru, pcom, u10, ihru, tmpav, rhd, sno_hru, hru_ra, hru_rmx, cht_mx, tmx, tmn,  &
        albday, epmax, idp, ipl, pet_day, petmeas, sumlai, vpd, ep_max
      
      integer :: j
      real :: tk, pb, gma, xl, ea, ed, dlt, ramm, ralb1, ralb, xx
      real :: rbo, rto, rn, uzz, zz, zom, zov, rv, rn_pet, fvpd
      real :: rc, rho, rout, d, chz, gsi_adj, pet_alpha

      !! initialize local variables
      j = ihru

      tk = tmpav(j) + 273.15

      !! calculate mean barometric pressure
      pb = 101.3 - hru(j)%topo%elev *                                   &                
                        (0.01152 - 0.544e-6 * hru(j)%topo%elev)

      !! calculate latent heat of vaporization
      xl = 2.501 - 2.361e-3 * tmpav(j)

      !! calculate psychrometric constant
      gma = 1.013e-3 * pb / (0.622 * xl)

      !! calculate saturation vapor pressure, actual vapor pressure and
      !! vapor pressure deficit
      ea = Ee(tmpav(j))
      ed = ea * rhd(j)
      vpd = ea - ed

      !!calculate the slope of the saturation vapor pressure curve
      dlt = 4098. * ea / (tmpav(j) + 237.3)**2
	
!! DETERMINE POTENTIAL ET

      select case (bsn_cc%pet)

       case (0)   !! PRIESTLEY-TAYLOR POTENTIAL EVAPOTRANSPIRATION METHOD
     
       !! net radiation
         !! calculate net short-wave radiation for PET
          if (sno_hru(j) <= .5) then
            ralb = hru_ra(j) * (1.0 - 0.23)
          else
            ralb = hru_ra(j) * (1.0 - 0.8)
          end if

        !! calculate net long-wave radiation

          !! net emissivity  equation 2.2.20 in SWAT manual
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
            if (hru_rmx(j) < 1.e-4) then
		    rto = 0.
            else
              rto = 0.9 * (hru_ra(j) / hru_rmx(j)) + 0.1
            end if

          !! net long-wave radiation equation 2.2.21
          rout = rbo * rto * 4.9e-9 * (tk**4)

          !! calculate net radiation
          rn_pet = ralb + rout
          
          !! net radiation
          pet_alpha = 1.28
          pet_day = pet_alpha * (dlt / (dlt + gma)) * rn_pet / xl
          pet_day = Max(0., pet_day)


       case (1)   !! PENMAN-MONTEITH POTENTIAL EVAPOTRANSPIRATION METHOD

       !! net radiation
         !! calculate net short-wave radiation for PET
          if (sno_hru(j) <= .5) then
            ralb = hru_ra(j) * (1.0 - 0.23) 
          else
            ralb = hru_ra(j) * (1.0 - 0.8) 
          end if
         !! calculate net short-wave radiation for max plant ET
          ralb1 = hru_ra(j) * (1.0 - albday) 

         !! calculate net long-wave radiation
          !! net emissivity  equation 2.2.20 in SWAT manual
          rbo = -(0.34 - 0.139 * Sqrt(ed))

          !! cloud cover factor equation 2.2.19
          if (hru_rmx(j) < 1.e-4) then
            rto = 0.
          else
            rto = 0.9 * (hru_ra(j) / hru_rmx(j)) + 0.1
          end if

          !! net long-wave radiation equation 2.2.21
          rout = rbo * rto * 4.9e-9 * (tk**4)

          !! calculate net radiation
          rn = ralb1 + rout
          rn_pet = ralb + rout
          !! net radiation

          rho = 1710. - 6.85 * tmpav(j)

          if (u10(j) < 0.01) u10(j) = 0.01

           !! potential ET: reference crop alfalfa at 40 cm height
           rv = 114. / (u10(j) * (170./1000.)**0.2)
           rc = 49. / (1.4 - 0.4 * hru(j)%parms%co2 / 330.)
           pet_day = (dlt * rn_pet + gma * rho * vpd / rv) /            &           
                                  (xl * (dlt + gma * (1. + rc / rv)))

           pet_day = Max(0., pet_day)
 
        !! maximum plant ET
          igrocom = 0
          do ipl = 1, pcom(j)%npl
            if (pcom(j)%plcur(ipl)%gro == 1) igrocom = 1
          end do
          if (igrocom <= 0) then
            ep_max = 0.0
          else
            !! determine wind speed and height of wind speed measurement
            !! adjust to 100 cm (1 m) above canopy if necessary
            if (cht_mx(j) <= 1.0) then
              zz = 170.0
            else
              zz = cht_mx(j) * 100. + 100.
            end if
            uzz = u10(j) * (zz/1000.)**0.2

            !! calculate canopy height in cm
            if (cht_mx(j) < 0.01) then
              chz = 1.
            else
              chz = cht_mx(j) * 100.
            end if

            !! calculate roughness length for momentum transfer
            if (chz <= 200.) then
              zom = 0.123 * chz
            else
              zom = 0.058 * chz**1.19
            end if
 
            !! calculate roughness length for vapor transfer
            zov = 0.1 * zom

            !! calculate zero-plane displacement of wind profile
            d = 0.667 * chz

            !! calculate aerodynamic resistance
            rv = Log((zz - d) / zom) * Log((zz - d) / zov)
            rv = rv / ((0.41)**2 * uzz)

            !! adjust stomatal conductivity for low vapor pressure
            !! this adjustment will lower maximum plant ET for plants
            !! sensitive to very low vapor pressure
            gsi_wav = 0.
            do ipl = 1, pcom(j)%npl
              idp = pcom(j)%plcur(ipl)%idplt
              rto = pcom(j)%plg(ipl)%lai / (sumlai + 0.01)
              xx = vpd - 1.
              if (xx > 0.0) then
                fvpd = Max(0.1,1.0 - plcp(idp)%vpd2 * xx)
              else
                fvpd = 1.0
              end if
              gsi_adj = pldb(idp)%gsi * fvpd
              gsi_wav = gsi_wav + gsi_adj * rto
            end do
          
            !! calculate canopy resistance
            rc = 1. / (gsi_adj + 0.01)           !single leaf resistance
            rc = rc / (0.5 * (sumlai + 0.01)                              &
                              * (1.4 - 0.4 * hru(j)%parms%co2 / 330.))

            !! calculate maximum plant ET
            ep_max = (dlt * rn + gma * rho * vpd / rv) /                  &
                              (xl * (dlt + gma * (1. + rc / rv)))
            if (ep_max < 0.) ep_max = 0.
            ep_max = Min(ep_max, pet_day)
          end if
       
       case (2)   !! HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD

        !! extraterrestrial radiation
        !! 37.59 is coefficient in equation 2.2.6 !!extraterrestrial
        !! 30.00 is coefficient in equation 2.2.7 !!max at surface
        ramm = hru_rmx(j) * 37.59 / 30. 

        if (tmx(j) > tmn(j)) then
         pet_day = hru(j)%hyd%harg_pet * (ramm / xl) * (tmpav(j) +      &
                                  17.8) * (tmx(j) - tmn(j))**0.5
         pet_day = Max(0., pet_day)
        else
          pet_day = 0.
        endif
      
       case (3)  !! READ IN PET VALUES
        pet_day = petmeas
  
      end select

      return
      end subroutine et_pot