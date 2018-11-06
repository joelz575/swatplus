      subroutine wet_initial
      
      use reservoir_module
      use reservoir_data_module
      use reservoir_data_module
      use hydrograph_module
      use hru_module, only : hru, ihru
      use maximum_data_module
      
      implicit none
      
      integer :: iprop          !              | 
      integer :: ihyd           !none          |counter 
      integer :: init           !              |
      real :: cnv               !none          |conversion factor (mm => m^3)
      real :: x1                !              |
      integer :: ires           !none          |counter 
      real :: res_h             !              |
      real :: res_h1            !              |
      real :: wet_h             !              |
      real :: wet_h1            !              | 
  
      do ihru = 1, sp_ob%hru
        !! set initial volumes and convert units
        iprop = hru(ihru)%dbs%surf_stor
        if (iprop > 0) then
          ihyd = wet_dat(iprop)%hyd
          !! ha*mm*10. => m**3
            wet_ob(iprop)%evol = wet_ob(iprop)%evol + wet_hyd(ihyd)%esa * hru(ihru)%area_ha * wet_hyd(ihyd)%edep * 10.
            wet_ob(iprop)%pvol = wet_ob(iprop)%pvol + wet_hyd(ihyd)%psa * hru(ihru)%area_ha * wet_hyd(ihyd)%pdep * 10.
            wet_ob(iprop)%psa = wet_ob(iprop)%psa + wet_hyd(ihyd)%psa * hru(ihru)%area_ha 
            wet_ob(iprop)%esa = wet_ob(iprop)%esa + wet_hyd(ihyd)%psa * hru(ihru)%area_ha 
        end if
      end do
      
      do iprop = 1, db_mx%wet_dat
        !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
        !!                                        ppm = t/m^3 * 10^6
        ihyd = wet_dat(iprop)%hyd
        init = wet_dat(iprop)%init

        cnv = wet(iprop)%flo / 1000.
        wet(ires) = cnv * om_init_water(init)
        wet(ires)%flo = om_init_water(init)%flo * res_ob(ires)%pvol

        !! wetland on hru - solve quadratic to find new depth
        !testing relationship wet_vol(jres) = float(jj) * .1 * wet_pvol(jres)
        x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ires)%flo / wet_hyd(ihyd)%pdep)
        if (x1 < 1.e-6) then
          res_h = 0.
        else
          res_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
          wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
        end if
        res_om_d(iprop)%area_ha = wet_ob(ires)%psa * (1. + wet_hyd(ihyd)%acoef * wet_h)

      end do
      close(105)

      return
      end subroutine wet_initial