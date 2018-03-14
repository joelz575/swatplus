      subroutine wet_initial
      
      use reservoir_module
      use reservoir_data_module
      use reservoir_data_module
      use hydrograph_module
      use hru_module, only : hru, ihru
  
      do ihru = 1, sp_ob%hru
        !! set initial volumes and convert units
        iprop = hru(ihru)%dbs%surf_stor
        ihyd = wet_dat(iprop)%hyd
        !! ha*mm*10. => m**3
        wet_ob(ihru)%evol = wet_hyd(ihyd)%esa * hru(ihru)%area_ha * wet_hyd(ihyd)%evol * 10.
        wet_hyd(ihru)%pvol = wet_hyd(ihyd)%psa * hru(ihru)%area_ha * wet_hyd(ihyd)%pvol * 10.
      end do
      
      do ihru = 1, sp_ob%hru
        !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
        !!                                        ppm = t/m^3 * 10^6
        iprop = hru(ihru)%dbs%surf_stor
        ihyd = wet_dat(iprop)%hyd
        init = wet_dat(iprop)%init
        
        cnv = wet(ihru)%flo / 1000.
        wet(ihru)%flo = res_init(init)%vol * wet_ob(ihru)%pvol
        wet(ihru)%sed = res_init(init)%sed * cnv
        wet(ihru)%orgn= res_init(init)%orgn * cnv
        wet(ihru)%no3 = res_init(init)%no3 * cnv
        wet(ihru)%no2 = res_init(init)%no2 * cnv
        wet(ihru)%nh3 = res_init(init)%nh3 * cnv
        wet(ihru)%sedp = res_init(init)%orgp * cnv
        wet(ihru)%solp = res_init(init)%solp * cnv
        wet(ihru)%san = res_init(init)%san * cnv
        wet(ihru)%sil = res_init(init)%sil * cnv
        wet(ihru)%cla = res_init(init)%cla * cnv
        wet(ihru)%sag = res_init(init)%sag * cnv
        wet(ihru)%lag = res_init(init)%lag * cnv
        wet(ihru)%grv = res_init(init)%gra * wet(ihru)%flo / 1000.
        wet(ihru)%chla = res_init(init)%chla * cnv
        wet(ihru)%psor = res_init(init)%psor * cnv
        wet(ihru)%psor = res_init(init)%psor * cnv
        wet(ihru)%baclp = res_init(init)%bactlp * cnv
        wet(ihru)%bacp = res_init(init)%bactp * cnv
      
        !! wetland on hru - solve quadratic to find new depth
        !testing relationship wet_vol(jres) = float(jj) * .1 * wet_pvol(jres)
        x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ires)%flo / wet_hyd(ihyd)%pvol)
        if (x1 < 1.e-6) then
          res_h = 0.
        else
          res_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
          wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
        end if
        wet_ob(ires)%area_ha = wet_hyd(ihyd)%psa * (1. + wet_hyd(ihyd)%acoef * wet_h)

      end do
      close(105)

      return
      end subroutine wet_initial