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
        wet(iprop)%flo = res_init(init)%vol * wet_ob(iprop)%pvol
        wet(iprop)%sed = res_init(init)%sed * cnv
        wet(iprop)%orgn= res_init(init)%orgn * cnv
        wet(iprop)%no3 = res_init(init)%no3 * cnv
        wet(iprop)%no2 = res_init(init)%no2 * cnv
        wet(iprop)%nh3 = res_init(init)%nh3 * cnv
        wet(iprop)%sedp = res_init(init)%orgp * cnv
        wet(iprop)%solp = res_init(init)%solp * cnv
        wet(iprop)%san = res_init(init)%san * cnv
        wet(iprop)%sil = res_init(init)%sil * cnv
        wet(iprop)%cla = res_init(init)%cla * cnv
        wet(iprop)%sag = res_init(init)%sag * cnv
        wet(iprop)%lag = res_init(init)%lag * cnv
        wet(iprop)%grv = res_init(init)%gra * wet(iprop)%flo / 1000.
        wet(iprop)%chla = res_init(init)%chla * cnv
        wet(iprop)%psor = res_init(init)%psor * cnv
        wet(iprop)%psor = res_init(init)%psor * cnv
        wet(iprop)%baclp = res_init(init)%bactlp * cnv
        wet(iprop)%bacp = res_init(init)%bactp * cnv
      
        !! wetland on hru - solve quadratic to find new depth
        !testing relationship wet_vol(jres) = float(jj) * .1 * wet_pvol(jres)
        x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ires)%flo / wet_hyd(ihyd)%pdep)
        if (x1 < 1.e-6) then
          res_h = 0.
        else
          res_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
          wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
        end if
        wet_ob(iprop)%area_ha = wet_ob(ires)%psa * (1. + wet_hyd(ihyd)%acoef * wet_h)

      end do
      close(105)

      return
      end subroutine wet_initial