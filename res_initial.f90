      subroutine res_initial (mres)
      
      use reservoir_module
      use jrw_datalib_module
      use hydrograph_module
      use parm
  
      do ires = 1, mres
        !! set initial volumes for res and hru types
        !! convert units
        iprop = res_ob(ires)%props
        ihyd = res_dat(iprop)%hyd
        if (res_hyd(ires)%in_unit == 0) then
          res_ob(ires)%evol = res_hyd(ihyd)%evol * 10000.       !! ha-m => m**3
          res_ob(ires)%pvol = res_hyd(ihyd)%pvol * 10000.       !! ha-m => m**3
          res_ob(ires)%esa = res_hyd(ihyd)%esa
          res_ob(ires)%psa = res_hyd(ihyd)%psa
        end if
        if (res_hyd(ires)%in_unit == 1) then
          ihru = res_ob(ires)%ob
          res_hyd(ires)%evol = res_hyd(ires)%evol * 10.          !! ha-m => m**3
          res_hyd(ires)%pvol = res_hyd(ires)%pvol * 10.          !! ha-m => m**3
        end if
        
        !! calculate shape parameters for surface area equation
        resdif = res_hyd(ihyd)%evol - res_hyd(ihyd)%pvol
        if ((res_hyd(ihyd)%esa - res_hyd(ihyd)%psa) > 0. .and. resdif > 0.) then
          lnvol = Log10(res_ob(ires)%evol) - Log10(res_ob(ires)%pvol)
          if (lnvol > 1.e-4) then
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / lnvol
          else  
            res_ob(ires)%br2 = (Log10(res_ob(ires)%esa) - Log10(res_ob(ires)%psa)) / 0.001
          end if
          if (res_ob(ires)%br2 > 0.9) then
            res_ob(ires)%br2 = 0.9
            res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
          else
            res_ob(ires)%br1 = (res_ob(ires)%esa / res_ob(ires)%evol) ** res_ob(iires)%br2
          end if  
        else
          res_ob(ires)%br2 = 0.9
          res_ob(ires)%br1 = (res_ob(ires)%psa / res_ob(ires)%pvol) ** 0.9
        end if
        
      end do
      
      do ires = 1, mres
      !!set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
      !!                                       ppm = t/m^3 * 10^6
      i = res_ob(ires)%props
      ihyd = res_dat(i)%hyd
      init = res_dat(i)%init
      
      res(ires)%flo = res_init(init)%vol * res_ob(ires)%pvol
      res(ires)%sed = res_init(init)%sed * res(ires)%flo / 1000.
      res(ires)%orgn= res_init(init)%orgn * res(ires)%flo / 1000.
      res(ires)%no3 = res_init(init)%no3 * res(ires)%flo / 1000.
      res(ires)%no2 = res_init(init)%no2 * res(ires)%flo / 1000.
      res(ires)%nh3 = res_init(init)%nh3 * res(ires)%flo / 1000.
      res(ires)%sedp = res_init(init)%orgp * res(ires)%flo / 1000.
      res(ires)%solp = res_init(init)%solp * res(ires)%flo / 1000.
      res_ob(ires)%seci = res_init(init)%seci * res(ires)%flo / 1000.
      res(ires)%san = res_init(init)%san * res(ires)%flo / 1000.
      res(ires)%sil = res_init(init)%sil * res(ires)%flo / 1000.
      res(ires)%cla = res_init(init)%cla * res(ires)%flo / 1000.
      res(ires)%sag = res_init(init)%sag * res(ires)%flo / 1000.
      res(ires)%lag = res_init(init)%lag * res(ires)%flo / 1000.
      res(ires)%grv = res_init(init)%gra * res(ires)%flo / 1000.
      res(ires)%chla = res_init(init)%chla * res(ires)%flo / 1000.
      res(ires)%psor = res_init(init)%psor * res(ires)%flo / 1000.
      res(ires)%psor = res_init(init)%psor * res(ires)%flo /1000.
      res(ires)%baclp = res_init(init)%bactlp * res(ires)%flo / 1000.
      res(ires)%bacp = res_init(init)%bactp * res(ires)%flo / 1000.      

      !! calculate initial surface area
      if (res_ob(ires)%typ == 'res') then       
        !! reservoir - old area volume relationship
        res_ob(ires)%area_ha = res_hyd(ihyd)%br1 * res(ires)%flo ** res_hyd(ihyd)%br2
      else
        !! wetland on hru - solve quadratic to find new depth
        !testing relationship res_vol(jres) = float(jj) * .1 * res_pvol(jres)
        x1 = res_hyd(ihyd)%bcoef ** 2 + 4. * res_hyd(ihyd)%ccoef * (1. - res(ires)%flo / res_hyd(ihyd)%pvol)
        if (x1 < 1.e-6) then
          res_h = 0.
        else
          res_h1 = (-res_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * res_hyd(ihyd)%ccoef)
          res_h = res_h1 + res_hyd(ihyd)%bcoef
        end if
        res_ob(ires)%area_ha = res_hyd(ihyd)%psa * (1. + res_hyd(ihyd)%acoef * res_h)
      end if

      end do
      close(105)

      return
      end subroutine res_initial