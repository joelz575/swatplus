      subroutine res_initial (mres)
      
      use reservoir_module
      use jrw_datalib_module
      use hydrograph_module
      use parm

      do ires = 1, mres
!     set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
!                                            ppm = t/m^3 * 10^6
      i = res_ob(ires)%props
      ihyd = res_dat(i)%hyd
      init = res_dat(i)%init
      res(ires)%flo = res_init(init)%vol * res_hyd(ihyd)%pvol
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

      end do
      close(105)

      return
      end subroutine res_initial