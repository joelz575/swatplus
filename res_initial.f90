      subroutine res_initial (mres)
      
      use reservoir_module
      use jrw_datalib_module
      use hydrograph_module
      use parm

      do ires = 1, mres
!     set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
!                                            ppm = t/m^3 * 10^6
      i = res_ob(ires)%props
      ihyd = res_dat(iob)%hyd
      res(ires)%flo = res_init(i)%vol * res_hyd(ihyd)%pvol
      res(ires)%sed = res_init(i)%sed * res(ires)%flo / 1000.
      res(ires)%orgn= res_init(i)%orgn * res(ires)%flo / 1000.
      res(ires)%no3 = res_init(i)%no3 * res(ires)%flo / 1000.
      res(ires)%no2 = res_init(i)%no2 * res(ires)%flo / 1000.
      res(ires)%nh3 = res_init(i)%nh3 * res(ires)%flo / 1000.
      res(ires)%sedp = res_init(i)%orgp * res(ires)%flo / 1000.
      res(ires)%solp = res_init(i)%solp * res(ires)%flo / 1000.
      res_ob(ires)%seci = res_init(i)%seci * res(ires)%flo / 1000.
      res(ires)%san = res_init(i)%san * res(ires)%flo / 1000.
      res(ires)%sil = res_init(i)%sil * res(ires)%flo / 1000.
      res(ires)%cla = res_init(i)%cla * res(ires)%flo / 1000.
      res(ires)%sag = res_init(i)%sag * res(ires)%flo / 1000.
      res(ires)%lag = res_init(i)%lag * res(ires)%flo / 1000.
      res(ires)%grv = res_init(i)%gra * res(ires)%flo / 1000.
      res(ires)%chla = res_init(i)%chla * res(ires)%flo / 1000.
      res(ires)%psor = res_init(i)%psor * res(ires)%flo / 1000.
      res(ires)%psor = res_init(i)%psor * res(ires)%flo /1000.
      res(ires)%baclp = res_init(i)%bactlp * res(ires)%flo / 1000.
      res(ires)%bacp = res_init(i)%bactp * res(ires)%flo / 1000.      

      end do
      close(105)

      return
      end subroutine res_initial