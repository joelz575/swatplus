      !! function to convert concentration to mass
      subroutine res_convert_mass (hyd1)
      
      use reservoir_module
      use hydrograph_module
        !! sediment stays in conc - nutrients convert to mass
        type (hyd_output), intent (inout) :: hyd1
        ! input as frac of principal - convert in res_initial
        hyd1%flo = hyd1%flo
        ! t = ppm * m3 / 1000000.
        hyd1%sed = hyd1%sed * hyd1%flo / 1000000.
        ! kg = ppm * m3 / 1000.
        hyd1%orgn = hyd1%orgn * hyd1%flo / 1000.
        hyd1%sedp = hyd1%sedp * hyd1%flo / 1000.
        hyd1%no3 = hyd1%no3 * hyd1%flo / 1000.
        hyd1%solp = hyd1%solp * hyd1%flo / 1000.
        hyd1%chla = hyd1%chla * hyd1%flo / 1000.
        hyd1%nh3 = hyd1%nh3 * hyd1%flo / 1000.
        hyd1%no2 = hyd1%no2 * hyd1%flo / 1000.
        hyd1%cbod = hyd1%cbod * hyd1%flo / 1000.
        hyd1%dox = hyd1%dox * hyd1%flo / 1000.
        hyd1%san = hyd1%san * hyd1%flo / 1000000.
        hyd1%sil = hyd1%sil * hyd1%flo / 1000000.
        hyd1%cla = hyd1%cla * hyd1%flo / 1000000.
        hyd1%sag = hyd1%sag * hyd1%flo / 1000000.
        hyd1%lag = hyd1%lag * hyd1%flo / 1000000.
        hyd1%grv = hyd1%grv * hyd1%flo / 1000000.
      end subroutine res_convert_mass
