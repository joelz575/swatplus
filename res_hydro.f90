      subroutine res_hydro (jres, ihyd, ised)

      use jrw_datalib_module
      use reservoir_module
      use conditional_module
      !use hydrograph_module
      use climate_parms
      use time_module
      
      real :: vol, vvr, targ, xx, flw, ndespill

      !! store initial values
      vol = res(jres)%flo

      !! calculate surface area for day
      ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2

      !! calculate water balance for day
      resev = 10. * res_hyd(ihyd)%evrsv * pet_day * ressa
      ressep = res_hyd(ihyd)%k * ressa * 240.
      respcp = respcp * ressa * 10.

      !! new water volume for day
      res(jres)%flo = res(jres)%flo + respcp + resflwi - resev - ressep

      !! if reservoir volume is less thanzero
      if (res(jres)%flo < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so
        !! that reservoir volume is zero
        ressep = ressep + res(jres)%flo
        res(jres)%flo = 0.

        !! if seepage is less than volume deficit, take remainder
        !! from evaporation
        if (ressep < 0.) then
          resev = resev + ressep
          ressep = 0.
        end if

      else  !res volume > 0
          
        !! determine reservoir outflow
        irel = res_dat(res_ob(jres)%props)%release
        iob_rel = res_ob(jres)%ob
        call conditions (irel, iob_rel)
        call actions (irel, iob_rel)
      
        !! subtract outflow from reservoir storage
        res(jres)%flo = res(jres)%flo - resflwo
        if (res(jres)%flo < 0.) then
          resflwo = resflwo + res(jres)%flo
          res(jres)%flo = 0.
        end if

       !! update surface area for day
        ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2

      end if   !res volume < > 0.
      
      return
      end subroutine res_hydro