      subroutine res_hydro (jres, id, iac, ihyd, ised)

      use jrw_datalib_module
      use reservoir_module
      use conditional_module
      !use hydrograph_module
      use climate_parms
      use time_module
      
      real :: vol, vvr, targ, xx, flw, ndespill
      
      !! store initial values
      vol = res(jres)%flo
      nstep = 1

      do tstep = 1, nstep
          
        !calc release from decision table
        select case (d_tbl(id)%act(iac)%name)
        case ("release")
            !condition is met - set the release rate
            select case (d_tbl(id)%act(iac)%option)
            case ("rate")
              resflwo = d_tbl(id)%act(iac)%const * 86400.
            case ("days")
              resflwo = (res(ob_num)%flo - b_lo) / d_tbl(id)%act(iac)%const
            case ("weir")
              resflwo = res_weir(ihyd)%c * res_weir(ihyd)%k * res_weir(ihyd)%w * (res_h ** 1.5)
            case ("meas")
              irel = int(d_tbl(id)%act(iac)%const)
              select case (recall(irel)%typ)
              case (1)    !daily
                resflwo = recall(irel)%hd(time%day,time%yrs)%flo
              case (2)    !monthly
                resflwo = recall(irel)%hd(time%mo,time%yrs)%flo
              case (3)    !annual
                resflwo = recall(irel)%hd(1,time%yrs)%flo
              end select
            end select
        end select
            
      !! calculate surface area for day
      if (ires == 0) then       !ires??
        ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2
      else
        !wetland on hru
        !! solve quadratic to find new depth
        !testing relationship res_vol(jres) = float(jj) * .1 * res_pvol(jres)
        x1 = res_hyd(ihyd)%bcoef ** 2 + 4. * res_hyd(ihyd)%ccoef * (1. - res(jres)%flo / res_hyd(ihyd)%pvol)
        if (x1 < 1.e-6) then
          res_h = 0.
        else
          res_h1 = (-res_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * res_hyd(ihyd)%ccoef)
          res_h = res_h1 + res_hyd(ihyd)%bcoef
        end if
        ressa = res_hyd(ihyd)%psa * (1. + res_hyd(ihyd)%acoef * res_h)
      end if

      !! calculate water balance for day
      iob = res_ob(jres)%ob
      iwst = ob(iob)%wst
      resev = 10. * res_hyd(ihyd)%evrsv * wst(iwst)%weat%pet * ressa
      ressep = res_hyd(ihyd)%k * ressa * 240.
      respcp = respcp * ressa * 10.

      !! new water volume for day
      res(jres)%flo = res(jres)%flo + respcp + resflwi - resev - ressep  !resflwi ??

      !! if reservoir volume is less thanzero
      if (res(jres)%flo < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so
        !! that reservoir volume is zero
        ressep = ressep + res(jres)%flo
        res(jres)%flo = 0.
        ressa = 0.

        !! if seepage is less than volume deficit, take remainder
        !! from evaporation
        if (ressep < 0.) then
          resev = resev + ressep
          ressep = 0.
        end if
        exit
        
      else  !res volume > 0

        !! subtract outflow from reservoir storage
        res(jres)%flo = res(jres)%flo - resflwo
        if (res(jres)%flo < 0.) then
          resflwo = resflwo + res(jres)%flo
          res(jres)%flo = 0.
        end if

        !! update surface area for day
        ressa = res_hyd(ihyd)%br1 * res(jres)%flo ** res_hyd(ihyd)%br2

      end if    !res volume < > 0.
      end do    !tstep loop
      
      return
      end subroutine res_hydro