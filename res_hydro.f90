      subroutine res_hydro (jres, id, ihyd, ised)

      use jrw_datalib_module
      use reservoir_module
      use conditional_module
      use climate_parms
      use time_module
      use hydrograph_module, only : res, recall, ob
      
      real :: vol, vvr, targ, xx, flw, ndespill
      character(len=1) :: action
      
      !! store initial values
      vol = res(jres)%flo
      resflwo = 0.
      nstep = 1

      do tstep = 1, nstep
          
        !calc release from decision table
        do iac = 1, d_tbl(id)%acts
          action = "n"
          do ial = 1, d_tbl(id)%alts
            if (d_tbl(id)%act_hit(ial) == "y" .and. d_tbl(id)%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == 'y') then
            select case (d_tbl(id)%act(iac)%option)
            case ("rate")
              resflwo = d_tbl(id)%act(iac)%const * 86400.
            case ("days")
              select case (d_tbl(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = res_ob(ihyd)%pvol
                case ("evol")
                  b_lo = res_ob(ihyd)%evol
              end select
              resflwo = (res(jres)%flo - b_lo) / d_tbl(id)%act(iac)%const
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
          end if
        end do    ! iac

      !! calculate water balance for day
      iob = res_ob(jres)%ob
      iwst = ob(iob)%wst
      resev = 10. * res_hyd(ihyd)%evrsv * wst(iwst)%weat%pet * res_ob(jres)%area_ha
      ressep = 240. * res_hyd(ihyd)%k * res_ob(jres)%area_ha
      respcp = 10. * wst(iwst)%weat%precip * res_ob(jres)%area_ha

      !! new water volume for day
      res(jres)%flo = res(jres)%flo + respcp + resflwi - resev - ressep

      !! if reservoir volume is less thanzero
      if (res(jres)%flo < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so
        !! that reservoir volume is zero
        ressep = ressep + res(jres)%flo
        res(jres)%flo = 0.
        res_ob(jres)%area_ha = 0.

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

      !! update surface area
      res_ob(jres)%area_ha = res_ob(jres)%br1 * res(jres)%flo ** res_ob(jres)%br2

      end if    !res volume < > 0.
      end do    !tstep loop
      
      return
      end subroutine res_hydro