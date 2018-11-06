      subroutine res_hydro (jres, id, ihyd, ised)

      use reservoir_data_module
      use reservoir_module
      use conditional_module
      use climate_module
      use time_module
      use hydrograph_module, only : res, recall, ob
      
      implicit none
      
      integer :: jres             !none      |hru number
      integer :: nstep            !none      |counter
      integer :: tstep            !none      |hru number
      integer :: iac              !none      |counter 
      integer :: id               !none      |hru number
      integer :: ial              !none      |counter
      integer :: irel             !          |
      integer :: iob              !          |
      integer :: iwst             !none      |hru number
      integer :: ised             !none      |counter
      integer :: ihyd             !          |
      real :: vol                 !          |
      real :: b_lo                !          |
      character(len=1) :: action  !          |
      real :: res_h               !          |

      
      !! store initial values
      vol = res(jres)%flo
      resflwo = 0.
      nstep = 1

      do tstep = 1, nstep
          
        !calc release from decision table
        do iac = 1, dtbl_res(id)%acts
          action = "n"
          do ial = 1, dtbl_res(id)%alts
            if (dtbl_res(id)%act_hit(ial) == "y" .and. dtbl_res(id)%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == "y") then
            select case (dtbl_res(id)%act(iac)%option)
            case ("rate")
              resflwo = dtbl_res(id)%act(iac)%const * 86400.
            case ("days")
              select case (dtbl_res(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = res_ob(ihyd)%pvol
                case ("evol")
                  b_lo = res_ob(ihyd)%evol
              end select
              resflwo = (res(jres)%flo - b_lo) / dtbl_res(id)%act(iac)%const
            case ("weir")
              resflwo = res_weir(ihyd)%c * res_weir(ihyd)%k * res_weir(ihyd)%w * (res_h ** 1.5)
            case ("meas")
              irel = int(dtbl_res(id)%act(iac)%const)
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