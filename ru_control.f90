      subroutine ru_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use parm, only : hhsurfq, ihru, qday 
      use ru_module
      use hydrograph_module
      use time_module
      
      character (len=3) :: ihtyp
      
      iday = 1
      ihdmx = 2
      cnv_m3 = 1000. * ru(isub)%da_km2
      
      hyd_flo = 0.
      ru_d(isub) = hz
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      
      sumfrac = 0.
      sumarea = 0.
      
      do ielem = 1, ru_def(isub)%num_tot
        ise = ru_def(isub)%num(ielem)
        iob = ru_elem(ise)%obj
        ihru = ru_elem(ise)%obtypno
        ihtyp = ru_elem(ise)%htyp
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        sumfrac = sumfrac + ru_elem(ise)%frac
        sumarea = sumarea + ob(iob)%area_ha
        
        !define delivery ratio - all variables are hyd_output type
        idr = ru_elem(ise)%idr
        ihtypno = ru_elem(ise)%htypno
        if (idr > 0) then
          !input dr from sub_dr.dat
          delrto = ru_dr(idr)
        else
          !calculated dr = f(tconc element/ tconc sub)
          delrto = ru_elem(ielem)%dr(0)
        end if

        if (ru_elem(ielem)%obtyp == "exc") then
        !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iob)%props) ** ru_dr(ru_elem(ise)%idr)
          ht2 = hz
          if (ob(iob)%area_ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = ru_elem(ise)%frac * ru(isub)%da_km2 /                   &
                                                     (ob(iob)%area_ha / 100.)
            ht1 = ef * ht1
            !ht2 = exco(ob(iob)%props2) ** dr(ru_elem(ise)%idr)
          end if
          
        else

        !define expansion factor to surface/soil and recharge
        ef = ru_elem(ise)%frac * ru(isub)%da_km2 / (ob(iob)%area_ha / 100.)
        
        !compute all hyd's needed for routing
        select case (ihtypno)
        case (1)   !total hyd
          ht1 = ob(iob)%hd(1) ** delrto
          ht1 = ef * ht1
          ru_d(isub) = ru_d(isub) + ht1
          if (ob(iob)%typ == "hru") then
            ht3 = ob(iob)%hd(3) ** delrto
            ht3 = ef * ht3
            ht4 = ob(iob)%hd(4) ** delrto
            ht4 = ef * ht4
            ht5 = ef * ob(iob)%hd(5)  !no dr on tile
          end if
        case (3)   !surface runoff hyd
          ht1 = ob(iob)%hd(ihtypno) ** delrto
          ht1 = ef * ht1
          ht3 = ob(iob)%hd(ihtypno) ** delrto
          ht3 = ef * ht3
          ru_d(isub) = ru_d(isub) + ht3
        case (4)   !soil lateral flow hyd
          ht1 = ob(iob)%hd(ihtypno) ** delrto
          ht1 = ef * ht1
          ht4 = ob(iob)%hd(ihtypno) ** delrto
          ht4 = ef * ht4
          ru_d(isub) = ru_d(isub) + ht4
        case (5)   !tile flow hyd
          ht1 = ef * ob(iob)%hd(ihtypno)
          ht5 = ef * ob(iob)%hd(ihtypno)  !no dr on tile
          ru_d(isub) = ru_d(isub) + ht5
        end select
        end if      !ru_elem(ielem)%obtyp == "exc"
        
        !recharge hyd - hru_lte computes gw flow and doesnt need recharge hyd
        if (ru_elem(ielem)%obtyp /= "hlt") then
          ht2 = ef * ob(iob)%hd(2)
          ru_d(isub) = ru_d(isub) + ht2
        end if
                
        ! sum subdaily hydrographs for each object
        if (time%step > 0) then
          select case (ru_elem(ielem)%obtyp)
          case ("hru")
            do ii = 1, time%step
              hyd_flo(ii) = hyd_flo(ii) + hhsurfq(ihru,ii) * ru_elem(ise)%frac * delrto%flo * cnv_m3
            end do
          end select
        end if

      ob(icmd)%hd(1) = ob(icmd)%hd(1) + ht1             !sum total hyd 
      ob(icmd)%hd(2) = ob(icmd)%hd(2) + ht2             !sum recharge hyd
      ob(icmd)%hd(3) = ob(icmd)%hd(3) + ht3             !sum surface hyd 
      ob(icmd)%hd(4) = ob(icmd)%hd(4) + ht4             !sum lateral soil hyd
      ob(icmd)%hd(5) = ob(icmd)%hd(5) + ht5             !sum tile hyd 
          
      end do  !element loop
      
      !! set subdaily hydrographs
      if (time%step > 0) then
      iday = ob(icmd)%day_cur
      iday_prev = iday - 1
      if (iday_prev < 1) iday_prev = 2
      
      !! subsurface flow = lateral + tile
      ssq = (ob(icmd)%hd(4)%flo + ob(icmd)%hd(5)%flo) / time%step
        
      !! zero previous days hyds - current day is the hyd from yesterday so its set
      qday = 0.
      do kk = 1, time%step
        ob(icmd)%ts(iday_prev,kk) = hz
        qday = qday + hyd_flo(kk)
      end do

      if (qday > 1.e-9) then
          
        !! use unit hydrograph to compute subdaily flow hydrographs
        sumflo = 0.  !sum flow in case hydrograph exceeds max days 
        
        do ii = 1, time%step !loop for total time steps in a day
          itot = ii
          do ib = 1, itsb(isub)  !loop for number of steps in the unit hydrograph base time
            itot = itot + ib - 1
            if (itot > time%step) then
              iday = iday + 1
              if (iday > ihdmx) iday = 1
              itot = 1
            end if

            !! check to see if day has gone past the max allocated days- uh > 1 day
            if (iday <= ihdmx) then
              ob(icmd)%ts(iday,itot)%flo = ob(icmd)%ts(iday,itot)%flo + hyd_flo(ii) * uhs(isub,ib)
              sumflo = sumflo + ob(icmd)%ts(iday,itot)%flo
            else
              !! adjust if flow exceeded max days
              rto = Max (1., ob(icmd)%hd(3)%flo / sumflo)
              do iadj = 1, itot - 1
                iday = iadj / time%step + 1
                istep = iadj - (iday - 1) * time%step
                ob(icmd)%ts(iday,itot)%flo = ob(icmd)%ts(iday,itot)%flo * rto
              end do
            end if
          end do
        end do
        
        sumflo_day = 0.
        iday = ob(icmd)%day_cur
        do istep = 1, time%step
          ob(icmd)%ts(iday,istep)%flo = ob(icmd)%ts(iday,istep)%flo + ssq
          sumflo_day = sumflo_day + ob(icmd)%ts(iday,istep)%flo
        end do

        !! set values for other routing variables - assume constant concentration
        !! storage locations set to zero are not currently used
        do ii = 1, time%step
          ratio = ob(icmd)%ts(iday,ii)%flo / sumflo_day
            if (ob(icmd)%hd(1)%flo > 0.) then
              ob(icmd)%ts(iday,ii)%temp = wtmp                                !!wtmp
              ob(icmd)%ts(iday,ii)%sed = ob(icmd)%hd(1)%sed * ratio           !!sedyld
              ob(icmd)%ts(iday,ii)%orgn = ob(icmd)%hd(1)%orgn * ratio         !!sedorgn
              ob(icmd)%ts(iday,ii)%sedp = ob(icmd)%hd(1)%sedp * ratio         !!sedorgp
              ob(icmd)%ts(iday,ii)%no3 = ob(icmd)%hd(1)%no3 * ratio           !!no3
              ob(icmd)%ts(iday,ii)%solp = ob(icmd)%hd(1)%solp * ratio         !!minp
              ob(icmd)%ts(iday,ii)%psol = ob(icmd)%hd(1)%psol * ratio         !!sol pst
              ob(icmd)%ts(iday,ii)%psor = ob(icmd)%hd(1)%psor * ratio         !!sorb pst
              ob(icmd)%ts(iday,ii)%chla = ob(icmd)%hd(1)%chla * ratio         !!chl_a
              ob(icmd)%ts(iday,ii)%nh3 = 0.                                   !! NH3
              ob(icmd)%ts(iday,ii)%no2 = 0.                                   !! NO2
              ob(icmd)%ts(iday,ii)%cbod = ob(icmd)%hd(1)%cbod * ratio         !!cbodu
              ob(icmd)%ts(iday,ii)%dox = ob(icmd)%hd(1)%dox * ratio           !!doxq & soxy
              ob(icmd)%ts(iday,ii)%bacp = ob(icmd)%hd(1)%bacp * ratio         !!bactp
              ob(icmd)%ts(iday,ii)%baclp = ob(icmd)%hd(1)%baclp * ratio       !!bactlp
              ob(icmd)%ts(iday,ii)%met1 = 0.                                  !!cmetal#1
              ob(icmd)%ts(iday,ii)%met2 = 0.                                  !!cmetal#2
              ob(icmd)%ts(iday,ii)%met3 = 0.                                  !!cmetal#3  
            end if
          end do
        else
          !! no surface runoff on current day so zero hyds
          do istep = 1, time%step
            ob(icmd)%ts(iday,istep)%flo = ssq
          end do
        end if  ! qday > 0
      end if  ! time%step  > 0

	return

      end subroutine ru_control