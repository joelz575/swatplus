      subroutine subbasin_control
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!

      use parm
      
      character (len=3) :: ihtyp
      
      iday = 1
      ihdmx = 2
      cnv_m3 = 1000. * sub(isub)%da_km2
      
      hyd_flo = 0.
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      ob(icmd)%hd(3) = hz
      ob(icmd)%hd(4) = hz
      ob(icmd)%hd(5) = hz
      swb_d(isub) = hwbz
      snb_d(isub) = hnbz
      sls_d(isub) = hlsz
      spw_d(isub) = hpwz
      
      do ielem = 1, sub_d(isub)%num_tot
        ise = sub_d(isub)%num(ielem)
        iob = sub_elem(ise)%obj
        ihru = sub_elem(ise)%obtypno
        ihtyp = sub_elem(ise)%htyp
        ht1 = hz
        ht2 = hz
        ht3 = hz
        ht4 = hz
        ht5 = hz
        delrto = hz
        
        !define delivery ratio - all variables are hyd_output type
        idr = sub_elem(ise)%idr
        ihtypno = sub_elem(ise)%htypno
        if (idr > 0) then
          !input dr from sub_dr.dat
          delrto = sub_dr(idr)
          else
          !calculated dr = f(tconc element/ tconc sub)
          delrto = sub_elem(ielem)%dr(0)
        end if

        if (sub_elem(ielem)%obtyp == "exc") then
        !! compute hyds for export coefficients-ht1==surface,ht2==groundwater
          ht1 = exco(ob(iob)%props) ** sub_dr(sub_elem(ise)%idr)
          ht2 = hz
          if (ob(iob)%area_ha > .01) then
            !per area units - mm, t/ha, kg/ha (ie hru, apex)
            ef = sub_elem(ise)%frac * sub(isub)%da_km2 /                   &
                                                     (ob(iob)%area_ha / 100.)
            ht1 = ef * ht1
            !ht2 = exco(ob(iob)%props2) ** dr(sub_elem(ise)%idr)
          end if
          
        else

        select case (sub_elem(ielem)%obtyp)
        case ("hru")
        !define expansion factor to surface/soil and recharge
        ef = sub_elem(ise)%frac * sub(isub)%da_km2 /                      &
                                                 (hru(ihru)%area_ha / 100.)
        case ("hlt")
        ef = sub_elem(ise)%frac * sub(isub)%da_km2 / sd_db(ihru)%dakm2
        end select
        
        !compute all hyd's needed for routing
        select case (ihtypno)
        case (1)   !total hyd
          ht1 = ob(iob)%hd(1) ** delrto
          ht1 = ef * ht1
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
        case (4)   !soil lateral flow hyd
          ht1 = ob(iob)%hd(ihtypno) ** delrto
          ht1 = ef * ht1
          ht4 = ob(iob)%hd(ihtypno) ** delrto
          ht4 = ef * ht4
        case (5)   !tile flow hyd
          ht1 = ef * ob(iob)%hd(ihtypno)
          ht5 = ef * ob(iob)%hd(ihtypno)  !no dr on tile
        end select

        !recharge hyd - hru_lte computes gw flow and doesnt need recharge hyd
        if (sub_elem(ielem)%obtyp /= "hlt") ht2 = ef * ob(iob)%hd(2)
        
        ! sum subdaily hydrographs for each object
        if (time%step > 0) then
          select case (sub_elem(ielem)%obtyp)
          case ("hru")
            do ii = 1, time%step
              hyd_flo(ii) = hyd_flo(ii) + hhsurfq(ihru,ii) * sub_elem(ise)%frac * delrto%flo * cnv_m3
            end do
          end select
        end if

        ! summing HRU output for the subbasin
        if (sub_elem(ise)%frac > 1.e-9) then
          const = 1. / sub_elem(ise)%frac   !only have / operator set up (could * frac_dfn directly)
          if (sub_elem(ise)%obtyp == "hru") then
            swb_d(isub) = swb_d(isub) + hwb_d(ihru) / const
            snb_d(isub) = snb_d(isub) + hnb_d(ihru) / const
            sls_d(isub) = sls_d(isub) + hls_d(ihru) / const
            spw_d(isub) = spw_d(isub) + hpw_d(ihru) / const
          end if
          ! summing HRU_LTE output
          if (sub_elem(ise)%obtyp == "hlt") then
            swb_d(isub) = swb_d(isub) + sdwb_d(ihru) / const
            snb_d(isub) = snb_d(isub) + sdnb_d(ihru) / const
            sls_d(isub) = sls_d(isub) + sdls_d(ihru) / const
            spw_d(isub) = spw_d(isub) + sdpw_d(ihru) / const
          end if
        end if
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
              rto = amax1 (1., ob(icmd)%hd(3)%flo / sumflo)
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
      
      
        
      ! summing subbasin output for the basin
      if (ob(icmd)%area_ha > 1.e-12 .and. bsn%area_ha > 1.e-12) then
        const = bsn%area_ha / ob(icmd)%area_ha        !only have / operator set up
        bwb_d = bwb_d + swb_d(isub) / const
        bnb_d = bnb_d + snb_d(isub) / const
        bls_d = bls_d + sls_d(isub) / const
        bpw_d = bpw_d + spw_d(isub) / const
      end if

       if (time%yrs > pco%nyskip .and. time%step == 0)                  & 
                                        call subbasin_output
      
	return

      end subroutine subbasin_control