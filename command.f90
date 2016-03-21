      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!! 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!                |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use hydrograph_module
      use hru_module
      use subbasin_module
      use channel_module
      use sd_hru_module
      use aquifer_module
      use sd_channel_module
      use reservoir_module
      !use recall_apex_daily
      !use recall_daily
      !use recmon_module
      !use recall_hourly
      ! use impound_module
      !use wateruse_module
      !use recyear_module
      !use sd_channel_module

      icmd = sp_ob1%objs
      do while (icmd /= 0)
        isur_in = 0
        isub_in = 0
        !subdaily - set current day of hydrograph
        if (time%step > 0) then
          !update current day of hydrograph for the object
          ob(icmd)%day_cur = ob(icmd)%day_cur + 1
          if (ob(icmd)%day_cur > ob(icmd)%day_max) ob(icmd)%day_cur = 1
        end if
        
        !sum all receiving hydrographs
        if (ob(icmd)%rcv_tot > 0) then
          ob(icmd)%hin = hz
          ob(icmd)%hin_s = hz
          if (time%step > 0) ob(icmd)%tsin(:) = hz
          ob(icmd)%peakrate = 0.
          do in = 1, ob(icmd)%rcv_tot
            !check to see if the object is in a subbasin
            !if routing over an hru, it can only be in one subbasin
            !element type should be an hru if routing over - if element is a channel use incoming directly
            !CEAP uses representative 1st order streams as elements to define upland sub
            if (ob(icmd)%subs_tot > 0 .and. ob(icmd)%typ <= "hru_lte") then
              ielem = ob(icmd)%elem
              isub = sub_elem(ielem)%sub(1) !can only be in one subbasin if routing over
              isub = sp_ob1%sub + isub - 1  !object number of the subbasin
              iob = ob(isub)%obj_in(in)
              ihyd = ob(isub)%htyp_in(in)
              ht1 = ob(isub)%frac_in(in) * ob(iob)%hd(ihyd)  !fraction of hydrograph
              ht1 = sub_elem(ielem)%frac * ht1               !fraction of hru in subbasin
            else
              iob = ob(icmd)%obj_in(in)
              if (iob == 4) then
                !! add flow to channel (or appropriate object)  **Ryan**
                !ht1%flo = 
              else
                ihyd = ob(icmd)%htyp_in(in)
                ht1 = ob(icmd)%frac_in(in) * ob(iob)%hd(ihyd)
              end if
            end if
            !set peak rate in
            ob(icmd)%peakrate = ob(iob)%peakrate
            
            if (ihyd == 4) then  !route lat flow through soil
              ob(icmd)%hin_s = ob(icmd)%hin_s + ht1
              isub_in = 1
            else
              ob(icmd)%hin = ob(icmd)%hin + ht1
              isur_in = 1
            end if
            !print individual inflow hyds
            !call hydin_output (in)      !deal with later
            
            !sum subdaily hydrographs
            if (time%step > 0) then
              do kk = 1, time%step
                iday = ob(iob)%day_cur
                ob(icmd)%tsin(kk) = ob(icmd)%tsin(kk) + ob(iob)%ts(iday,kk)
              end do
            end if
          end do
            
          if (isur_in == 1) then
            ht1 = ob(icmd)%hin
            call hydin_output (1,'sur')
          end if
          if (isub_in == 1) then
            ht1 = ob(icmd)%hin_s
            call hydin_output (2,'sub')
          end if
            
          !convert to per area basis
          if (ob(icmd)%typ == "sub" .or. ob(icmd)%typ == "hru") then  !only convert hru and subbasin hyds for routing
            if (ob(icmd)%subs_tot > 0) then
              !object is in a subbasin
              ielem = ob(icmd)%elem
              isub = sub_elem(ielem)%sub(1)  !can only be in one subbasin if routing over
              conv = 100. * sub(isub)%da_km2 * sub_elem(ielem)%frac
            else
              conv = ob(icmd)%area_ha
            end if
            ob(icmd)%hin_s = ob(icmd)%hin_s // conv
            ob(icmd)%hin = ob(icmd)%hin // conv
          end if
        end if

        ! select the next command type

        select case (ob(icmd)%typ)
            
          case ("hru")   ! hru
            ihru = ob(icmd)%num
            call hru_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output
                      
          case ("hru_lte")   ! hru_lte
            isd = ob(icmd)%num
            call sd_hru_control (isd)
            if (ob(icmd)%rcv_tot > 0) call hyddep_output
            
          case ("sub")   ! subbasin
            isub = ob(icmd)%num
            call subbasin_control
            if (ob(icmd)%rcv_tot > 0) call hyddep_output

          case ("modflow")   ! modflow
            !! call modflow (daily)  **Ryan**
            
          case ("aqu")   ! aquifer
            if (ob(icmd)%dfn_tot == 0) then   !1-D use old bf recession
              call aqu_1d_control
              if (time%yrs > pco%nyskip) then
                call aquifer_output
              endif
            end if
          
          case ("chan")   ! channel
            jrch = ob(icmd)%num
            jrchq = ob(icmd)%props2
            if (ob(icmd)%rcv_tot > 0) then
              call channel_control
            end if
                        
          case ("res")   ! reservoir
            jres = ob(icmd)%num
            if (ob(icmd)%rcv_tot > 0) then
              call res_control (jres)
            end if
              
            case ("recall")   ! recall hydrograph
                irec = ob(icmd)%num
                select case (recall(irec)%typ)
                  case (1)    !daily
                    ob(icmd)%hd(1) = recall(irec)%hd(time%day,time%yrs)
                  case (2)    !monthly
                    ob(icmd)%hd(1) = recall(irec)%hd(time%mo,time%yrs)
                  case (3)    !annual
                    ob(icmd)%hd(1) = recall(irec)%hd(1,time%yrs)
                end select

          !case ("exco")   ! export coefficient hyds are set at start

          case ("dr")   ! delivery ratios
            ob(icmd)%hd(1) = ob(icmd)%hin ** dr(ob(icmd)%props)
            
          case ("outlet")  !outlet
            !all recieving hydrographs have been summed
              
          case ("chandeg")  !swatdeg channel
            isdch = ob(icmd)%num
            isd_chsur = ob(icmd)%props2
            call sd_channel_control
            
        end select
        
        !print all outflow hydrographs
        if (ob(icmd)%src_tot > 0) then
          do iout = 1, ob(icmd)%src_tot
            ht1 = ob(icmd)%frac_out(iout) * ob(icmd)%hd(iout)
            call hydout_output (iout)
          end do
        end if
  
        !set the next command
        icmd = ob(icmd)%cmd_next
        
          end do
          
      if (time%yrs > pco%nyskip) then
          call obj_output
      end if
      
      if (time%yrs > pco%nyskip .and. time%step == 0) then 
        call basin_output
      end if
      
      return
      end