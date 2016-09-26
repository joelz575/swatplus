      subroutine subbasin_output
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
     
        !! sum monthly variables
        swb_m(isub) = swb_m(isub) + swb_d(isub)
        snb_m(isub) = snb_m(isub) + snb_d(isub)
        sls_m(isub) = sls_m(isub) + sls_d(isub)
        spw_m(isub) = spw_m(isub) + spw_d(isub)
        
!!!!! daily print - SUBBASIN
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%wb_sub == 3) then
            write (4200,100) time%day, time%yrc, isub, swb_d(isub)  !! waterbal
              if (pco%csvout == 1 .and. pco%wb_sub == 3) then 
                write (4031,'(*(G0.3,:","))') time%day, time%yrc, isub, swb_d(isub)  !! waterbal
              end if 
          end if 
          if (pco%nb_sub == 3) then
            write (4201,100) time%day, time%yrc, isub, snb_d(isub)  !! nutrient bal
            if (pco%csvout == 1 .and. pco%nb_sub == 3) then 
              write (4033,'(*(G0.3,:","))') time%day, time%yrc, isub, snb_d(isub)  !! nutrient bal
            end if 
          end if
          if (pco%ls_sub == 3) then
            write (4202,100) time%day, time%yrc, isub, sls_d(isub)  !! losses
            if (pco%csvout == 1 .and. pco%ls_sub == 3) then 
              write (4035,'(*(G0.3,:","))') time%day, time%yrc, isub, sls_d(isub)  !! losses
            end if 
          end if
          if (pco%pw_sub == 3) then
            write (4203,100) time%day, time%yrc, isub, spw_d(isub)  !! plant weather
            if (pco%csvout == 1 .and. pco%pw_sub == 3) then 
              write (4037,'(*(G0.3,:","))') time%day, time%yrc, isub, spw_d(isub)  !! plant weather 
            end if
          end if 
       end if

!!!!! monthly print - SUBBASIN
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo)) 
          spw_m(isub) = spw_m(isub) // const
          swb_m(isub)%cn = swb_m(isub)%cn / const 
          swb_m(isub)%sw = swb_m(isub)%sw / const
          swb_y(isub) = swb_y(isub) + swb_m(isub)
          snb_y(isub) = snb_y(isub) + snb_m(isub)
          sls_y(isub) = sls_y(isub) + sls_m(isub)
          spw_y(isub) = spw_y(isub) + spw_m(isub)
          if (pco%wb_sub == 2) then
            write (4200,100) time%mo, time%yrc, isub, swb_m(isub)
            if (pco%csvout == 1 .and. pco%wb_sub == 2) then 
              write (4031,'(*(G0.3,:","))') time%mo, time%yrc, isub, swb_m(isub)
            end if 
          end if
          if (pco%nb_sub == 2) then 
            write (4201,100) time%mo, time%yrc, isub, snb_m(isub)
            if (pco%csvout == 1 .and. pco%nb_sub == 2) then 
              write (4033,'(*(G0.3,:","))') time%mo, time%yrc, isub, snb_m(isub)
            end if 
          end if
          if (pco%ls_sub == 2) then
            write (4202,100) time%mo, time%yrc, isub, sls_m(isub)
            if (pco%csvout == 1 .and. pco%ls_sub == 2) then 
              write (4035,'(*(G0.3,:","))') time%mo, time%yrc, isub, sls_m(isub)
            end if 
          end if
          if (pco%pw_sub == 2) then
            write (4203,100) time%mo, time%yrc, isub, spw_m(isub)
            if (pco%csvout == 1 .and. pco%pw_sub == 2) then 
              write (4037,'(*(G0.3,:","))') time%mo, time%yrc, isub, spw_m(isub)
            end if 
          end if
  
          swb_m(isub) = hwbz
          snb_m(isub) = hnbz
          sls_m(isub) = hlsz
          spw_m(isub) = hpwz
        end if

!!!!! yearly print - SUBBASIN
        if (time%end_yr == 1) then
           spw_y(isub) = spw_y(isub) // 12.
           swb_y(isub)%cn = swb_y(isub)%cn / 12. 
           swb_y(isub)%sw = swb_y(isub)%sw / 12.
           swb_a(isub) = swb_a(isub) + swb_y(isub)
           snb_a(isub) = snb_a(isub) + snb_y(isub)
           sls_a(isub) = sls_a(isub) + sls_y(isub)
           spw_a(isub) = spw_a(isub) + spw_y(isub)
           if (pco%wb_sub == 1) then
             write (4200,102) '     0', time%yrc, isub, swb_y(isub)
             if (pco%csvout == 1 .and. pco%wb_sub == 1) then 
               write (4031,'(*(G0.3,:","))') '     0', time%yrc, isub, swb_y(isub)
             end if 
           end if
           if (pco%nb_sub == 1) then
             write (4201,102) '     0', time%yrc, isub, snb_y(isub)
             if (pco%csvout == 1 .and. pco%nb_sub == 1) then 
               write (4033,'(*(G0.3,:","))') '     0', time%yrc, isub, snb_y(isub)
             end if 
           end if
           if (pco%ls_sub == 1) then
             write (4202,102) '     0', time%yrc, isub, sls_y(isub)
             if (pco%csvout == 1 .and. pco%ls_sub == 1) then 
               write (4035,'(*(G0.3,:","))') '     0', time%yrc, isub, sls_y(isub)
             end if 
           end if
           if (pco%pw_sub == 1) then
             write (4203,102) '     0', time%yrc, isub, spw_y(isub)
             if (pco%csvout == 1 .and. pco%pw_sub == 1) then 
               write (4037,'(*(G0.3,:","))') '     0', time%yrc, isub, spw_y(isub)
             end if 
           end if
 
!!!!! zero yearly variables        
          swb_y(isub) = hwbz
          snb_y(isub) = hnbz
          sls_y(isub) = hlsz
          spw_y(isub) = hpwz
        end if
        
!!!!! average annual print - SUBBASIN
      if (time%end_sim == 1) then
        swb_a(isub) = swb_a(isub) / time%yrs_prt
        write (4204,102) '     0', time%yrs, isub, swb_a(isub)
        if (pco%csvout == 1) then 
          write (4032,'(*(G0.3,:","))') '     0', time%yrs, isub, swb_a(isub)
        end if 
      end if
      if (time%end_sim == 1) then
        snb_a(isub) = snb_a(isub) / time%yrs_prt
        write (4205,102) '     0', time%yrs, isub, snb_a(isub)
        if (pco%csvout == 1) then 
          write (4034,'(*(G0.3,:","))') '     0', time%yrs, isub, snb_a(isub)
        end if
      end if
      if (time%end_sim == 1) then     
        sls_a(isub) = sls_a(isub) / time%yrs_prt
        write (4206,102) '     0', time%yrs, isub, sls_a(isub)
        if (pco%csvout == 1) then 
          write (4036,'(*(G0.3,:","))') '     0', time%yrs, isub, sls_a(isub)
        end if 
      end if
      if (time%end_sim == 1) then    
        spw_a(isub) = spw_a(isub) / time%yrs_prt
        write (4207,102) '     0', time%yrs, isub, spw_a(isub) 
      end if
      
      return
      
100   format (2i6,i8,18f12.3)
102   format (a,i6,i8,18f12.3)
       
      end subroutine subbasin_output