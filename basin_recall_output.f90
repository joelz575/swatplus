      subroutine basin_recall_output
      
      use time_module
      use basin_module
      use hydrograph_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 'avann' = average annual (always print)
!!                 'year'  = yearly
!!                 'mon'   = monthly
!!                 'day'   = daily
     
        brec_d = hz
        !! sum monthly variables
        do irec = 1,sp_ob%recall
          brec_d = brec_d + rec_d(irec)
        end do
        
        brec_d = brec_d / bsn%area_tot_ha
        brec_m = brec_m + brec_d
        
        !! daily print - BASIN RECALL
        if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%recall_bsn%d == 'y') then
            write (8000,100) time%day, time%yrc, iaq, brec_d
            if (pco%csvout == 'y') then
              write (8002,'(*(G0.3,:","))') time%day, time%yrc, iaq, brec_d
            end if
          end if
        end if

        !! monthly print - BASIN RECALL
        if (time%end_mo == 1) then
          brec_y = brec_y + brec_m
          if (pco%recall_bsn%m == 'y') then
            write (8000,100) time%mo, time%yrc, iaq, brec_m
            if (pco%csvout == 'y') then
              write (8002,'(*(G0.3,:","))') time%mo, time%yrc, iaq, brec_m
            endif
          end if
          brec_m = hz
        end if

        !! yearly print - BASIN RECALL
        if (time%end_yr == 1) then
          brec_a = brec_a + brec_y
          if (pco%recall_bsn%y == 'y') then
            write (8000,102) '     0', time%yrc, iaq, brec_y
            if (pco%csvout == 'y') then
              write (8002,'(*(G0.3,:","))') '     0', time%yrc, iaq, brec_y 
            end if
          end if
          !! zero yearly variables        
          brec_y = hz
        end if
        
      !! average annual print - BASIN RECALL

      if (time%end_sim == 1 .and. pco%recall_bsn%a == 'y') then
        brec_a = brec_a / time%yrs_prt
        write (8001,102) '     0', time%yrs, iaq, brec_a
        if (pco%csvout == 'y') then 
          write (8003,'(*(G0.3,:","))') '     0', time%yrs, iaq, brec_a 
        end if 
      end if
      
      return
      
100   format (2i6,i8,25f15.3)
102   format (a6,i6,i8,25f15.3)
       
      end subroutine basin_recall_output