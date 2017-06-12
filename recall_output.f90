      subroutine recall_output
      
      use time_module
      use basin_module
      use hydrograph_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 'avann' = average annual (always print)
!!                 'year'  = yearly
!!                 'mon'   = monthly
!!                 'day'   = daily
     
        !! sum monthly variables
        do irec = 1, sp_ob%recall
        rec_m(irec) = rec_m(irec) + rec_d(irec)
        
        !! daily print - RECALL
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%recall%d == 'y') then
            write (4500,100) time%day, time%yrc, irec, rec_d(irec)
            if (pco%csvout == 'y') then
              write (4502,'(*(G0.3,:","))') time%day, time%yrc, irec, rec_d(irec)
            end if
          end if
        end if

        !! monthly print - RECALL
        if (time%end_mo == 1) then
          if (pco%recall%m == 'y') then
            write (4500,100) time%mo, time%yrc, irec, rec_m(irec)
            if (pco%csvout == 'y') then
              write (4502,'(*(G0.3,:","))') time%mo, time%yrc, irec, rec_m(irec)
            endif
          end if
          rec_m(irec) = hz
        end if

        !! yearly print - RECALL
        if (time%end_yr == 1) then
          if (pco%recall%y == 'y') then
            write (4500,102) '     0', time%yrc, irec, rec_y(irec)
            if (pco%csvout == 'y') then
              write (4502,'(*(G0.3,:","))') '     0', time%yrc, irec, rec_y(irec) 
            end if
          end if
          !! zero yearly variables        
          rec_y(irec) = hz
        end if
        
      !! average annual print - RECALL
      if (time%end_sim == 1 .and. pco%recall%a == 'y') then
        rec_a(irec) = rec_a(irec) / time%yrs_prt
        write (4501,102) '     0', time%yrs, irec, rec_a(irec)
        if (pco%csvout == 'y') then 
          write (4503,'(*(G0.3,:","))') '     0', time%yrs, irec, rec_a(irec)  
        end if 
      end if
      
      end do        ! sp_ob%recall
        
      return
      
100   format (2i6,i8,25f15.3)
102   format (a6,i6,i8,25f15.3)
       
      end subroutine recall_output