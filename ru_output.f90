      subroutine ru_output (iru)
      
      use time_module
      use basin_module
      use hydrograph_module
      
      integer, intent (in) :: iru
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs ROUTING UNIT variables on daily, monthly and annual time steps
    
        !! sum monthly variables
        ru_m(iru) = ru_m(iru) + ru_d(iru)
        
        !! daily print - ROUTING UNIT
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%ru%d == 'y') then
            write (2600,100) time%day, time%yrc, iru, ru_d(iru)
            if (pco%csvout == 'y') then
              write (2604,'(*(G0.3,:","))') time%day, time%yrc, iru, ru_d(iru)
            end if
          end if
        end if

        !! monthly print - ROUTING UNIT
        if (time%end_mo == 1) then
          ru_y(iru) = ru_y(iru) + ru_m(iru)
          if (pco%ru%m == 'y') then
            write (2601,100) time%mo, time%yrc, iru, ru_m(iru)
            if (pco%csvout == 'y') then
              write (2605,'(*(G0.3,:","))') time%mo, time%yrc, iru, ru_m(iru)
            endif
          end if
          ru_m(iru) = hz
        end if

        !! yearly print - ROUTING UNIT
        if (time%end_yr == 1) then
          ru_a(iru) = ru_a(iru) + ru_y(iru)
          if (pco%ru%y == 'y') then
            write (2602,102) '     0', time%yrc, iru, ru_y(iru)
            if (pco%csvout == 'y') then
              write (2606,'(*(G0.3,:","))') '     0', time%yrc, iru, ru_y(iru) 
            end if
          end if
          !! zero yearly variables        
          ru_y(iru) = hz
        end if
        
      !! average annual print - ROUTING UNIT
          if (time%end_sim == 1 .and. pco%ru%a == 'y') then
          ru_a(iru) = ru_a(iru) / time%yrs_prt
            write (2603,102) '     0', time%yrs, iru, ru_a(iru)
            if (pco%csvout == 'y') then 
              write (2607,'(*(G0.3,:","))') '     0', time%yrs, iru, ru_a(iru)  
            end if 
          end if

      return
      
100   format (2i6,i8,25f15.3)
102   format (a6,i6,i8,25f15.3)
       
      end subroutine ru_output