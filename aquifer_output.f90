      subroutine aquifer_output(iaq)
      
      use time_module
      use basin_module
      use aquifer_module
      !use aquifer_module, only : aqu, aqu_m, aqu_y
      
      integer, intent (in) :: iaq
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
     
        !! sum monthly variables
        !iaq = ob(icmd)%num 
        aqu_m(iaq) = aqu_m(iaq) + aqu(iaq)
        
        !! daily print - AQUIFER
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%aqu%d == 'y') then
            write (2520,100) time%day, time%yrc, iaq, aqu(iaq)
            if (pco%csvout == 'y') then
              write (2524,'(*(G0.3,:","))') time%day, time%yrc, iaq, aqu(iaq)
            end if
          end if
        end if

        !! monthly print - AQUIFER
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          aqu_m(iaq)%stor = aqu_m(iaq)%stor / const 
          aqu_m(iaq)%hgt = aqu_m(iaq)%hgt / const
          aqu_m(iaq)%no3 = aqu_m(iaq)%no3 / const
          aqu_y(iaq) = aqu_y(iaq) + aqu_m(iaq)
          if (pco%aqu%m == 'y') then
            write (2521,100) time%mo, time%yrc, iaq, aqu_m(iaq)
            if (pco%csvout == 'y') then
              write (2525,'(*(G0.3,:","))') time%mo, time%yrc, iaq, aqu_m(iaq)
            endif
          end if
          aqu_m(iaq) = aquz
        end if

        !! yearly print - AQUIFER
        if (time%end_yr == 1) then
          aqu_y(iaq)%stor = aqu_y(iaq)%stor / 12.
          aqu_y(iaq)%hgt = aqu_y(iaq)%hgt / 12.
          aqu_y(iaq)%no3 = aqu_y(iaq)%no3 / 12.
          aqu_a(iaq) = aqu_a(iaq) + aqu_y(iaq)
          if (pco%aqu%y == 'y') then
            write (2522,102) '     0', time%yrc, iaq, aqu_y(iaq)
            if (pco%csvout == 'y') then
              write (2526,'(*(G0.3,:","))') '     0', time%yrc, iaq, aqu_y(iaq) 
            end if
          end if
          !! zero yearly variables        
          aqu_y(iaq) = aquz
        end if
        
      !! average annual print - AQUIFER
      if (time%end_sim == 1 .and. pco%aqu%a == 'y') then
        aqu_a(iaq) = aqu_a(iaq) / time%yrs_prt
        write (2523,102) '     0', time%yrs, iaq, aqu_a(iaq)
        if (pco%csvout == 'y') then 
          write (2527,'(*(G0.3,:","))') '     0', time%yrs, iaq, aqu_a(iaq)  
        end if 
      end if
      
      return
      
100   format (2i6,i8,17f15.3)
102   format (a6,i6,i8,17f15.3)
       
      end subroutine aquifer_output