      subroutine aquifer_output
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily
     
        !! sum monthly variables
        aqu_m(iaq) = aqu_m(iaq) + aqu(iaq)
        
        !! daily print - AQUIFER
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
          if (pco%aqu == 3) then
            write (4500,100) time%day, time%yrc, iaq, aqu(iaq)
            if (pco%csvout == 1) then
              write (4502,'(*(G0.3,:","))') time%day, time%yrc, iaq, aqu(iaq)
            end if
          end if
        end if

        !! monthly print - AQUIFER
        if (time%end_mo == 1) then
          aqu_y(iaq) = aqu_y(iaq) + aqu_m(iaq)
          if (pco%aqu == 2) then
            write (4500,100) time%mo, time%yrc, iaq, aqu_m(iaq)
            if (pco%csvout == 1) then
              write (4502,'(*(G0.3,:","))') time%mo, time%yrc, iaq, aqu_m(iaq)
            endif
          end if
          aqu_m(iaq) = aquz
        end if

        !! yearly print - AQUIFER
        if (time%end_yr == 1) then
           aqu_a(iaq) = aqu_a(iaq) + aqu_y(iaq)
           if (pco%aqu == 1) then
             write (4500,102) '     0', time%yrc, iaq, aqu_y(iaq)
             if (pco%csvout == 1) then
               write (4502,'(*(G0.3,:","))') '     0', time%yrc, iaq, aqu_y(iaq) 
             end if
           end if
          !! zero yearly variables        
          aqu_y(iaq) = aquz
        end if
        
      !! average annual print - AQUIFER
      if (time%end_sim == 1 .or. pco%aqu == 0) then
        aqu_a(iaq) = aqu_a(iaq) / time%yrs_prt
        write (4501,102) '     0', time%yrs, iaq, aqu_a(iaq)
        if (pco%csvout == 1) then 
          write (4503,'(*(G0.3,:","))') '     0', time%yrs, iaq, aqu_a(iaq)  
        end if 
      end if
      
      return
      
100   format (2i6,i8,17f15.3)
102   format (a6,i6,i8,17f15.3)
       
      end subroutine aquifer_output