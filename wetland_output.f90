      subroutine wetland_output(j)
      
      use time_module
      use basin_module
      use reservoir_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs reservoir output variables    

!!!!! daily print
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%res%d == 'y') then
            write (2548,100) time%day, time%yrs, j, wet_d(j)
             if (pco%csvout == 'y') then
               write (2552,'(*(G0.3,:","))') time%day, time%yrs, j, wet_d(j) 
             end if
          end if 
        end if 
                                                    
        wet_m(j) = wet_m(j) + wet_d(j)

!!!!! monthly print
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          wet_m(j)%vol = wet_m(j)%vol / const
          wet_m(j)%area_ha = wet_m(j)%area_ha / const
          wet_y(j) = wet_y(j) + wet_m(j)
          if (pco%res%m == 'y') then
            write (2549,100) time%day, time%yrs, j, wet_m(j)
              if (pco%csvout == 'y') then
                write (2553,'(*(G0.3,:","))') time%day, time%yrs, j, wet_m(j) 
              end if 
          end if
          wet_m(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          wet_y(j)%vol = wet_y(j)%vol / 12.
          wet_y(j)%area_ha = wet_y(j)%area_ha / 12.
          wet_a(j) = wet_a(j) + wet_y(j)
          if (pco%res%y == 'y') then
            write (2550,100) time%day, time%yrs, j, wet_y(j)
              if (pco%csvout == 'y') then
                write (2554,'(*(G0.3,:","))') time%day, time%yrs, j, wet_y(j)
              end if
          end if
          res_y(j) = resmz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res%a == 'y') then
          wet_a(j) = wet_y(j) / time%yrs_prt
          write (2551,100) time%day, time%yrs, j, wet_a(j)
          if (pco%csvout == 'y') then
            write (2555,'(*(G0.3,:","))') time%day, time%yrs, j, wet_a(j)
          end if 
        end if
        
      return
        
100   format (2i6,i8,46e10.3)
       
      end subroutine wetland_output