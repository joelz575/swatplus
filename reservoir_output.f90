      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs reservoir output variables    

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                         .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%res%d == 'y') then
            write (2540,100) time%day, time%yrs, j, res_d(j)
             if (pco%csvout == 'y') then
               write (2544,'(*(G0.3,:","))') time%day, time%yrs, j, res_d(j) 
             end if
          end if 
        end if 
                                                    
        res_m(j) = res_m(j) + res_d(j)

!!!!! monthly print
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          res_m(j)%vol = res_m(j)%vol / const
          res_m(j)%area_ha = res_m(j)%area_ha / const
          res_y(j) = res_y(j) + res_m(j)
          if (pco%res%m == 'y') then
            write (2541,100) time%day, time%yrs, j,  res_m(j)
              if (pco%csvout == 'y') then
                write (2545,'(*(G0.3,:","))') time%day, time%yrs, j, res_m(j) 
              end if 
          end if
          res_m(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          res_y(j)%vol = res_y(j)%vol / 12.
          res_y(j)%area_ha = res_y(j)%area_ha / 12.
          res_a(j) = res_a(j) + res_y(j)
          if (pco%res%y == 'y') then
            write (2542,100) time%day, time%yrs, j, res_y(j)
              if (pco%csvout == 'y') then
                write (2546,'(*(G0.3,:","))') time%day, time%yrs, j, res_y(j)
              end if
          end if
          res_y(j) = resmz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res%a == 'y') then
          res_a(j) = res_y(j) / time%yrs_prt
          write (2543,100) time%day, time%yrs, j,  res_a(j)
          if (pco%csvout == 'y') then
            write (2547,'(*(G0.3,:","))') time%day, time%yrs, j, res_a(j)
          end if 
        end if
        
      return
        
100   format (2i6,i8,46e10.3)
       
      end subroutine reservoir_output