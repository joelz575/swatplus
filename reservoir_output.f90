      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs reservoir output variables    

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                         .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%res == 'day') then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, res_d(j)
             if (pco%csvout == 'yes') then
               write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, res_d(j) 
             end if
          end if 
        end if 
                                                    
        res_m(j) = res_m(j) + res_d(j)

!!!!! monthly print
        if (time%end_mo == 1) then
          res_y(j) = res_y(j) + res_m(j)
          if (pco%res == 'mon') then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, res_m(j)
              if (pco%csvout == 'yes') then
                write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, res_m(j) 
              end if 
          end if
          res_m(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          res_a(j) = res_a(j) + res_y(j)
          if (pco%res == 'year') then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, res_y(j)
              if (pco%csvout == 'yes') then
                write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, res_y(j)
              end if
          end if
          res_y(j) = resmz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res /= 'null') then
          res_a(j) = res_y(j) / time%yrs_prt
          write (7008,100) time%day, time%yrs, j, res(j)%flo, res_a(j)
          if (pco%csvout == 'yes') then
            write (7009,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, res_a(j)
          end if 
        end if
        
      return
        
100   format (2i6,i8,46e10.3)
       
      end subroutine reservoir_output