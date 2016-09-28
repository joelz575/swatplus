      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs reservoir output variables    

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                         .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%res == 3) then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, resd(j)
             if (pco%csvout == 1) then
               write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, resd(j) 
             end if
          end if 
        end if 
                                                    
        resm(j) = resm(j) + resd(j)
        resd(j) = resmz
        
!!!!! monthly print
        if (time%end_mo == 1) then
          resy(j) = resy(j) + resm(j)
          if (pco%res == 2) then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, resm(j)
              if (pco%csvout == 1) then
                write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, resm(j) 
              end if 
          end if
          resm(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          resa(j) = resa(j) + resy(j)
          if (pco%res == 1) then
            write (5002,100) time%day, time%yrs, j, res(j)%flo, resy(j)
              if (pco%csvout == 1) then
                write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, resy(j)
              end if
          end if
          resy(j) = resmz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res == 0) then
          resa(j) = resy(j) / time%yrs_prt
          write (5002,100) time%day, time%yrs, j, res(j)%flo, resa(j)
          if (pco%csvout == 1) then
            write (5006,'(*(G0.3,:","))') time%day, time%yrs, j, res(j)%flo, resa(j)
          end if 
        end if
        
      return
        
100   format (2i6,i8,46e10.3)
       
      end subroutine reservoir_output