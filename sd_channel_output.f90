      subroutine sd_channel_output (ich)
    
      use basin_module
      use time_module
      !use hydrograph_module
      
      integer, intent (in) :: ich
            
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs channel output variables 

      chsd_m(ich) = chsd_m(ich) + chsd_d(ich)
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
        if (pco%sd_chan%d == 'y') then
          write (2500,100) time%day, time%yrc, ich, chsd_d(ich)
           if (pco%csvout == 'y') then
             write (2504,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_d(ich)
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          chsd_y(ich) = chsd_y(ich) + chsd_m(ich)
          if (pco%sd_chan%m == 'y') then
          write (2501,100) time%day, time%yrc, ich, chsd_m(ich)
          if (pco%csvout == 'y') then
            write (2505,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_m(ich)
          end if
        end if
        chsd_m(ich) = chsdz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        chsd_a(ich) = chsd_a(ich) + chsd_y(ich)
        if (pco%sd_chan%y == 'y') then 
          write (2502,100) time%day, time%yrc, ich, chsd_y(ich)
          if (csvout == 1) then
           write (2506,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_y(ich)
          end if
        end if
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        yrs = float(time%nbyr)
        chsd_a(ich) = chsd_a(ich) / yrs
        if (pco%sd_chan%a == 'y') then
        write (2503,100) time%day, time%yrc, ich, chsd_a(ich)
        if (csvout == 1) then
          write (2507,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_a(ich)
        end if
       end if
     end if 
      
      return

100   format (2i6,i8,60(1x,e11.4))
       
      end subroutine sd_channel_output