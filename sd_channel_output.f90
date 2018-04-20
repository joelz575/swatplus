      subroutine sd_channel_output (ichan)
    
      use sd_channel_module
      use basin_module
      use time_module
      use hydrograph_module
      
      implicit none
      
      integer, intent (in) :: ichan         !             |
      integer :: iob                        !             |
      real :: csvout                        !none         |code to print .csv files n=no print; y=print; 
      real :: yrs                           !             |
       
      iob = sp_ob1%chandeg + ichan - 1

      chsd_m(ichan) = chsd_m(ichan) + chsd_d(ichan)
      
!!!!! daily print
       if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan%d == 'y') then
          write (2500,100) time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_d(ichan)
           if (pco%csvout == 'y') then
             write (2504,'(*(g0.3,:","))') time%day, time%yrc, ichan, ob(iob)%num, ob(iob)%name, chsd_d(ichan)
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          chsd_y(ichan) = chsd_y(ichan) + chsd_m(ichan)
          if (pco%sd_chan%m == 'y') then
          write (2501,100) time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_m(ichan)
          if (pco%csvout == 'y') then
            write (2505,'(*(g0.3,:","))') time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_m(ichan)
          end if
        end if
        chsd_m(ichan) = chsdz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        chsd_a(ichan) = chsd_a(ichan) + chsd_y(ichan)
        if (pco%sd_chan%y == 'y') then 
          write (2502,100) time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_y(ichan)
          if (csvout == 1) then
           write (2506,'(*(g0.3,:","))') time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_y(ichan)
          end if
        end if
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        yrs = float(time%nbyr)
        chsd_a(ichan) = chsd_a(ichan) / yrs
        if (pco%sd_chan%a == 'y') then
        write (2503,100) time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_a(ichan)
        if (csvout == 1) then
          write (2507,'(*(g0.3,:","))') time%day, time%yrc, ich, ob(iob)%num, ob(iob)%name, chsd_a(ichan)
        end if
       end if
     end if 
      
      return

100   format (2i6,2i8,2x,a,60e15.4)      
       
      end subroutine sd_channel_output