      subroutine basin_sdchannel_output
      
      use time_module
      use basin_module
      use sd_channel_module
      use hydrograph_module
      
      implicit none
      
      integer :: iob
                  
      bchsd_d = chsdz

      !! sum all channel output
      do ich = 1, sp_ob%chandeg
        bchsd_d = bchsd_d + chsd_d(ich)
        chsd_d(ich) = chsdz
      end do
  
      bchsd_m = bchsd_m + bchsd_d
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%sd_chan_bsn%d == "y") then
          write (2120,100) time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_d
          if (pco%csvout == "y") then
            write (2124,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_d
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        bchsd_y = bchsd_y + bchsd_m
        if (pco%sd_chan_bsn%m == "y") then
          write (2121,100) time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_m
          if (pco%csvout == "y") then
            write (2125,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_m
          end if
        end if
        bchsd_m = chsdz
      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        bchsd_a = bchsd_a + bchsd_y
        if (pco%sd_chan_bsn%y == "y") then 
          write (2122,100) time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_y
          if (pco%csvout == "y") then
            write (2126,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_y
          end if
        end if
        
        bchsd_y = chsdz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%sd_chan_bsn%a == "y") then
        bchsd_a = bchsd_a / time%yrs_prt
        write (2123,100) time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_a
        if (pco%csvout == "y") then
          write (2127,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, jrch, "     1", bsn%name, bchsd_a
        end if
      end if

100   format (4i6,i8,2x,a,2x,a16,60(1x,e15.4))

      return
      
      end subroutine basin_sdchannel_output