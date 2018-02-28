      subroutine basin_sdchannel_output
      
      use time_module
      use basin_module
      use jrw_datalib_module
      use sd_channel_module
      use hydrograph_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs channel output variables

      bchsd_d = chsdz

      !! sum all channel output
      do ich = 1, sp_ob%chandeg
        bchsd_d = bchsd_d + chsd_d(ich)
        chsd_d(ich) = chsdz
      end do

      bchsd_m = bchsd_m + bchsd_d
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                            .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
        if (pco%sd_chan_bsn%d == 'y') then
          write (2120,100) time%day, time%yrc, jrch, bchsd_d
          if (pco%csvout == 'y') then
            write (2124,'(*(g0.3,:","))') time%day, time%yrc, jrch, bchsd_d
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        bchsd_y = bchsd_y + bchsd_m
        if (pco%sd_chan_bsn%m == 'y') then
          write (2121,100) time%mo, time%yrc, jrch, bchsd_m
          if (pco%csvout == 'y') then
            write (2125,'(*(g0.3,:","))') time%mo, time%yrc, jrch, bchsd_m
          end if
        end if
        bchsd_m = chsdz
      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        bchsd_a = bchsd_a + bchsd_y
        if (pco%sd_chan_bsn%y == 'y') then 
          write (2122,100) time%day, time%yrs, jrch, bchsd_y
          if (pco%csvout == 'y') then
            write (2126,'(*(g0.3,:","))') time%day, time%yrs, jrch, bchsd_y
          end if
        end if
        
        bchsd_y = chsdz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%sd_chan_bsn%a == 'y') then
        bchsd_a = bchsd_a / time%yrs_prt
        write (2123,100) time%day, time%yrs, jrch, bchsd_a
        if (pco%csvout == 'y') then
          write (2127,'(*(g0.3,:","))') time%day, time%yrs, jrch, bchsd_a
        end if
      end if

100   format (2i6,i8,60(1x,e15.4))
      return
      
      end subroutine basin_sdchannel_output