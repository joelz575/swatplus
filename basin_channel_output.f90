      subroutine basin_channel_output
      
      use time_module
      use basin_module
      use jrw_datalib_module
      use sd_channel_module
             
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
        if (pco%chan_bsn == 'day') then
          write (4404,100) time%day, time%yrc, jrch, bchsd_d
          if (pco%csvout == 'yes') then
            write (4405,'(*(g0.3,:","))') time%day, time%yrc, jrch, bchsd_d
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        bch_y = bch_y + bch_m
        if (pco%chan_bsn == 'mon') then
          write (4404,100) time%mo, time%yrc, jrch, bchsd_m
          if (pco%csvout == 'yes') then
            write (4405,'(*(g0.3,:","))') time%mo, time%yrc, jrch, bchsd_m
          end if
        end if
        bch_m = chz
      end if

!!!!! yearly print
      if (time%end_yr == 'year') then
        bch_a = bch_a + bch_y
        if (pco%chan_bsn == 'year') then 
          write (4404,100) time%day, time%yrs, jrch, bchsd_y
          if (pco%csvout == 'yes') then
            write (4405,'(*(g0.3,:","))') time%day, time%yrs, jrch, bchsd_y
          end if
        end if
        
        bch_y = chz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%chan_bsn /= 'null') then
        bch_a = bch_a / time%yrs_prt
        write (4406,100) time%day, time%yrs, jrch, bchsd_a
        if (pco%csvout == 'yes') then
          write (4407,'(*(g0.3,:","))') time%day, time%yrs, jrch, bchsd_a
        end if
      end if

100   format (2i6,i8,60(1x,e15.4))
      return
      
      end subroutine basin_channel_output