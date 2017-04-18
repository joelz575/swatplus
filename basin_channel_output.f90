      subroutine basin_channel_output
      
      use time_module
      use basin_module
      use jrw_datalib_module
      use channel_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs channel output variables

      bch_d = chz

      !! sum all channel output
      do ich = 1, sp_ob%chan
        bch_d = bch_d + ch_d(ich)
        ch_d(ich) = chz
      end do

      bch_m = bch_m + bch_d
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                            .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
        if (pco%chan_bsn%d == 'y') then
          write (2110,100) time%day, time%yrc, jrch, bch_d
          if (pco%csvout == 'y') then
            write (2114,'(*(g0.3,:","))') time%day, time%yrc, jrch, bch_d
          end if 
        end if 
      end if

!!!!! monthly print
      if (time%end_mo == 1) then
        bch_y = bch_y + bch_m
        if (pco%chan_bsn%m == 'y') then
          write (2111,100) time%mo, time%yrc, jrch, bch_m
          if (pco%csvout == 'y') then
            write (2115,'(*(g0.3,:","))') time%mo, time%yrc, jrch, bch_m
          end if
        end if
        bch_m = chz
      end if

!!!!! yearly print
      if (time%end_yr == 1) then
        bch_a = bch_a + bch_y
        if (pco%chan_bsn%y == 'y') then 
          write (2112,100) time%day, time%yrs, jrch, bch_y
          if (pco%csvout == 'y') then
            write (2116,'(*(g0.3,:","))') time%day, time%yrs, jrch, bch_y
          end if
        end if
        
        bch_y = chz
      end if

!!!!! average annual print
      if (time%end_sim == 1 .and. pco%chan_bsn%a == 'y') then
        bch_a = bch_a / time%yrs_prt
        write (2113,100) time%day, time%yrs, jrch, bch_a
        if (pco%csvout == 'y') then
          write (2117,'(*(g0.3,:","))') time%day, time%yrs, jrch, bch_a
        end if
      end if

100   format (2i6,i8,60(1x,e15.4))
      return
      
      end subroutine basin_channel_output