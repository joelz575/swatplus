      subroutine sd_channel_output
      
      !use time_module
      use basin_module
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs channel output variables 

      chsd_m(ich) = chsd_m(ich) + chsd_d(ich)
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
        if (pco%chan == 3) then
          write (4600,100) time%day, time%yrc, ich, chsd_d(ich),            &
            sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
           if (pco%csvout == 1) then
             write (4602,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_d(ich),  &
             sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
           end if
        end if
      end if

!!!!! monthly print
        if (time%end_mo == 1) then
          chsd_y(ich) = chsd_y(ich) + chsd_m(ich)
          if (pco%chan == 2) then
          write (4600,100) time%day, time%yrc, ich, chsd_m(ich),                &
           sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
          if (pco%csvout == 1) then
            write (4602,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_m(ich), &
            sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len 
          end if
        end if
        chsd_m(ich) = chsdz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        chsd_a(ich) = chsd_a(ich) + chsd_y(ich)
        if (pco%chan == 1) then 
          write (4600,100) time%day, time%yrc, ich, chsd_y(ich),          &
          sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
          if (csvout == 1) then
           write (4602,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_y(ich),  &
            sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
          end if
        end if
        chsd_y(ich) = chsdz
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        yrs = float(time%nbyr)
        chsd_a(ich) = chsd_a(ich) / yrs
        write (4601,100) time%day, time%yrc, ich, chsd_a(ich),           &
          sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
        if (csvout == 1) then
          write (4603,'(*(g0.3,:","))') time%day, time%yrc, ich, chsd_a(ich),   &
           sd_ch(ich)%chw, sd_ch(ich)%chd, sd_ch(ich)%chs,sd_ch(ich)%hc_len
        end if
      end if
      
      return

100   format (2i6,i8,60(1x,e10.4))
       
      end subroutine sd_channel_output