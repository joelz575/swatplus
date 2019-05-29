      subroutine basin_sdchannel_output
      
      use time_module
      use basin_module
      use channel_module
      use hydrograph_module
      use water_body_module
      
      implicit none
             
      integer :: ichan      !none       |counter
      integer :: brch       !           |
      real :: const         !           |

      bch_stor_d = chaz
      bch_in_d = chaz
      bch_out_d = chaz
      bch_wat_d = wbodz
      brch = 1

      !! sum all channel output
      do ichan = 1, sp_ob%chan
        bch_stor_d = bch_stor_d + ch_stor(ichan)
        bch_in_d = bch_in_d + ch_in_d(ichan)
        bch_out_d = bch_out_d + ch_out_d(ichan)
        bch_wat_d = bch_wat_d + ch_wat_d(ichan)
      end do

      bch_stor_m = bch_stor_m + bch_stor_d
      bch_in_m = bch_in_m + bch_in_d
      bch_out_m = bch_out_m + bch_out_d
      bch_wat_m = bch_wat_m + bch_wat_d

       !! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%chan_bsn%d == "y") then
          write (2110,100) time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_d, bch_stor_d, bch_in_d, bch_out_d
          if (pco%csvout == "y") then
            write (2114,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_d, bch_stor_d, bch_in_d, bch_out_d
          end if 
        end if 
      end if

      !! monthly print
      if (time%end_mo == 1) then
        bch_stor_y = bch_stor_y + bch_stor_m
        bch_in_y = bch_in_y + bch_in_m
        bch_out_y = bch_out_y + bch_out_m
        bch_wat_y = bch_wat_y + bch_wat_m
        
        const = float (ndays(time%mo + 1) - ndays(time%mo))
        bch_stor_m = bch_stor_m / const           !! all storage variables are averages
        bch_wat_m = bch_wat_m // const            !! // only divides area (daily average values)

        if (pco%chan_bsn%m == "y") then
          write (2111,100) time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_m, bch_stor_m, bch_in_m, bch_out_m
          if (pco%csvout == "y") then
            write (2115,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_m, bch_stor_m, bch_in_m, bch_out_m
          end if
        end if
        bch_stor_m = chaz
        bch_in_m = chaz
        bch_out_m = chaz
        bch_wat_m = wbodz
      end if

      !! yearly print
      if (time%end_yr == 1) then
        bch_stor_a = bch_stor_a + bch_stor_y
        bch_in_a = bch_in_a + bch_in_y
        bch_out_a = bch_out_a + bch_out_y
        bch_wat_a = bch_wat_a + bch_wat_y
        
        const = time%day_end_yr
        bch_stor_y = bch_stor_y / const     !! all storage variables are averages
        bch_wat_y = bch_wat_y // const      !! // only divides area (daily average values)

        if (pco%chan_bsn%y == "y") then 
          write (2112,100) time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_y, bch_stor_y, bch_in_y, bch_out_y
          if (pco%csvout == "y") then
            write (2116,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_y, bch_stor_y, bch_in_y, bch_out_y
          end if
        end if
        bch_stor_y = chaz
        bch_in_y = chaz
        bch_out_y = chaz
        bch_wat_y = wbodz
      end if

      !! average annual print
      if (time%end_sim == 1 .and. pco%chan_bsn%a == "y") then
        bch_stor_a = bch_stor_a / time%yrs_prt      !! all storage variables (averaged) must be divided by years
        bch_in_a = bch_in_a / time%yrs_prt          !! all inflow and outflow varaibles (summed) are divided by years
        bch_out_a = bch_out_a / time%yrs_prt
        bch_wat_a = bch_wat_a / time%yrs_prt        !! all summed variables divided by years
        bch_wat_a = bch_wat_a // time%yrs_prt       !! all averaged variables divided by years

        write (2113,100) time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_a, bch_stor_a, bch_in_a, bch_out_a
        if (pco%csvout == "y") then
          write (2117,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, brch, "     1", bsn%name, bch_wat_a, bch_stor_a, bch_in_a, bch_out_a
        end if
      end if

 100   format (4i6,i8,2x,a,2x,a17,f14.4,59(1x,e14.4))
      return
      
      end subroutine basin_sdchannel_output