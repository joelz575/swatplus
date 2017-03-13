      subroutine basin_aquifer_output
      
      use time_module
      use basin_module
      use aquifer_module
      use jrw_datalib_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 'avann' = average annual (always print)
!!                 'year'  = yearly
!!                 'mon'   = monthly
!!                 'day'   = daily
     
        !! sum monthly variables

        baqu_d = aquz
        
        do iaq = 1, sp_ob%aqu
          const = 1. / acu_elem(iaq)%bsn_frac
          baqu_d = baqu_d + aqu(iaq) / const
        end do
        
        baqu_m = baqu_m + baqu_d
        
        !! daily print - AQUIFER
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%aqu_bsn == 'day') then
            write (4504,100) time%day, time%yrc, iaq, baqu_d
            if (pco%csvout == 'yes') then
              write (4505,'(*(G0.3,:","))') time%day, time%yrc, iaq, baqu_d
            end if
          end if
        end if

        !! monthly print - AQUIFER
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          baqu_m%stor = baqu_m%stor / const 
          baqu_m%hgt = baqu_m%hgt / const
          baqu_m%no3 = baqu_m%no3 / const
          baqu_y = baqu_y + baqu_m
          if (pco%aqu_bsn == 'mon') then
            write (4504,100) time%mo, time%yrc, iaq, baqu_m
            if (pco%csvout == 'yes') then
              write (4505,'(*(G0.3,:","))') time%mo, time%yrc, iaq, baqu_m
            endif
          end if
          baqu_m = aquz
        end if

        !! yearly print - AQUIFER
        if (time%end_yr == 1) then
          baqu_y%stor = baqu_y%stor / 12.
          baqu_y%hgt = baqu_y%hgt / 12.
          baqu_y%no3 = baqu_y%no3 / 12.
          baqu_a = baqu_a + baqu_y
          if (pco%aqu_bsn == 'year') then
            write (4504,102) '     0', time%yrc, iaq, baqu_y
            if (pco%csvout == 'yes') then
              write (4505,'(*(G0.3,:","))') '     0', time%yrc, iaq, baqu_y 
            end if
          end if
          !! zero yearly variables        
          baqu_y = aquz
        end if
        
      !! average annual print - AQUIFER
      if (time%end_sim == 1 .and. pco%aqu_bsn /= 'null') then
        baqu_a = baqu_a / time%yrs_prt
        write (4506,102) '     0', time%yrs, iaq, baqu_a
        if (pco%csvout == 'yes') then 
          write (4507,'(*(G0.3,:","))') '     0', time%yrs, iaq, baqu_a 
        end if 
      end if
      
      return
      
100   format (2i6,i8,17f15.3)
102   format (a6,i6,i8,17f15.3)
       
      end subroutine basin_aquifer_output