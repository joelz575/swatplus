      subroutine basin_reservoir_output
      
      use time_module
      use basin_module
      use reservoir_module
      use jrw_datalib_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 'avann' = average annual (always print)
!!                 'year'  = yearly
!!                 'mon'   = monthly
!!                 'day'   = daily
     
        !! zero daily variables
        bres_d = resmz
        
        !! sum all reservoirs to get basin totals
        do ires = 1, sp_ob%res
          bres_d = bres_d + res_d(ires)
          res_d(ires) = resmz
        end do
        
        !! for concentrations - use average for all reservoirs (not weighted by size)
        bres_d%sedcon = bres_d%sedcon / sp_ob%res
        bres_d%pstcon = bres_d%pstcon / sp_ob%res
        bres_d%spstcon = bres_d%spstcon / sp_ob%res
        bres_d%orgpc = bres_d%orgpc / sp_ob%res
        bres_d%solpc = bres_d%solpc / sp_ob%res
        bres_d%orgnc = bres_d%orgnc / sp_ob%res
        bres_d%no3c = bres_d%no3c / sp_ob%res
        bres_d%no2c = bres_d%no2c / sp_ob%res
        bres_d%nh3c = bres_d%nh3c / sp_ob%res
        
        !! sum monthly variables
        bres_m = bres_m + bres_d
        
        !! daily print - RESERVOIR
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%res_bsn%d == 'y') then
            write (2100,100) time%day, time%yrc, ires, bres_d
            if (pco%csvout == 'y') then
              write (2104,'(*(G0.3,:","))') time%day, time%yrc, ires, bres_d
            end if
          end if
        end if

        !! monthly print - RESERVOIR
        if (time%end_mo == 1) then
          bres_y = bres_y + bres_m
          if (pco%res_bsn%m == 'y') then
            write (2101,100) time%mo, time%yrc, ires, bres_m
            if (pco%csvout == 'y') then
              write (2105,'(*(G0.3,:","))') time%mo, time%yrc, ires, bres_m
            endif
          end if
          bres_m = resmz
        end if

        !! yearly print - RESERVOIR
        if (time%end_yr == 1) then
          bres_a = bres_a + bres_y
          if (pco%res_bsn%y == 'y') then
            write (2102,102) '     0', time%yrc, ires, bres_y
            if (pco%csvout == 'y') then
              write (2106,'(*(G0.3,:","))') '     0', time%yrc, ires, bres_y 
            end if
          end if
          !! zero yearly variables        
          bres_y = resmz
        end if
        
      !! average annual print - RESERVOIR
      if (time%end_sim == 1 .and. pco%res_bsn%a == 'y') then
        bres_a = bres_a / time%yrs_prt
        write (2103,103) '     0', time%yrs, ires, bres_a
        if (pco%csvout == 'y') then 
          write (2107,'(*(G0.3,:","))') '     0', time%yrs, ires, bres_a 
        end if 
      end if
      
      return
      
100   format (2i6,i8,43f15.3)
102   format (a6,i6,i8,43f15.3)
103   format (a6,i6,i8,43f15.3)
       
      end subroutine basin_reservoir_output