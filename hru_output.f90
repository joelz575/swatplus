      subroutine hru_output(ihru)
    
      use hru_module, only : pcom, ipl
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      
      integer, intent (in) :: ihru
      integer :: idp
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru

        hwb_m(j) = hwb_m(j) + hwb_d(j)
        hnb_m(j) = hnb_m(j) + hnb_d(j)
        hls_m(j) = hls_m(j) + hls_d(j) 
        hpw_m(j) = hpw_m(j) + hpw_d(j)

      !! daily print
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == 'y') then
            write (2000,100) time%day, time%yrc, j, hwb_d(j)  !! waterbal
             if (pco%csvout == 'y') then
               write (4015,'(*(G0.3,:","))') time%day, time%yrc, j, hwb_d(j)  !! waterbal
             end if
          end if
          if (pco%nb_hru%d == 'y') then
            write (2020,104) time%day, time%yrc, j, hnb_d(j)  !! nutrient bal
              if (pco%csvout == 'y') then
                write (2024,'(*(G0.3,:","))') time%day, time%yrc, j, hnb_d(j)  !! nutrient bal
              end if
          end if
          if (pco%ls_hru%d == 'y') then
            write (2030,102) time%day, time%yrc, j, hls_d(j)  !! losses
              if (pco%csvout == 'y') then
                write (2034,'(*(G0.3,:","))') time%day, time%yrc, j, hls_d(j)  !! losses
              end if
          end if
          if (pco%pw_hru%d == 'y') then
            write (2040,101) time%day, time%yrc, j, hpw_d(j)  !! plant weather 
              if (pco%csvout == 'y') then 
                write (2044,'(*(G0.3,:","))') time%day, time%yrc, j, hpw_d(j)  !! plant weather
              end if 
          end if
        end if

        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpw_m(j) = hpw_m(j) // const
          !hwb_m(j) = hwb_m(j) // const
          hwb_m(j)%cn = hwb_m(j)%cn / const
          hwb_m(j)%snopack = hwb_m(j)%snopack / const
          hwb_m(j)%sw = hwb_m(j)%sw / const
          hwb_y(j) = hwb_y(j) + hwb_m(j)
          hnb_y(j) = hnb_y(j) + hnb_m(j)
          hls_y(j) = hls_y(j) + hls_m(j)
          hpw_y(j) = hpw_y(j) + hpw_m(j)
          
          !! monthly print
           if (pco%wb_hru%m == 'y') then
             write (2001,100) time%mo, time%yrc, j, hwb_m(j)
               if (pco%csvout == 'y') then
                 write (2005,'(*(G0.3,:","))') time%mo, time%yrc, j, hwb_m(j)
               end if
           end if
           if (pco%nb_hru%m == 'y') then
             write (2021,104) time%mo, time%yrc, j, hnb_m(j)
               if (pco%csvout == 'y') then
                 write (2025,'(*(G0.3,:","))') time%mo, time%yrc, j, hnb_m(j)
               end if
           end if
           if (pco%ls_hru%m == 'y') then
             write (2031,102) time%mo, time%yrc, j, hls_m(j)
               if (pco%csvout == 'y') then 
                 write (2035,'(*(G0.3,:","))') time%mo, time%yrc, j, hls_m(j)
               end if
           end if
           if (pco%pw_hru%m == 'y') then
             write (2041,101) time%mo, time%yrc, j, hpw_m(j)
               if (pco%csvout == 'y') then 
                 write (2045,'(*(G0.3,:","))') time%mo, time%yrc, j, hpw_m(j)
               end if 
           end if
          
          hwb_m(j) = hwbz
          hnb_m(j) = hnbz
          hpw_m(j) = hpwz
          hls_m(j) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpw_y(j) = hpw_y(j) // 12.
          !hwb_y(j) = hwb_y(j) // 12.
          hwb_y(j)%cn = hwb_y(j)%cn / 12.
          hwb_y(j)%snopack = hwb_y(j)%snopack / 12.
          hwb_y(j)%sw = hwb_y(j)%sw / 12.
          hwb_a(j) = hwb_a(j) + hwb_y(j)
          hnb_a(j) = hnb_a(j) + hnb_y(j)
          hls_a(j) = hls_a(j) + hls_y(j)
          hpw_a(j) = hpw_a(j) + hpw_y(j)
          
          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_hru%y == 'y') then
             write (2002,100) time%end_yr, time%yrc, j, hwb_y(j)
               if (pco%csvout == 'y') then
                 write (2006,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hwb_y(j)
               end if
           end if
           if (time%end_yr == 1 .and. pco%nb_hru%y == 'y') then
             write (2022,104) time%end_yr, time%yrc, j, hnb_y(j)
               if (pco%csvout == 'y') then
                 write (2026,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hnb_y(j)
               end if
           end if
           if (time%end_yr == 1 .and. pco%ls_hru%y == 'y') then
             write (2032,102) time%end_yr, time%yrc, j, hls_y(j)
               if (pco%csvout == 'y') then
                 write (2036,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hls_y(j)
               end if
           end if
           if (time%end_yr == 1 .and. pco%pw_hru%y == 'y') then
             write (2042,101) time%end_yr, time%yrc, j, hpw_y(j)
               if (pco%csvout == 'y') then 
                 write (2046,'(*(G0.3,:","))') time%end_yr, time%yrc, j, hpw_y(j)
               end if 
           end if
          
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_hru%a == 'y') then
           hwb_a(j) = hwb_a(j) / time%yrs_prt
           write (2003,100) time%end_yr, time%yrs, j, hwb_a(j)
           if (pco%csvout == 'y') then
             write (2007,100) time%end_yr, time%yrs, j, hwb_a(j)
           end if
           hwb_a(j) = hwbz
         end if
        
         if (time%end_sim == 1 .and. pco%nb_hru%a == 'y') then 
           hnb_a(j) = hnb_a(j) / time%yrs_prt
           write (2023,104) time%end_yr, time%yrs, j, hnb_a(j)
             if (pco%csvout == 'y') then 
               write (2027,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hnb_a(j)
             end if
             hnb_a(j) = hnbz
         end if
        
         if (time%end_sim == 1 .and. pco%ls_hru%a == 'y') then
           hls_a(j) = hls_a(j) / time%yrs_prt 
           write (2033,101) time%end_yr, time%yrs, j, hls_a(j)
             if (pco%csvout == 'y') then 
               write (2037,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hls_a(j)
             end if
             hls_a(j) = hlsz
         end if
        
         if (time%end_sim == 1 .and. pco%pw_hru%a == 'y') then     
           hpw_a(j) = hpw_a(j) / time%yrs_prt      
           write (2043,102) time%end_yr, time%yrs, j, hpw_a(j)
             if (pco%csvout == 'y') then 
               write (2047,'(*(G0.3,:","))') time%end_yr, time%yrs, j, hpw_a(j)
             end if
             hpw_a(j) = hpwz
         end if

         if (time%end_sim == 1) then
           do ipl = 1, pcom(j)%npl
             idp = pcom(j)%plcur(ipl)%idplt
             if (pcom(j)%plcur(ipl)%harv_num > 0) then 
               pcom(j)%plcur(ipl)%yield = pcom(j)%plcur(ipl)%yield /           &
                                         pcom(j)%plcur(ipl)%harv_num
             endif
            write (4008,103) time%end_yr, time%yrs, j,pldb(idp)%plantnm,   &
                                                 pcom(j)%plcur(ipl)%yield
            if (pco%csvout == 'y') then
              write (4009,'(*(G0.3,:","))') time%end_yr, time%yrs, j,pldb(idp)%plantnm,   &
                                                 pcom(j)%plcur(ipl)%yield 
            end if
           end do
         end if
      return
      
100   format (2i6,i8,27f12.3)
101   format (2i6,i8,20f12.3)
102   format (2i6,i8,20f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
104   format (2i6,i8,27f18.3)
       
      end subroutine hru_output