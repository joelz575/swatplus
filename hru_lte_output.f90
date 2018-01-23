      subroutine hru_lte_output (isd)

      use time_module
      use basin_module
      use output_landscape_module

      integer, intent (in) :: isd
        !! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                        .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%wb_sd%d == 'y') then
            write (2300,100) time%day, time%yrc, isd, hltwb_d(isd)  !! waterbal
              if (pco%csvout == 'y') then 
                write (2304,'(*(G0.3,:","))') time%day, time%yrc, isd, hltwb_d(isd)  !! waterbal
              end if 
          end if
!          if (pco%nb_sd%d == 'y') then
!            write (2420,100) time%day, time%yrc, isd, hltnb_d(isd)  !! nutrient bal
!             if (pco%csvout == 'y') then 
!               write (2424,'(*(G0.3,:","))') time%day, time%yrc, isd, hltnb_d(isd)  !! nutrient bal
!             end if 
!          end if
          if (pco%ls_sd%d == 'y') then
            write (2440,102) time%day, time%yrc, isd, hltls_d(isd)  !! losses
              if (pco%csvout == 'y') then 
                write (2444,'(*(G0.3,:","))') time%day, time%yrc, isd, hltls_d(isd)  !! losses
              endif 
          end if
          if (pco%pw_sd%d == 'y') then
            write (2460,101) time%day, time%yrc, isd, hltpw_d(isd)  !! plant weather 
              if (pco%csvout == 'y') then 
                write (2464,'(*(G0.3,:","))') time%day, time%yrc, isd, hltpw_d(isd)  !! plant weather 
              end if 
          end if
        end if

        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hltpw_m(isd) = hltpw_m(isd) // const
          hltwb_m(isd)%cn = hltwb_m(isd)%cn / const 
          hltwb_m(isd)%sw = hltwb_m(isd)%sw / const
          hltwb_y(isd) = hltwb_y(isd) + hltwb_m(isd)
          hltnb_y(isd) = hltnb_y(isd) + hltnb_m(isd)
          hltls_y(isd) = hltls_y(isd) + hltls_m(isd)
          hltpw_y(isd) = hltpw_y(isd) + hltpw_m(isd)
          
          !! monthly print
           if (pco%wb_sd%m == 'y') then
             write (2301,100) time%mo, time%yrc, isd, hltwb_m(isd)
               if (pco%csvout == 'y') then 
                 write (2305,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltwb_m(isd)
               end if 
           end if
!           if (pco%nb_sd%m == 'y') then
!             write (2421,100) time%mo, time%yrc, isd, hltnb_m(isd)
!             if (pco%csvout == 'y') then 
!               write (2425,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltnb_m(isd)
!             end if 
!           end if
           if (pco%ls_sd%m == 'y') then
             write (2441,102) time%mo, time%yrc, isd, hltls_m(isd)
               if (pco%csvout == 'y') then 
                 write (2445,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltls_m(isd)
               end if 
           end if
           if (pco%pw_sd%m == 'y') then
             write (2461,101) time%mo, time%yrc, isd, hltpw_m(isd)
               if (pco%csvout == 'y') then 
                 write (2465,'(*(G0.3,:","))') time%mo, time%yrc, isd, hltpw_m(isd)
               end if 
           end if
          
          hltwb_m(isd) = hwbz
          hltnb_m(isd) = hnbz
          hltpw_m(isd) = hpwz
          hltls_m(isd) = hlsz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hltpw_y(isd) = hltpw_y(isd) // 12.
          hltwb_y(isd)%cn = hltwb_y(isd)%cn / 12. 
          hltwb_y(isd)%sw = hltwb_y(isd)%sw / 12.
          hltwb_a(isd) = hltwb_a(isd) + hltwb_y(isd)
          hltnb_a(isd) = hltnb_a(isd) + hltnb_y(isd)
          hltls_a(isd) = hltls_a(isd) + hltls_y(isd)
          hltpw_a(isd) = hltpw_a(isd) + hltpw_y(isd)

          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_sd%y == 'y') then
             write (2302,100) time%end_yr, time%yrc, isd, hltwb_y(isd)
                if (pco%csvout == 'y') then 
                  write (2306,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltwb_y(isd)
                end if 
           end if
!           if (time%end_yr == 1 .and. pco%nb_sd%y == 'y') then
!             write (2422,100) time%end_yr, time%yrc, isd, hltnb_y(isd)
!             if (pco%csvout == 'y') then 
!               write (2426,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltnb_y(isd)
!             end if 
!           end if
           if (time%end_yr == 1 .and. pco%ls_sd%y == 'y') then
             write (2442,102) time%end_yr, time%yrc, isd, hltls_y(isd)
               if (pco%csvout == 'y') then 
                 write (2446,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltls_y(isd)
               end if 
           end if
           if (time%end_yr == 1 .and. pco%pw_sd%y == 'y') then
             write (2462,101) time%end_yr, time%yrc, isd, hltpw_y(isd)
              if (pco%csvout == 'y') then 
                write (2466,'(*(G0.3,:","))') time%end_yr, time%yrc, isd, hltpw_y(isd)
              end if 
           end if

        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_sd%a == 'y') then
           hltwb_a(isd) = hltwb_a(isd) / time%yrs_prt
           write (2303,100) time%end_yr, time%yrs, isd, hltwb_a(isd)
           if (pco%csvout == 'y') then 
             write (2307,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltwb_a(isd)
           end if
           hltwb_a(isd) = hwbz
         end if
        
!         if (time%end_sim == 1 .and. pco%nb_sd%a == 'y') then 
!           hltnb_a(isd) = hltnb_a(isd) / time%yrs_prt
!           write (2423,100) time%end_yr, time%yrs, isd, hltnb_a(isd)
!         if (pco%csvout == 'y') then 
!             write (2427,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltnb_a(isd)
!           end if
!         end if
!         hltnb_a(isd) = hnbz       
         
         if (time%end_sim == 1 .and. pco%ls_sd%a == 'y') then
           hltls_a(isd) = hltls_a(isd) / time%yrs_prt  
           write (2443,101) time%end_yr, time%yrs, isd, hltls_a(isd)
           if (pco%csvout == 'y') then 
             write (2447,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltls_a(isd)
           end if
         end if
         hltls_a(isd) = hlsz
        
         if (time%end_sim == 1 .and. pco%pw_sd%a == 'y') then   
           hltpw_a(isd) = hltpw_a(isd) / time%yrs_prt      
           write (2463,102) time%end_yr, time%yrs, isd, hltpw_a(isd)
           if (pco%csvout == 'y') then 
             write (2467,'(*(G0.3,:","))') time%end_yr, time%yrs, isd, hltpw_a(isd)
           end if
           hltpw_a(isd) = hpwz
         end if

      return
     
100   format (2i6,i8,27f12.3)
101   format (2i6,i8,21f12.3)
102   format (2i6,i8,21f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
 
      end subroutine hru_lte_output