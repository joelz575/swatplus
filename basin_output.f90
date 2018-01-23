      subroutine basin_output
      
      use time_module
      use hydrograph_module
      use jrw_datalib_module
      use output_landscape_module
      use basin_module
      
      integer :: iz=1
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs BASIN variables on daily, monthly and annual time steps

!!    PRINT CODES: 0 = average annual (always print)
!!                 1 = yearly
!!                 2 = monthly
!!                 3 = daily

        ! summing subbasin output for the basin
        do ihru = 1, sp_ob%hru
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            if (lsu_elem(iihru)%obtyp == 'hru') then
              const = 1. / lsu_elem(iihru)%bsn_frac    !only have / operator set up
              bwb_d = bwb_d + hwb_d(iihru) / const
              bnb_d = bnb_d + hnb_d(iihru) / const
              bls_d = bls_d + hls_d(iihru) / const
              bpw_d = bpw_d + hpw_d(iihru) / const
            end if
           end if
          end do
          
            ! or if it is not routed and not in a subbasin
        do ihru = 1, sp_ob%hru_lte
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            if (lsu_elem(iihru)%obtyp == 'hlt') then
              const = 1. / lsu_elem(iihru)%bsn_frac
              bwb_d = bwb_d + hltwb_d(iihru) / const
              bnb_d = bnb_d + hltnb_d(iihru) / const
              bls_d = bls_d + hltls_d(iihru) / const
              bpw_d = bpw_d + hltpw_d(iihru) / const
            end if 
          end if
        end do

        !! sum monthly variables
        bwb_m = bwb_m + bwb_d
        bnb_m = bnb_m + bnb_d
        bls_m = bls_m + bls_d
        bpw_m = bpw_m + bpw_d
        
!!!!! daily print - SUBBASIN
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                    .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%wb_bsn%d == 'y') then
            write (2050,100) time%day, time%yrc, iz, bwb_d  !! waterbal
            if (pco%csvout == 'y') then 
              write (2054,'(*(G0.3,:","))') time%day, time%yrc, iz, bwb_d  !! waterbal
            end if
          end if 
          if (pco%nb_bsn%d == 'y') then
            write (2060,100) time%day, time%yrc, iz, bnb_d  !! nutrient bal
            if (pco%csvout == 'y') then 
              write (2064,'(*(G0.3,:","))') time%day, time%yrc, iz, bnb_d  !! nutrient bal
            end if
          end if
          if (pco%ls_bsn%d == 'y') then
            write (2070,100) time%day, time%yrc, iz, bls_d  !! losses
            if (pco%csvout == 'y') then 
              write (2074,'(*(G0.3,:","))') time%day, time%yrc, iz, bls_d  !! losses
            end if 
          end if
          if (pco%pw_bsn%d == 'y') then
            write (2080,100) time%day, time%yrc, iz, bpw_d  !! plant weather
            if (pco%csvout == 'y') then 
              write (2084,'(*(G0.3,:","))') time%day, time%yrc, iz, bpw_d  !! plant weather
            end if
          end if
        end if

      !! zero daily basin outputs
      bwb_d = hwbz
      bnb_d = hnbz
      bls_d = hlsz
      bpw_d = hpwz
          
!!!!! monthly print - BASIN
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          bpw_m = bpw_m // const
          !bwb_m = bwb_m // const
          bwb_m%cn = bwb_m%cn / const 
          bwb_m%sw = bwb_m%sw / const
          bwb_y = bwb_y + bwb_m
          bnb_y = bnb_y + bnb_m
          bls_y = bls_y + bls_m
          bpw_y = bpw_y + bpw_m
          if (pco%wb_bsn%m == 'y') then
            write (2051,100) time%mo, time%yrc, iz, bwb_m
            if (pco%csvout == 'y') then 
              write (2055,'(*(G0.3,:","))') time%mo, time%yrc, iz, bwb_m
            end if          
          end if
          if (pco%nb_bsn%m == 'y') then 
            write (2061,100) time%mo, time%yrc, iz, bnb_m
            if (pco%csvout == 'y') then 
              write (2065,100) time%mo, time%yrc, iz, bnb_m
            end if 
          end if
          if (pco%ls_bsn%m == 'y') then
            write (2071,100) time%mo, time%yrc, iz, bls_m
            if (pco%csvout == 'y') then 
              write (2075,'(*(G0.3,:","))') time%mo, time%yrc, iz, bls_m
            end if 
          end if
          if (pco%pw_bsn%m == 'y') then
            write (2081,100) time%mo, time%yrc, iz, bpw_m
            if (pco%csvout == 'y') then 
              write (2085,'(*(G0.3,:","))') time%mo, time%yrc, iz, bpw_m
            end if 
          end if
  
          bwb_m = hwbz
          bnb_m = hnbz
          bls_m = hlsz
          bpw_m = hpwz
        end if

!!!!! yearly print - BASIN
        if (time%end_yr == 1) then
           bpw_y = bpw_y // 12.
           !bwb_y = bwb_y // 12.
           bwb_y%cn = bwb_y%cn / 12. 
           bwb_y%sw = bwb_y%sw / 12.
           bwb_a = bwb_a + bwb_y
           bnb_a = bnb_a + bnb_y
           bls_a = bls_a + bls_y
           bpw_a = bpw_a + bpw_y
           if (pco%wb_bsn%y == 'y') then
             write (2052,102) '     0', time%yrc, iz, bwb_y
             if (pco%csvout == 'y') then 
                write (2056,'(*(G0.3,:","))') '     0', time%yrc, iz, bwb_y
             end if 
           end if
           if (pco%nb_bsn%y == 'y') then
             write (2062,102) '     0', time%yrc, iz, bnb_y
             if (pco%csvout == 'y') then 
               write (2066,'(*(G0.3,:","))') '     0', time%yrc, iz, bnb_y
             end if
           end if
           if (pco%ls_bsn%y == 'y') then
             write (2072,102) '     0', time%yrc, iz, bls_y
             if (pco%csvout == 'y') then 
                write (2076,'(*(G0.3,:","))') '     0', time%yrc, iz, bls_y
             end if 
           end if
           if (pco%pw_bsn%y == 'y') then
             write (2082,102) '     0', time%yrc, iz, bpw_y
             if (pco%csvout == 'y') then 
               write (2086,'(*(G0.3,:","))') '     0', time%yrc, iz, bpw_y
             end if 
           end if
 
!!!!! zero yearly variables        
          bwb_y = hwbz
          bnb_y = hnbz
          bls_y = hlsz
          bpw_y = hpwz
        end if
        
!!!!! average annual print - BASIN
      if (time%end_sim == 1 .and. pco%wb_bsn%a == 'y') then
        bwb_a = bwb_a / time%yrs_prt
        write (2053,103) '     0', time%yrs, iz, bwb_a, cal_sim
        if (pco%csvout == 'y') then 
          write (2057,'(*(G0.3,:","))') '     0', time%yrs, iz, bwb_a, cal_sim
        end if
        bwb_a = hwbz
      end if
      if (time%end_sim == 1 .and. pco%nb_bsn%a == 'y') then
        bnb_a = bnb_a / time%yrs_prt
        write (2063,102) '     0', time%yrs, iz, bnb_a
        if (pco%csvout == 'y') then 
          write (2067,'(*(G0.3,:","))') '     0', time%yrs, iz, bnb_a
        end if 
        bnb_a = hnbz
      end if
      if (time%end_sim == 1 .and. pco%ls_bsn%a == 'y') then     
        bls_a = bls_a / time%yrs_prt
        write (2073,102) '     0', time%yrs, iz, bls_a
        if (pco%csvout == 'y') then 
          write (2077,'(*(G0.3,:","))') '     0', time%yrs, iz, bls_a
        end if 
        bls_a = hlsz
      end if
      if (time%end_sim == 1 .and. pco%pw_bsn%a == 'y') then     
        bpw_a = bpw_a / time%yrs_prt
        write (2083,102) '     0', time%yrs, iz, bpw_a
        if (pco%csvout == 'y') then 
          write (2087,'(*(G0.3,:","))') '     0', time%yrs, iz, bpw_a
        end if 
        bpw_a = hpwz
      end if
      
      return
100   format (2i6,i8,27f12.3)
102   format (a,i6,i8,27f12.3)
103   format (a,i6,i8,27f12.3,a)
       
      end subroutine basin_output