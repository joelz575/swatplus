      subroutine lsu_output
      
      use time_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use hydrograph_module
      use output_landscape_module
      
      implicit none
      
      integer :: isub          !none        |counter
      integer :: ielem         !none        |counter
      integer :: ihru          !none        |counter 
      integer :: iob           !none        |counter 
      real :: const            !            |  
                            
      do isub = 1, db_mx%lsu_out
        ! summing HRU output for the subbasin
        do ielem = 1, lsu_out(isub)%num_tot
          ihru = lsu_out(isub)%num(ielem)
          iob = sp_ob1%sub + isub - 1                          !!! double check for placement and 'isub'
          if (lsu_elem(ihru)%sub_frac > 1.e-9) then
            const = 1. / lsu_elem(ihru)%sub_frac   !only have / operator set up
            if (lsu_elem(ihru)%obtyp == 'hru') then
              ruwb_d(isub) = ruwb_d(isub) + hwb_d(ihru) / const
              runb_d(isub) = runb_d(isub) + hnb_d(ihru) / const
              ruls_d(isub) = ruls_d(isub) + hls_d(ihru) / const
              rupw_d(isub) = rupw_d(isub) + hpw_d(ihru) / const
            end if
            ! summing HRU_LTE output
            if (lsu_elem(ihru)%obtyp == 'hlt') then
              ruwb_d(isub) = ruwb_d(isub) + hltwb_d(ihru) / const
              runb_d(isub) = runb_d(isub) + hltnb_d(ihru) / const
              ruls_d(isub) = ruls_d(isub) + hltls_d(ihru) / const
              rupw_d(isub) = rupw_d(isub) + hltpw_d(ihru) / const
            end if
          end if
        end do    !ielem
      
        !! sum monthly variables
        ruwb_m(isub) = ruwb_m(isub) + ruwb_d(isub)
        runb_m(isub) = runb_m(isub) + runb_d(isub)
        ruls_m(isub) = ruls_m(isub) + ruls_d(isub)
        rupw_m(isub) = rupw_m(isub) + rupw_d(isub)
        
!!!!! daily print - SUBBASIN
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_sub%d == 'y') then
            write (2140,100) time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_d(isub)  !! waterbal
              if (pco%csvout == 'y') then 
                write (2144,'(*(G0.3,:","))') time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_d(isub)  !! waterbal
              end if 
          end if 
          if (pco%nb_sub%d == 'y') then
            write (2150,103) time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_d(isub)  !! nutrient bal
            if (pco%csvout == 'y') then 
              write (2154,'(*(G0.3,:","))') time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_d(isub)  !! nutrient bal
            end if 
          end if
          if (pco%ls_sub%d == 'y') then
            write (2160,100) time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_d(isub)  !! losses
            if (pco%csvout == 'y') then 
              write (2164,'(*(G0.3,:","))') time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_d(isub)  !! losses
            end if 
          end if
          if (pco%pw_sub%d == 'y') then
            write (2170,100) time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_d(isub)  !! plant weather
            if (pco%csvout == 'y') then 
              write (2175,'(*(G0.3,:","))') time%day, time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_d(isub)  !! plant weather 
            end if
          end if 
       end if

        ruwb_d(isub) = hwbz
        runb_d(isub) = hnbz
        ruls_d(isub) = hlsz
        rupw_d(isub) = hpwz
        
!!!!! monthly print - SUBBASIN
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo)) 
          rupw_m(isub) = rupw_m(isub) // const
          ruwb_m(isub)%cn = ruwb_m(isub)%cn / const 
          ruwb_m(isub)%sw = ruwb_m(isub)%sw / const
          ruwb_y(isub) = ruwb_y(isub) + ruwb_m(isub)
          runb_y(isub) = runb_y(isub) + runb_m(isub)
          ruls_y(isub) = ruls_y(isub) + ruls_m(isub)
          rupw_y(isub) = rupw_y(isub) + rupw_m(isub)
          
          if (pco%wb_sub%m == 'y') then
            write (2141,100) time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_m(isub)
            if (pco%csvout == 'y') then 
              write (2145,'(*(G0.3,:","))') time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_m(isub)
            end if 
          end if
          if (pco%nb_sub%m == 'y') then 
            write (2151,103) time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_m(isub)
            if (pco%csvout == 'y') then 
              write (2155,'(*(G0.3,:","))') time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_m(isub)
            end if 
          end if
          if (pco%ls_sub%m == 'y') then
            write (2161,100) time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_m(isub)
            if (pco%csvout == 'y') then 
              write (2165,'(*(G0.3,:","))') time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_m(isub)
            end if 
          end if
          if (pco%pw_sub%m == 'y') then
            write (2171,100) time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_m(isub)
            if (pco%csvout == 'y') then 
              write (2175,'(*(G0.3,:","))') time%mo, time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_m(isub)
            end if 
          end if
  
          ruwb_m(isub) = hwbz
          runb_m(isub) = hnbz
          ruls_m(isub) = hlsz
          rupw_m(isub) = hpwz
        end if

!!!!! yearly print - SUBBASIN
        if (time%end_yr == 1) then
           rupw_y(isub) = rupw_y(isub) // 12.
           ruwb_y(isub)%cn = ruwb_y(isub)%cn / 12. 
           ruwb_y(isub)%sw = ruwb_y(isub)%sw / 12.
           ruwb_a(isub) = ruwb_a(isub) + ruwb_y(isub)
           runb_a(isub) = runb_a(isub) + runb_y(isub)
           ruls_a(isub) = ruls_a(isub) + ruls_y(isub)
           rupw_a(isub) = rupw_a(isub) + rupw_y(isub)
           
           if (pco%wb_sub%y == 'y') then
             write (2142,102) '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_y(isub)
             if (pco%csvout == 'y') then 
               write (2146,'(*(G0.3,:","))') '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, ruwb_y(isub)
             end if 
           end if
           if (pco%nb_sub%y == 'y') then
             write (2152,104) '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_y(isub)
             if (pco%csvout == 'y') then 
               write (2156,'(*(G0.3,:","))') '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, runb_y(isub)
             end if 
           end if
           if (pco%ls_sub%y == 'y') then
             write (2162,102) '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_y(isub)
             if (pco%csvout == 'y') then 
               write (2166,'(*(G0.3,:","))') '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, ruls_y(isub)
             end if 
           end if
           if (pco%pw_sub%y == 'y') then
             write (2172,102) '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_y(isub)
             if (pco%csvout == 'y') then 
               write (2176,'(*(G0.3,:","))') '     0', time%yrc, isub, ob(iob)%num, ob(iob)%name, rupw_y(isub)
             end if 
           end if
 
!!!!! zero yearly variables        
          ruwb_y(isub) = hwbz
          runb_y(isub) = hnbz
          ruls_y(isub) = hlsz
          rupw_y(isub) = hpwz
        end if
        
!!!!! average annual print - SUBBASIN
      if (time%end_sim == 1 .and. pco%wb_sub%a == 'y') then
        ruwb_a(isub) = ruwb_a(isub) / time%yrs_prt
        write (2143,102) '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, ruwb_a(isub)
        if (pco%csvout == 'y') then 
          write (2147,'(*(G0.3,:","))') '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, ruwb_a(isub)
        end if 
      end if
      if (time%end_sim == 1 .and. pco%nb_sub%a == 'y') then
        runb_a(isub) = runb_a(isub) / time%yrs_prt
        write (2153,104) '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, runb_a(isub)
        if (pco%csvout == 'y') then 
          write (2157,'(*(G0.3,:","))') '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, runb_a(isub)
        end if
      end if
      if (time%end_sim == 1 .and. pco%ls_sub%a == 'y') then     
        ruls_a(isub) = ruls_a(isub) / time%yrs_prt
        write (2163,102) '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, ruls_a(isub)
        if (pco%csvout == 'y') then 
          write (2167,'(*(G0.3,:","))') '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, ruls_a(isub)
        end if 
      end if
      if (time%end_sim == 1 .and. pco%pw_sub%a == 'y') then    
        rupw_a(isub) = rupw_a(isub) / time%yrs_prt
        write (2173,102) '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, rupw_a(isub) 
        if (pco%csvout == 'y') then 
          write (2177,'(*(G0.3,:","))') '     0', time%yrs, isub, ob(iob)%num, ob(iob)%name, rupw_a(isub)
        end if
      end if
      end do    !isub
      
      return
      
100   format (2i6,2i8,2x,a,27f12.3)
102   format (a,i6,2i8,2x,a,27f12.3)
103   format (2i6,2i8,2x,a,27(6x,f12.3))
104   format (a,i6,2i8,2x,a,27(6x,f12.3))
       
      end subroutine lsu_output