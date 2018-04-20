      subroutine hcsin_output
    
      use hydrograph_module
      use time_module
      use basin_module
      use constituent_mass_module

      implicit none
      
      integer :: iin              !none          |counter
      integer :: ipest            !none          |pesticide counter
           
      do icmd = 1, sp_ob%objs
        do iin = 1, ob(icmd)%rcv_tot
        !! daily print
        if (cs_db%num_pests > 0) then
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%hyd%d == 'y') then
            write (2560,*) time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_d(iin), &
             (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)          
              if (pco%csvout == 'y') then
                write (2564,'(*(G0.3,:","))') time%day, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin), &
                 ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_d(iin),         &
                 (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
              end if       
          endif
         end if
        end if
                                                    
        hcs1 = obcs(icmd)%hcs_m(iin)
        hcs2 = obcs(icmd)%hcs_d(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_m(iin) = hcs2
        obcs(icmd)%hcs_d(iin) = hin_csz

        !! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd%m == 'y') then
            write (2561,*) time%mo, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),         &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_m(iin), & 
             (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
              if (pco%csvout == 'y') then
                write (2565,'(*(G0.3,:","))') time%mo, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
                 ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_m(iin),  & 
                 (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
              end if
          end if
          ob(icmd)%hin_y(iin) = ob(icmd)%hin_y(iin)+ ob(icmd)%hin_m(iin)
          ob(icmd)%hin_m(iin) = hz
        endif
        
        hcs1 = obcs(icmd)%hcs_m(iin)
        hcs2 = obcs(icmd)%hcs_d(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_m(iin) = hcs2
        obcs(icmd)%hcs_d(iin) = hin_csz
        
        !! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd%y == 'y') then
            write (2562,*) time%yrc, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_y(iin), &
             (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == 'y') then
              write (2566,'(*(G0.3,:","))') time%yrc, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
               ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_y(iin),   &
               (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
            endif
          end if
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin)+ ob(icmd)%hin_y(iin)
          ob(icmd)%hin_y(iin) = hz
        endif

        hcs1 = obcs(icmd)%hcs_m(iin)
        hcs2 = obcs(icmd)%hcs_d(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_m(iin) = hcs2
        obcs(icmd)%hcs_d(iin) = hin_csz
        
        !! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == 'y') then
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin) / time%yrs_prt
          write (2563,*) time%yrc, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_a(iin), &
             (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
            if (pco%csvout == 'y') then
              write (2567,'(*(G0.3,:","))') time%yrc, time%yrs, ob(icmd)%typ, ob(icmd)%num, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%frac_in(iin), ob(icmd)%hin_a(iin),  &
             (obcs(icmd)%hin%pest(ipest), ipest = 1, cs_db%num_pests)
            end if
        end if

        hcs1 = obcs(icmd)%hcs_m(iin)
        hcs2 = obcs(icmd)%hcs_d(iin)
        call constit_hyd_add
        obcs(icmd)%hcs_m(iin) = hcs2
        obcs(icmd)%hcs_d(iin) = hin_csz
        
        end do
      end do
      end subroutine hcsin_output