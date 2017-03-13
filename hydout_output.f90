      subroutine hydout_output (iout)
    
      use time_module
      use basin_module

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  

!!!!! daily print
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                              .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%hyd == 'day') then
            write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ,        &
             ob(icmd)%props, ob(icmd)%obtyp_out(iout),                      &
             ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),           &
             ob(icmd)%obj_out(iout), ht1
            if (pco%csvout == 'yes' .and. pco%hyd == 'day') then
              write (5007,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,   &
               ob(icmd)%props, ob(icmd)%obtyp_out(iout),                              &
               ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),                   &
               ob(icmd)%obj_out(iout), ht1  
            end if 
          end if
        endif
        ob(icmd)%hout_m(iout) = ob(icmd)%hout_m(iout) + ht1

!!!!! monthly print
        if (time%end_mo == 1) then
          if (pco%hyd == 'mon') then
            write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ,      & 
           ob(icmd)%props, ob(icmd)%obtyp_out(iout),                      &
           ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),           &
           ob(icmd)%obj_out(iout), ob(icmd)%hout_m(iout)
            if (pco%csvout == 'yes' .and. pco%hyd == 'mon') then
              write (5007,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ, & 
             ob(icmd)%props, ob(icmd)%obtyp_out(iout),                              &
             ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),                   &
             ob(icmd)%obj_out(iout), ob(icmd)%hout_m(iout)
            end if
          end if
          ob(icmd)%hout_y(iout) = ob(icmd)%hout_y(iout) +                 &
                                             ob(icmd)%hout_m(iout)
          ob(icmd)%hout_m(iout) = hz
        end if
        
!!!!! yearly print
        if (time%end_yr == 1) then
          if (pco%hyd == 'year') then
            write (5001,101) time%day, time%yrs, icmd, ob(icmd)%typ,      &
           ob(icmd)%props, ob(icmd)%obtyp_out(iout),                      &
           ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),           &
           ob(icmd)%obj_out(iout), ob(icmd)%hout_y(iout)
             if (pco%csvout == 'yes' .and. pco%hyd == 'year') then
               write (5007,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,  &
               ob(icmd)%props, ob(icmd)%obtyp_out(iout),                              &
               ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),                   &
               ob(icmd)%obj_out(iout), ob(icmd)%hout_y(iout)
             end if
          end if
          ob(icmd)%hout_a(iout) = ob(icmd)%hout_a(iout) +                 &
                                                 ob(icmd)%hout_y(iout)
          ob(icmd)%hout_y(iout) = hz
        end if
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd /= 'null') then
          ob(icmd)%hout_a(iout) = ob(icmd)%hout_a(iout) / time%yrs_prt
          write (5001,100) ob(icmd)%name, time%day, time%yrs, icmd,       &
           ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_out(iout),        &
           ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),           &
           ob(icmd)%obj_out(iout), ob(icmd)%hout_a(iout)
            if (pco%csvout == 'yes') then
              write (5007,'(*(G0.3,:","))') ob(icmd)%name, time%day, time%yrs, icmd,    &
              ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_out(iout),                   &
              ob(icmd)%obtypno_out(iout), ob(icmd)%htyp_out(iout),                      &
              ob(icmd)%obj_out(iout), ob(icmd)%hout_a(iout)
            end if 
        end if
        
      return
        
100   format (a16,3i8,a8,i8,'    out ',2(a8,i8),a13,30(1x,e11.4))
101   format (3i8,a8,i8,'    out ',2(a8,i8),a13,30(1x,e11.4))
       
      end subroutine hydout_output