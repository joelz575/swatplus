      subroutine hyddep_output
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  

!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
        if (pco%hyd == 3) then
            write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ,       &
              ob(icmd)%props, ht1
          if (pco%csvout == 1 .and. pco%hyd == 3) then
            write (5009,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,       &
              ob(icmd)%props, ht1
          end if 
        endif
      end if
                                                    
      ob(icmd)%hdep_m = ob(icmd)%hdep_m + ht1

!!!!! monthly print
      if (time%end_mo == 1) then
        if (pco%hyd == 2) then
            write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ,     &
             ob(icmd)%props, ob(icmd)%hdep_m
          if (pco%csvout == 1 .and. pco%hyd == 2) then
            write (5009,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,     &
             ob(icmd)%props, ob(icmd)%hdep_m
          end if
        end if
          ob(icmd)%hdep_y = ob(icmd)%hdep_y + ob(icmd)%hdep_m
          ob(icmd)%hdep_m = hz
      endif
        
!!!!! yearly print
      if (time%end_yr == 1) then
        if (pco%hyd == 1) then
            write (5005,101) time%day, time%yrs, icmd, ob(icmd)%typ,     &
             ob(icmd)%props, ob(icmd)%hin_y
          if (pco%csvout == 1 .and. pco%hyd == 1) then
            write (5009,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,     &
             ob(icmd)%props, ob(icmd)%hin_y
          end if 
        end if
          ob(icmd)%hdep_a = ob(icmd)%hdep_a + ob(icmd)%hdep_y
          ob(icmd)%hdep_y = hz
      endif
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd == 0) then
          ob(icmd)%hdep_a = ob(icmd)%hdep_a / time%yrs_prt
          write (5005,100) ob(icmd)%name, time%day, time%yrs, icmd,      &
             ob(icmd)%typ, ob(icmd)%props, ob(icmd)%hdep_a
           if (pco%csvout == 1) then
             write (5009,'(*(G0.3,:","))') ob(icmd)%name, time%day, time%yrs, icmd,      &
              ob(icmd)%typ, ob(icmd)%props, ob(icmd)%hdep_a
           end if 
        end if
        
      return

100   format (a16,3i8,a8,i8,a13,30(1x,e11.4))
101   format (3i8,a8,i8,a13,30(1x,e11.4))
       
      end subroutine hyddep_output