      subroutine hydin_output (iin)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  
             
      character (len=3) :: surlat
      integer :: iin  !inflow hyd for the object (sequential - 1,2,...)
      
!!!!! daily print
      if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
        if (pco%hyd%d == 'y') then
            write (2560,*) time%day, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ht1
            
              if (pco%csvout == 'y') then
                write (2564,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ht1
              end if       
        endif
      end if
                                                    
      ob(icmd)%hin_m(iin) = ob(icmd)%hin_m(iin) + ht1

!!!!! monthly print
      if (time%end_mo == 1) then
        if (pco%hyd%m == 'y') then
            write (2561,*) time%mo, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ob(icmd)%hin_m(iin)
              if (pco%csvout == 'y') then
                write (2565,'(*(G0.3,:","))') time%mo, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ob(icmd)%hin_m(iin)
              end if
        end if
          ob(icmd)%hin_y(iin) = ob(icmd)%hin_y(iin)+ ob(icmd)%hin_m(iin)
          ob(icmd)%hin_m(iin) = hz
      endif
        
!!!!! yearly print
      if (time%end_yr == 1) then
        if (pco%hyd%y == 'y') then
            write (2562,*) time%yrc, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ob(icmd)%hin_y(iin)
          if (pco%csvout == 'y') then
             write (2566,'(*(G0.3,:","))') time%yrc, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ob(icmd)%hin_y(iin)
          endif
        end if
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin)+ ob(icmd)%hin_y(iin)
          ob(icmd)%hin_y(iin) = hz
      endif
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd%a == 'y') then
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin) / time%yrs_prt
          write (2563,*) time%yrc, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ht1
            if (pco%csvout == 'y') then
              write (2567,'(*(G0.3,:","))') time%yrc, time%yrs, icmd, ob(icmd)%typ, ob(icmd)%props, ob(icmd)%obtyp_in(iin),        &
             ob(icmd)%obtypno_in(iin), ob(icmd)%htyp_in(iin), ob(icmd)%obj_in(iin), ht1
            end if
        end if

      end subroutine hydin_output