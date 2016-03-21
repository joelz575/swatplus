      subroutine hydin_output (iin, surlat)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs hyd variables on daily, monthly and annual time steps
      
      !!  0 = average annual (always print)
      !!  1 = yearly
      !!  2 = monthly
      !!  3 = daily  
             
      character (len=3) :: surlat
      integer :: iin  !1==surf 2==lateral
      
!!!!! daily print
      if (pco%hyd == 3) then
          write (5004,101) time%day, time%yrs, icmd, ob(icmd)%typ,            &
             ob(icmd)%props, surlat, ht1
            if (pco%csvout == 1 .and. pco%hyd == 3) then
              write (5008,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,     &
               ob(icmd)%props, surlat, ht1
            end if       
      endif
        ob(icmd)%hin_m(iin) = ob(icmd)%hin_m(iin) + ht1

!!!!! monthly print
      if (time%end_mo == 1) then
        if (pco%hyd == 2) then
            write (5004,101) time%day, time%yrs, icmd, ob(icmd)%typ,      &
             ob(icmd)%props, surlat, ob(icmd)%hin_m(iin)
              if (pco%csvout == 1 .and. pco%hyd == 2) then
                write (5008,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,      &
                 ob(icmd)%props, surlat, ob(icmd)%hin_m(iin)
              end if
        end if
          ob(icmd)%hin_y(iin) = ob(icmd)%hin_y(iin)+ ob(icmd)%hin_m(iin)
          ob(icmd)%hin_m(iin) = hz
      endif
        
!!!!! yearly print
      if (time%end_yr == 1) then
        if (pco%hyd == 1) then
            write (5004,101) time%day, time%yrs, icmd, ob(icmd)%typ,     & 
             ob(icmd)%props, surlat, ob(icmd)%hin_y(iin)
          if (pco%csvout == 1 .and. pco%hyd == 1) then
             write (5008,'(*(G0.3,:","))') time%day, time%yrs, icmd, ob(icmd)%typ,     & 
               ob(icmd)%props, surlat, ob(icmd)%hin_y(iin)
          endif
        end if
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin)+ ob(icmd)%hin_y(iin)
          ob(icmd)%hin_y(iin) = hz
      endif
        
!!!!! average annual print
        if (time%end_sim == 1 .and. pco%hyd == 0) then
          ob(icmd)%hin_a(iin) = ob(icmd)%hin_a(iin) / time%yrs_prt
          write (5004,100) ob(icmd)%name, time%day, time%yrs, icmd,     & 
             ob(icmd)%typ, ob(icmd)%props, surlat, ht1
            if (pco%csvout == 1) then
              write (5008,'(*(G0.3,:","))') ob(icmd)%name, time%day, time%yrs, icmd,     & 
              ob(icmd)%typ, ob(icmd)%props, surlat, ht1 
            end if
        end if
        
      return

100   format (a16,3i8,a8,i8,a6,a13,30(1x,e10.4))
101   format (3i8,a8,i8,a6,a13,30(1x,e10.4))
       
      end subroutine hydin_output