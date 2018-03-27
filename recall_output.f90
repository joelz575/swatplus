      subroutine recall_output (irec)
      
      use time_module
      use basin_module
      use hydrograph_module
      
      integer, intent (in) :: irec
      
      iob = sp_ob1%recall + irec - 1   !!!!!! added for new output write
             
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs SUBBASIN variables on daily, monthly and annual time steps
     
        !! sum monthly variables
        rec_m(irec) = rec_m(irec) + rec_d(irec)
        
        !! daily print - RECALL
         if (pco%day_print == 'y' .and. pco%int_day_cur == pco%int_day) then
          if (pco%recall%d == 'y') then
            write (4500,100) time%day, time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_d(irec)
            if (pco%csvout == 'y') then
              write (4504,'(*(G0.3,:","))') time%day, time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_d(irec)
            end if
          end if
        end if

        !! monthly print - RECALL
        if (time%end_mo == 1) then
          rec_y(irec) = rec_y(irec) + rec_m(irec)
          if (pco%recall%m == 'y') then
            write (4501,100) time%mo, time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_m(irec)
            if (pco%csvout == 'y') then
              write (4505,'(*(G0.3,:","))') time%mo, time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_m(irec)
            endif
          end if
          rec_m(irec) = hz
        end if

        !! yearly print - RECALL
        if (time%end_yr == 1) then
          rec_a(irec) = rec_a(irec) + rec_y(irec)
          if (pco%recall%y == 'y') then
            write (4502,102) '     0', time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_y(irec)
            if (pco%csvout == 'y') then
              write (4506,'(*(G0.3,:","))') '     0', time%yrc, irec, ob(iob)%num, ob(iob)%name, rec_y(irec) 
            end if
          end if
          !! zero yearly variables        
          rec_y(irec) = hz
        end if
        
      !! average annual print - RECALL
          if (time%end_sim == 1 .and. pco%recall%a == 'y') then
          rec_a(irec) = rec_a(irec) / time%yrs_prt
            write (4503,102) '     0', time%yrs, irec, ob(iob)%num, ob(iob)%name, rec_a(irec)
            if (pco%csvout == 'y') then 
              write (4507,'(*(G0.3,:","))') '     0', time%yrs, irec, ob(iob)%num, ob(iob)%name, rec_a(irec)  
            end if 
          end if

      return
      
100   format (2i6,2i8,2x,a,25f15.3)
102   format (a6,i6,2i8,2x,a,25f15.3)
       
      end subroutine recall_output