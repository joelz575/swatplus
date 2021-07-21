      subroutine water_allocation_output (idmd)

      use time_module
      use hydrograph_module
      use water_allocation_module
      
      implicit none
      
      integer, intent (in) :: idmd          !             |
      integer :: iwallo                     !             |
      real :: const                         !             |

      wallom_out(idmd)%src(1) = wallom_out(idmd)%src(1) + wallod_out(idmd)%src(1)
      wallom_out(idmd)%src(2) = wallom_out(idmd)%src(2) + wallod_out(idmd)%src(2)
      iwallo = 1
      
!!!!! daily print
       if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
        if (pco%wb_reg%d == "y") then   !!using region water balance print codes for now
          write (2510,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, wallod_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, wallod_out(idmd)%src(2)  
           if (pco%csvout == "y") then
             write (2514,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, wallod_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, wallod_out(idmd)%src(2)
           end if
        end if
       end if
       wallod_out(idmd)%src(1) = walloz
       wallod_out(idmd)%src(2) = walloz

!!!!! monthly print
        if (time%end_mo == 1) then
          walloy_out(idmd)%src(1) = walloy_out(idmd)%src(1) + wallom_out(idmd)%src(1)
          walloy_out(idmd)%src(2) = walloy_out(idmd)%src(2) + wallom_out(idmd)%src(2)
          !const = float (ndays(time%mo + 1) - ndays(time%mo))
          !wallom_out(idmd) = wallom_out(idmd) / const
          
          if (pco%wb_reg%m == "y") then
          write (2511,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, wallom_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, wallom_out(idmd)%src(2)
          if (pco%csvout == "y") then
            write (2515,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, wallom_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, wallom_out(idmd)%src(2)
          end if
        end if
        wallom_out(idmd)%src(1) = walloz
        wallom_out(idmd)%src(2) = walloz
        end if

!!!!! yearly print
      if (time%end_yr == 1) then
        walloa_out(idmd)%src(1) = walloa_out(idmd)%src(1) + walloy_out(idmd)%src(1)
        walloa_out(idmd)%src(2) = walloa_out(idmd)%src(2) + walloy_out(idmd)%src(2)
        !const = time%day_end_yr
        !walloy_out(idmd) = walloy_out(idmd) / const
          
        if (pco%wb_reg%y == "y") then 
          write (2512,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, walloy_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, walloy_out(idmd)%src(2)
          if (pco%csvout == "y") then
           write (2516,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, walloy_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, walloy_out(idmd)%src(2)
          end if
        end if
        walloy_out(idmd)%src(1) = walloz
        walloy_out(idmd)%src(2) = walloz
      end if

!!!!! average annual print
      if (time%end_sim == 1) then
        walloa_out(idmd)%src(1) = walloa_out(idmd)%src(1) / time%yrs_prt
        walloa_out(idmd)%src(2) = walloa_out(idmd)%src(2) / time%yrs_prt
        !walloa_out(idmd) = walloa_out(idmd) / time%days_prt
        
        if (pco%wb_reg%a == "y") then
        write (2513,100) time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ,              &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, walloa_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, walloa_out(idmd)%src(2)
        if (pco%csvout == "y") then
          write (2517,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, idmd, wallo(iwallo)%dmd(idmd)%ob_typ, &
              wallo(iwallo)%dmd(idmd)%obtyp_num, wallo(iwallo)%dmd(idmd)%src(1)%ob_typ,                                 &
              wallo(iwallo)%dmd(idmd)%src(1)%obtyp_num, walloa_out(idmd)%src(1), wallo(iwallo)%dmd(idmd)%src(2)%ob_typ,  &
              wallo(iwallo)%dmd(idmd)%src(2)%obtyp_num, walloa_out(idmd)%src(2)
        end if
       end if
     end if 
      
      return

100   format (4i6,i8,5x,a,i8,5x,a,2x,i8,5x,3f15.4,5x,a,i8,3f15.4)    
      end subroutine water_allocation_output
