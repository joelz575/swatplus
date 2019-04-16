      subroutine reservoir_output(j)
      
      use time_module
      use basin_module
      use reservoir_module
      use hydrograph_module
      
      implicit none
      
      integer, intent (in) :: j   !                |
      integer :: iob              !                |
      real :: const               !                |
      
      iob = sp_ob1%res + j - 1

!!!!! daily print
         if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%res%d == "y") then
            write (2540,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_d(j), res_out_d(j), res_om_d(j)
               if (pco%csvout == "y") then
               write (2544,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_d(j), res_out_d(j), res_om_d(j)
             end if
          end if 
         end if 
  
         
        res_in_m(j) = res_in_m(j) + res_in_d(j)
        res_out_m(j) = res_out_m(j) + res_out_d(j)
        res_om_m(j) = res_om_m(j) + res_om_d(j)

!!!!! monthly print
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          res_in_m(j)%flo = res_in_m(j)%flo / const
          res_in_y(j) = res_in_y(j) + res_in_m(j)
          res_out_m(j)%flo = res_out_m(j)%flo / const
          res_out_y(j) = res_out_y(j) + res_out_m(j)
          res_om_m(j) = res_om_m(j) / const
          res_om_y(j) = res_om_y(j) + res_om_m(j)
          if (pco%res%m == "y") then
            write (2541,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_m(j), res_out_m(j), res_om_m(j)
              if (pco%csvout == "y") then
                write (2545,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_m(j), res_out_m(j), res_om_m(j) 
              end if 
          end if
          res_in_m(j) = resmz
          res_out_m(j) = resmz
          res_om_m(j) = res_omz
          !res_om_m(j) = resmz
        end if

!!!!! yearly print
       if (time%end_yr == 1) then
          res_in_y(j)%flo = res_in_y(j)%flo / 12.
          res_in_a(j) = res_in_a(j) + res_in_y(j)
          res_out_y(j)%flo = res_out_y(j)%flo / 12.
          res_out_a(j) = res_out_a(j) + res_out_y(j)
          res_om_y(j) = res_om_y(j) / 12.
          res_om_a(j) = res_om_a(j) + res_om_y(j)
          if (pco%res%y == "y") then
            write (2542,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_y(j), res_out_y(j), res_om_y(j)
              if (pco%csvout == "y") then
                write (2546,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_y(j), res_out_y(j), res_om_y(j)
              end if
          end if
          res_in_y(j) = resmz
          res_out_y(j) = resmz
          res_om_y(j) = res_omz
       end if

!!!!! average annual print
        if (time%end_sim == 1 .and. pco%res%a == "y") then
          res_in_a(j) = res_in_a(j) / time%yrs_prt
          
          res_out_a(j) = res_out_a(j) / time%yrs_prt
          res_om_a(j) = res_om_a(j) / time%yrs_prt
          write (2543,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_a(j), res_out_a(j), res_om_a(j)
          if (pco%csvout == "y") then
            write (2547,'(*(G0.3,:","))')time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, res_in_a(j), res_out_a(j), res_om_a(j)
          end if 
          res_in_a(j) = resmz
          res_out_a(j) = resmz
          res_om_a(j) = res_omz
        end if
        
      return
        
100   format (4i6,2i10,2x,a,e10.3,e12.3,45e10.3)
      
     
      end subroutine reservoir_output