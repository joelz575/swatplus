      subroutine hru_pesticide_output(ihru)
    
      use output_ls_pesticide_module
      use plant_module
      use plant_data_module
      use time_module
      use basin_module
      use output_landscape_module
      use constituent_mass_module
      use hydrograph_module, only : sp_ob1, ob
      
      implicit none
      
      integer, intent (in) :: ihru             !            |
      integer :: ipest                         !            |
      integer :: j
      integer :: iob
      real :: const
                         
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine outputs HRU variables on daily, monthly and annual time steps

      j = ihru
      
      iob = sp_ob1%hru + j - 1
          
      !! print balance for each pesticide
      do ipest = 1, cs_db%num_pests
          
      hpestb_m(j)%pest(ipest) = hpestb_m(j)%pest(ipest) + hpest_bal(j)%pest(ipest)

      !! daily print
        if (pco%day_print == "y" .and. pco%int_day_cur == pco%int_day) then
          if (pco%wb_hru%d == "y") then
             write (2800,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpest_bal(j)%pest(ipest)   !! pesticide balance
             if (pco%csvout == "y") then
               write (2804,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpest_bal(j)%pest(ipest)
             end if
          end if
        end if
        !! check end of month
        if (time%end_mo == 1) then
          const = float (ndays(time%mo + 1) - ndays(time%mo))
          hpestb_m(j)%pest(ipest) = hpestb_m(j)%pest(ipest) // const
          !hwb_m(j) = hwb_m(j) // const
          !hwb_m(j)%cn = hwb_m(j)%cn / const
          !hwb_m(j)%snopack = hwb_m(j)%snopack / const
          !hwb_m(j)%sw = hwb_m(j)%sw / const
          !hwb_m(j)%sw_300 = hwb_m(j)%sw_300 / const
          
          hpestb_y(j)%pest(ipest) = hpestb_y(j)%pest(ipest) + hpestb_m(j)%pest(ipest)

          !! monthly print
           if (pco%wb_hru%m == "y") then
             write (2801,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_m(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2805,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_m(j)%pest(ipest)
               end if
           end if
          
          hpestb_m(j)%pest(ipest) = pestbz
        end if
        
        !! check end of year
        if (time%end_yr == 1) then
          hpestb_y(j)%pest(ipest) = hpestb_y(j)%pest(ipest) // 12.
          !hwb_y(j) = hwb_y(j) // 12.
          !hwb_y(j)%cn = hwb_y(j)%cn / 12.
          !hwb_y(j)%snopack = hwb_y(j)%snopack / 12.
          !hwb_y(j)%sw = hwb_y(j)%sw / 12.
          !hwb_y(j)%sw_300 = hwb_y(j)%sw_300 / 12.
          hpestb_a(j)%pest(ipest) = hpestb_a(j)%pest(ipest) + hpestb_y(j)%pest(ipest)

          !! yearly print
           if (time%end_yr == 1 .and. pco%wb_hru%y == "y") then
             write (2802,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_y(j)%pest(ipest)
               if (pco%csvout == "y") then
                 write (2806,'(*(G0.3,:","))') time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_y(j)%pest(ipest)
               end if
           end if
           
        end if
        
!!!!! average annual print
         if (time%end_sim == 1 .and. pco%wb_hru%a == "y") then
           hpestb_a(j)%pest(ipest) = hpestb_a(j)%pest(ipest) / time%yrs_prt
           write (2803,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_a(j)%pest(ipest)
           if (pco%csvout == "y") then
             write (2807,100) time%day, time%mo, time%day_mo, time%yrc, j, ob(iob)%gis_id, ob(iob)%name, hpestb_a(j)%pest(ipest)
           end if
           hpestb_a(j)%pest(ipest) = pestbz
         end if

      end do    !pesticide loop
      return
100   format (4i6,2i8,2x,a,9f12.3)      
101   format (4i6,2i8,2x,a,20f12.3)
102   format (4i6,2i8,2x,a,20f12.3)
103   format (2i6,i8,4x,a,5x,f12.3)
104   format (4i6,2i8,2x,a,27f18.3)
       
      end subroutine hru_pesticide_output