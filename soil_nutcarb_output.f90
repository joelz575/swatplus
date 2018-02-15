      subroutine soil_nutcarb_output
    
      use basin_module
      use time_module

      !! print soil nutrients carbon output file
        if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                 .and. time%day <= pco%jd_end .and. int_print == pco%interval) then
          if (pco%snutc == 'd') then
            call soil_nutcarb_write
          end if
          if (pco%snutc == 'm' .and. time%end_mo == 1) then
            call soil_nutcarb_write
          end if
          if (pco%snutc == 'y' .and. time%end_yr == 1) then
            call soil_nutcarb_write
          end if
          if (pco%snutc == 'a' .and. time%end_sim ==1) then
            call soil_nutcarb_write
          end if       
        end if
    
      return
      
      end subroutine soil_nutcarb_output