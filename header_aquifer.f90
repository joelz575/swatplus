      subroutine header_aquifer

      use parm
      use aquifer_module

         
!!!  AQUIFER
       if (sp_ob%aqu > 0) then
        if (pco%aqu%d == 'y') then
          open (2520,file="aquifer_day_aqu.txt",recl = 1500)
          write (2520,*) aqu_hdr !! aquifer
          write (9000,*) 'AQUIFER             aquifer_day_aqu.txt'
         if (pco%csvout == 'y') then
            open (2524,file="aquifer_day_aqu.csv",recl = 1500)
            write (2524,'(*(G0.3,:,","))') sdch_hdr   !! aquifer csv
            write (9000,*) 'AQUIFER             aquifer_day_aqu.csv'
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%m == 'y') then
          open (2521,file="aquifer_mon_aqu.txt",recl = 1500)
          write (2521,*) aqu_hdr   !! aquifer
          write (9000,*) 'AQUIFER             aquifer_mon_aqu.txt'
          if (pco%csvout == 'y') then
            open (2525,file="aquifer_mon_aqu.csv",recl = 1500)
            write (2525,'(*(G0.3,:,","))') sdch_hdr   !! aquifer csv
            write (9000,*) 'AQUIFER             aquifer_mon_aqu.csv'
          end if
         end if
        end if

       if (sp_ob%aqu > 0) then
        if (pco%aqu%y == 'y') then
          open (2522,file="aquifer_yr_aqu.txt",recl = 1500)
          write (2522,*) aqu_hdr !! aquifer
          write (9000,*) 'AQUIFER             aquifer_yr_aqu.txt'
         if (pco%csvout == 'y') then
            open (2526,file="aquifer_yr_aqu.csv",recl = 1500)
            write (2526,'(*(G0.3,:,","))') sdch_hdr   !! aquifer csv
            write (9000,*) 'AQUIFER             aquifer_yr_aqu.csv'
         end if
        endif
       endif
       
        if (sp_ob%aqu > 0) then
         if (pco%aqu%a == 'y') then
          open (2523,file="aquifer_aa_aqu.txt",recl = 1500)
          write (2523,*) aqu_hdr   !! aquifer
          write (9000,*) 'AQUIFER             aquifer_aa_aqu.txt'
          if (pco%csvout == 'y') then
            open (2527,file="aquifer_aa_aqu.csv",recl = 1500)
            write (2527,'(*(G0.3,:,","))') sdch_hdr   !! aquifer csv
            write (9000,*) 'AQUIFER             aquifer_aa_aqu.csv'
          end if
         end if 
        end if 
                        
      return
      end subroutine header_aquifer