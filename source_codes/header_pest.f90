     subroutine header_pest
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : res, sp_ob
     use output_ls_pesticide_module
     use constituent_mass_module
     implicit none 

    !! HRU_PESTICIDE - daily
      if (pco%wb_hru%d == "y" .and. cs_db%num_tot > 0) then
        open (2800,file="hru_pest_day.txt",recl=800)
        write (2800,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_day.txt"
        write (2800,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2804,file="hru_pest_day.csv",recl=800)
            write (2804,*) bsn%name, prog
            write (2804,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_day.txt.csv"
          end if
      end if
      
!! HRU_PESTICIDE - monthly
      if (pco%wb_hru%m == "y" .and. cs_db%num_tot > 0 ) then
        open (2801,file="hru_pest_mon.txt",recl=800)
        write (2801,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_mon.txt"
        write (2801,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2805,file="hru_pest_mon.csv",recl=800)
            write (2805,*) bsn%name, prog
            write (2805,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_mon.txt.csv"
          end if
      end if
      
!! HRU_PESTICIDE - yearly
      if (pco%wb_hru%y == "y" .and. cs_db%num_tot > 0) then
        open (2802,file="hru_pest_yr.txt",recl=800)
        write (2802,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_yr.txt"
        write (2802,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2806,file="hru_pest_yr.csv",recl=800)
            write (2806,*) bsn%name, prog
            write (2806,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_yr.txt.csv"
          end if
      end if
      
!! HRU_PESTICIDE - ave annual
      if (pco%wb_hru%a == "y" .and. cs_db%num_tot > 0) then
        open (2803,file="hru_pest_aa.txt",recl=800)
        write (2803,*) bsn%name, prog
        write (9000,*) "HRU_PEST                  hru_pest_aa.txt"
        write (2803,*) pestb_hdr

          if (pco%csvout == "y") then
            open (2807,file="hru_pest_aa.csv",recl=800)
            write (2807,*) bsn%name, prog
            write (2807,'(*(G0.3,:","))') pestb_hdr
            write (9000,*) "HRU_PEST                  hru_pest_aa.txt.csv"
          end if
      end if
    
      return
      end subroutine header_pest  