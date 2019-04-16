     subroutine header_reservoir
    
     use basin_module
     use reservoir_module
     use hydrograph_module, only : res, sp_ob
     implicit none 

    !! RESERVOIR
      if (pco%res%d == "y" .and. sp_ob%res > 0 ) then
        open (2540,file="reservoir_day.txt",recl=800)
        write (2540,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_day.txt"
        ! may need to combine these two header types below ??? gsm  9/2018
        write (2540,*) res_hdr, res_hdr1, res_hdr2
        ! may need to combine these two header types below ??? gsm  9/2018
        write (2540,*) res_hdr_unt, res_hdr_unt1, res_hdr_unt2
          if (pco%csvout == "y") then
            open (2544,file="reservoir_day.csv",recl=800)
            write (2544,*) bsn%name, prog
            write (2544,'(*(G0.3,:","))') res_hdr, res_hdr1, res_hdr2
            write (2544,'(*(G0.3,:","))') res_hdr_unt, res_hdr_unt1, res_hdr_unt2
            write (9000,*) "RES                       reservoir_day.csv"
          end if
      end if
      
     if (pco%res%m == "y" .and. sp_ob%res > 0 ) then
        open (2541,file="reservoir_mon.txt",recl=800)
        write (2541,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_mon.txt"
        write (2541,*) res_hdr, res_hdr1, res_hdr2
        write (2541,*) res_hdr_unt, res_hdr_unt1, res_hdr_unt2
           if (pco%csvout == "y") then
            open (2545,file="reservoir_mon.csv",recl=800)
            write (2545,*) bsn%name, prog
            write (2545,'(*(G0.3,:","))') res_hdr, res_hdr1, res_hdr2
            write (2545,'(*(G0.3,:","))') res_hdr_unt, res_hdr_unt1, res_hdr_unt2
            write (2545,*) "RES                       reservoir_mon.csv"
          end if
     end if
     
     if (pco%res%y == "y" .and. sp_ob%res > 0 ) then
        open (2542,file="reservoir_yr.txt",recl=800)
        write (2542,*) bsn%name, prog
        write (9000,*) "RES                       reservoir_yr.txt"
        write (2542,*) res_hdr, res_hdr1, res_hdr2
        write (2542,*) res_hdr_unt, res_hdr_unt1, res_hdr_unt2
          if (pco%csvout == "y") then
            open (2546,file="reservoir_yr.csv",recl=800)
            write (2546,*) bsn%name, prog
            write (2546,'(*(G0.3,:","))') res_hdr, res_hdr1, res_hdr2
            write (2546,'(*(G0.3,:","))') res_hdr_unt, res_hdr_unt1, res_hdr_unt2
            write (9000,*) "RES                       reservoir_yr.csv"
          end if
      end if
      
      if (pco%res%a == "y" .and. sp_ob%res > 0) then
        open (2543,file="reservoir_aa.txt",recl = 800)
        write (2543,*) bsn%name, prog
        write (2543,*) res_hdr, res_hdr1, res_hdr2
        write (2543,*) res_hdr_unt, res_hdr_unt1, res_hdr_unt2
        write (9000,*) "RES                       reservoir_aa.txt"
          if (pco%csvout == "y") then
            open (2547,file="reservoir_aa.csv",recl=800)
            write (2547,*) bsn%name, prog
            write (2547,'(*(G0.3,:","))') res_hdr, res_hdr1, res_hdr2
            write (2547,'(*(G0.3,:","))') res_hdr_unt, res_hdr_unt1, res_hdr_unt2
            write (9000,*) "RES                       reservoir_aa.csv"
          end if
      end if
    
      return
      end subroutine header_reservoir  