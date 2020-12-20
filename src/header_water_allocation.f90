      subroutine header_water_allocation

      use maximum_data_module
      use water_allocation_module
      use basin_module
      
      implicit none 

!!!  SWAT-DEG CHANNEL
      if (db_mx%wallo_db > 0) then
        if (pco%wb_reg%d == "y") then
          open (2510,file="water_allo_day.txt",recl = 1500)
          write (2510,*) bsn%name, prog
          write (2510,*) wallo_hdr
          write (2510,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_day.txt"
          if (pco%csvout == "y") then
            open (2514,file="water_allo_day.csv",recl = 1500)
            write (2514,*) bsn%name, prog
            write (2514,'(*(G0.3,:,","))') wallo_hdr
            write (2514,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_day.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%wb_reg%m == "y") then  
          open (2511,file="water_allo_mon.txt",recl = 1500)
          write (2511,*) bsn%name, prog
          write (2511,*) wallo_hdr
          write (2511,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_mon.txt"
          if (pco%csvout == "y") then
            open (2515,file="water_allo_mon.csv",recl = 1500)
            write (2515,*) bsn%name, prog
            write (2515,'(*(G0.3,:,","))') wallo_hdr
            write (2515,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_mon.csv"
          end if
          end if
         end if 
        
      if (db_mx%wallo_db > 0) then
        if (pco%wb_reg%y == "y") then
          open (2512,file="water_allo_yr.txt",recl = 1500)
          write (2512,*) bsn%name, prog
          write (2512,*) wallo_hdr
          write (2512,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_yr.txt"
          if (pco%csvout == "y") then
            open (2516,file="water_allo_yr.csv",recl = 1500)
            write (2516,*) bsn%name, prog
            write (2516,'(*(G0.3,:,","))') wallo_hdr
            write (2516,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_yr.csv"
          end if
        endif
      endif
      
        if (db_mx%wallo_db > 0) then
          if (pco%wb_reg%a == "y") then
          open (2513,file="water_allo_aa.txt",recl = 1500)
          write (2513,*) bsn%name, prog
          write (2513,*) wallo_hdr
          write (2513,*) wallo_hdr_units
          write (9000,*) "WATER_ALLOCATION          water_allo_aa.txt"
          if (pco%csvout == "y") then
            open (2517,file="water_allo_aa.csv",recl = 1500)
            write (2517,*) bsn%name, prog
            write (2517,'(*(G0.3,:,","))') wallo_hdr
            write (2517,'(*(G0.3,:,","))') wallo_hdr_units
            write (9000,*) "WATER_ALLOCATION          water_allo_aa.csv"
          end if
          end if
         end if 
       
      return
      end subroutine header_water_allocation
