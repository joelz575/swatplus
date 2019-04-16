      subroutine header_sd_channel

      use sd_channel_module
      use basin_module
      use hydrograph_module
      implicit none 

!!!  SWAT-DEG CHANNEL
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == "y") then
          open (2500,file="channel_sd_day.txt",recl = 1500)
          write (2500,*) bsn%name, prog
          write (2500,*) sdch_hdr !! swat deg channel
          write (2500,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG CHANNEL          channel_sd_day.txt"
          if (pco%csvout == "y") then
            open (2504,file="channel_sd_day.csv",recl = 1500)
            write (2504,*) bsn%name, prog
            write (2504,'(*(G0.3,:,","))') sdch_hdr 
            write (2504,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG CHANNEL          channel_sd_day.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          open (2501,file="channel_sd_mon.txt",recl = 1500)
          write (2501,*) bsn%name, prog
          write (2501,*) sdch_hdr   !! swat deg channel
          write (2501,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG CHANNEL          channel_sd_mon.txt"
          if (pco%csvout == "y") then
            open (2505,file="channel_mon_sd.csv",recl = 1500)
            write (2505,*) bsn%name, prog
            write (2505,'(*(G0.3,:,","))') sdch_hdr   
            write (2505,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG CHANNEL          channel_sd_mon.csv"
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          open (2502,file="channel_sd_yr.txt",recl = 1500)
          write (2502,*) bsn%name, prog
          write (2502,*) sdch_hdr !! swat deg channel
          write (2502,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG CHANNEL          channel_sd_yr.txt"
          if (pco%csvout == "y") then
            open (2506,file="channel_sd_yr.csv",recl = 1500)
            write (2506,*) bsn%name, prog
            write (2506,'(*(G0.3,:,","))') sdch_hdr !! swat deg channel csv
            write (2506,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG CHANNEL          channel_sd_yr.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          open (2503,file="channel_sd_aa.txt",recl = 1500)
          write (2503,*) bsn%name, prog
          write (2503,*) sdch_hdr   !! swat deg channel
          write (2503,*) sdch_hdr_units
          write (9000,*) "SWAT-DEG CHANNEL          channel_sd_aa.txt"
          if (pco%csvout == "y") then
            open (2507,file="channel_sd_aa.csv",recl = 1500)
            write (2507,*) bsn%name, prog
            write (2507,'(*(G0.3,:,","))') sdch_hdr   
            write (2507,'(*(G0.3,:,","))') sdch_hdr_units
            write (9000,*) "SWAT-DEG CHANNEL          channel_sd_aa.csv"
          end if
          end if
         end if 
        
      return
      end subroutine header_sd_channel