      subroutine header_sd_channel

      use sd_channel_module
      use basin_module
      use hydrograph_module
      implicit none 

!!!  SWAT-DEG CHANNEL
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == "y") then
          open (2500,file="channel_day_sd.txt",recl = 1500)
          write (2500,*) bsn%name, prog
          write (2500,*) sdch_hdr !! swat deg channel
          write (9000,*) "SWAT-DEG CHANNEL    channel_day_sd.txt"
          if (pco%csvout == "y") then
            open (2504,file="channel_day_sd.csv",recl = 1500)
            write (2504,*) bsn%name, prog
            write (2504,'(*(G0.3,:,","))') sdch_hdr 
            write (9000,*) "SWAT-DEG CHANNEL    channel_day_sd.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == "y") then  
          open (2501,file="channel_mon_sd.txt",recl = 1500)
          write (2501,*) bsn%name, prog
          write (2501,*) sdch_hdr   !! swat deg channel
          write (9000,*) "SWAT-DEG CHANNEL    channel_mon_sd.txt"
          if (pco%csvout == "y") then
            open (2505,file="channel__mon_sd.csv",recl = 1500)
            write (2505,*) bsn%name, prog
            write (2505,'(*(G0.3,:,","))') sdch_hdr   
            write (9000,*) "SWAT-DEG CHANNEL    channel_mon_sd.csv"
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == "y") then
          open (2502,file="channel_yr_sd.txt",recl = 1500)
          write (2502,*) bsn%name, prog
          write (2502,*) sdch_hdr !! swat deg channel
          write (9000,*) "SWAT-DEG CHANNEL    channel_yr_sd.txt"
          if (pco%csvout == "y") then
            open (2506,file="channel_yr_sd.csv",recl = 1500)
            write (2506,*) bsn%name, prog
            write (2506,'(*(G0.3,:,","))') sdch_hdr !! swat deg channel csv
            write (9000,*) "SWAT-DEG CHANNEL    channel_yr_sd.csv"
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == "y") then
          open (2503,file="channel_aa_sd.txt",recl = 1500)
          write (2503,*) bsn%name, prog
          write (2503,*) sdch_hdr   !! swat deg channel
          write (9000,*) "SWAT-DEG CHANNEL    channel_aa_sd.txt"
          if (pco%csvout == "y") then
            open (2507,file="channel_aa_sd.csv",recl = 1500)
            write (2507,*) bsn%name, prog
            write (2507,'(*(G0.3,:,","))') sdch_hdr   
            write (9000,*) "SWAT-DEG CHANNEL    channel_aa_sd.csv"
          end if
          end if
         end if 
        
      return
      end subroutine header_sd_channel