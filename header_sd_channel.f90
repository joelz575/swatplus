      subroutine header_sd_channel

      use parm
      use sd_channel_module

!!!  SWAT-DEG CHANNEL
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%d == 'y') then
          open (2500,file="channel_sd_day_cha.txt",recl = 1500)
          write (2500,*) sdch_hdr !! swat deg channel
          write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_day_cha.txt'
          if (pco%csvout == 'y') then
            open (2504,file="channel_sd_day_cha.csv",recl = 1500)
            write (2504,'(*(G0.3,:,","))') sdch_hdr 
            write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_day_cha.csv'
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%m == 'y') then  
          open (2501,file="channel_sd_mon_cha.txt",recl = 1500)
          write (2501,*) sdch_hdr   !! swat deg channel
          write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_mon_cha.txt'
          if (pco%csvout == 'y') then
            open (2505,file="channel_sd_mon_cha.csv",recl = 1500)
            write (2505,'(*(G0.3,:,","))') sdch_hdr   
            write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_mon_cha.csv'
          end if
          end if
         end if 
        
      if (sp_ob%chandeg > 0) then
        if (pco%sd_chan%y == 'y') then
          open (2502,file="channel_sd_yr_cha.txt",recl = 1500)
          write (2502,*) sdch_hdr !! swat deg channel
          write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_yr_cha.txt'
          if (pco%csvout == 'y') then
            open (2506,file="channel_sd_yr_cha.csv",recl = 1500)
            write (2506,'(*(G0.3,:,","))') sdch_hdr !! swat deg channel csv
            write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_yr_cha.csv'
          end if
        endif
      endif
      
        if (sp_ob%chandeg > 0) then
          if (pco%sd_chan%a == 'y') then
          open (2503,file="channel_sd_aa_cha.txt",recl = 1500)
          write (2503,*) sdch_hdr   !! swat deg channel
          write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_aa_cha.txt'
          if (pco%csvout == 'y') then
            open (2507,file="channel_sd_aa_cha.csv",recl = 1500)
            write (2507,'(*(G0.3,:,","))') sdch_hdr   
            write (9000,*) 'SWAT-DEG CHANNEL    channel_sd_aa_cha.csv'
          end if
          end if
         end if 
        
      return
      end subroutine header_sd_channel