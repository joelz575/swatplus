      subroutine header_channel

      use parm
      use channel_module
      
!!!  CHANNEL
      if (sp_ob%chan > 0) then
        if (pco%chan%d == 'y') then
          open (2480,file="channel_day_cha.txt",recl = 1500)
          write (2480,*) ch_hdr !! channel
          write (9000,*) 'CHANNEL             channel_day_cha.txt'
          if (pco%csvout == 'y')  then
            open (2484,file="channel_day_cha.csv",recl = 1500)
            write (2484,'(*(G0.3,:,","))') ch_hdr !! channel header csv format
           write (9000,*) 'CHANNEL             channel_day_cha.csv'
          end if
        endif
      endif
        
        if (sp_ob%chan > 0) then
          if (pco%chan%m == 'y') then
          open (2481,file="channel_mon_cha.txt",recl = 1500)
          write (2481,*) ch_hdr   !! channel
          write (9000,*) 'CHANNEL             channel_mon_cha.txt'
          if (pco%csvout == 'y') then
            open (2485,file="channel_mon_cha.csv",recl = 1500)
            write (2485,'(*(G0.3,:,","))') ch_hdr   !! channel aa header csv format
            write (9000,*) 'CHANNEL             channel_mon_cha.csv'
          end if
          end if
         end if

      if (sp_ob%chan > 0) then
        if (pco%chan%y == 'y') then
          open (2482,file="channel_yr_cha.txt",recl = 1500)
          write (2482,*) ch_hdr !! channel
          write (9000,*) 'CHANNEL             channel_yr_cha.txt'
          if (pco%csvout == 'y')  then
            open (2486,file="channel_yr_cha.csv",recl = 1500)
            write (2486,'(*(G0.3,:,","))') ch_hdr !! channel header csv format
           write (9000,*) 'CHANNEL             channel_yr_cha.csv'
          end if
        endif
      endif
        
        if (sp_ob%chan > 0) then
          if (pco%chan%a == 'y') then
          open (2483,file="channel_aa_cha.txt",recl = 1500)
          write (2483,*) ch_hdr   !! channel
          write (9000,*) 'CHANNEL             channel_aa_cha.txt'
          if (pco%csvout == 'y') then
            open (2487,file="channel_aa_cha.csv",recl = 1500)
            write (2487,'(*(G0.3,:,","))') ch_hdr   !! channel aa header csv format
            write (9000,*) 'CHANNEL             channel_aa_cha.csv'
          end if
          end if
        end if
                         
      return
      end subroutine header_channel