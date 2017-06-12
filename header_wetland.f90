     subroutine header_wetland
    
     use basin_module
     use reservoir_module

    !! RESERVOIR/WETLAND - DAILY
      if (pco%res%d == 'y' .and. sp_ob%res > 0 ) then
        open (2548,file="wetland_day_res.txt",recl=800)
        write (9000,*) 'RES_WET                 wetland_day_res.txt'
        write (2548,*) res_hdr
        write (2548,*) res_hdr_unt
          if (pco%csvout == 'y') then
            open (2552,file="wetland_day_res.csv",recl=800)
            write (2552,'(*(G0.3,:","))') res_hdr
            write (2552,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES_WET                 wetland_day_res.csv'
          end if
      end if
      
    !! RESERVOIR/WETLAND - MONTHLY
      if (pco%res%m == 'y' .and. sp_ob%res > 0 ) then
        open (2549,file="wetland_mon_res.txt",recl=800)
        write (9000,*) 'RES_WET                 wetland_mon_res.txt'
        write (2549,*) res_hdr
        write (2549,*) res_hdr_unt
          if (pco%csvout == 'y') then
            open (2553,file="wetland_mon_res.csv",recl=800)
            write (2553,'(*(G0.3,:","))') res_hdr
            write (2553,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES_WET                 wetland_mon_res.csv'
          end if
      end if
      
   !! RESERVOIR/WETLAND YEARLY
     if (pco%res%y == 'y' .and. sp_ob%res > 0 ) then
        open (2550,file="wetland_yr_res.txt",recl=800)
        write (9000,*) 'RES_WET                 wetland_yr_res.txt'
        write (2550,*) res_hdr
        write (2550,*) res_hdr_unt
          if (pco%csvout == 'y') then
            open (2554,file="wetland_yr_res.csv",recl=800)
            write (2554,'(*(G0.3,:","))') res_hdr
            write (2554,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES_WET                 wetland_yr_res.csv'
          end if
     end if
     
    
    !! RESERVOIR/WETLAND - AVERAGE ANNUAL   
      if (pco%res%a == 'y' .and. sp_ob%res > 0) then
        open (2551,file="wetland_aa_res.txt",recl = 800)
        write (2551,*) res_hdr
        write (2551,*) res_hdr_unt
        write (9000,*) 'RES_WET                 wetland_aa_res.txt'
          if (pco%csvout == 'y') then
            open (2555,file="wetland_aa_res.csv",recl=800)
            write (2555,'(*(G0.3,:","))') res_hdr
            write (2555,'(*(G0.3,:","))') res_hdr_unt
            write (9000,*) 'RES_WET                 wetland_aa_res.csv'
          end if
      end if
    
      return
     end subroutine header_wetland