     subroutine header_hyd
    
     use basin_module   
     use hydrograph_module
   
      !! HYDCON (no headers)    
      if (pco%hydcon == 'y') then
        open (7000,file="hydcon.out")
        write (9000,*) 'HYDCON              hydcon.out'
          if (pco%csvout == 'y') then
            open (7001,file="hydcon.csv")
            write (9000,*) 'HYDCON              hydcon.csv'
          end if
      end if

      !! HYDOUT  
      if (pco%hyd%d == 'y') then
        open (2580,file="hydout_day_hyd.txt",recl=800)
        write (2580,*) hyd_hdr
        write (9000,*) 'HYDOUT              hydout_day_hyd.txt'
          if (pco%csvout == 'y') then
            open (2584,file="hydout_day_hyd.csv",recl=800)
            write (2584,'(*(G0.3,:","))') hyd_hdr
            write (9000,*)   'HYDOUT              hydout_day_hyd.csv'
          end if
      end if
      
     if (pco%hyd%m == 'y') then
        open (2581,file="hydout_mon_hyd.txt",recl=800)
        write (2581,*) hyd_hdr
        write (9000,*) 'HYDOUT              hydout_mon_hyd.txt'
          if (pco%csvout == 'y') then
            open (2585,file="hydout_mon_hyd.csv",recl=800)
            write (2585,'(*(G0.3,:","))') hyd_hdr
            write (9000,*)   'HYDOUT              hydout_mon_hyd.csv'
          end if
     end if
     
     if (pco%hyd%y == 'y') then
        open (2582,file="hydout_yr_hyd.txt",recl=800)
        write (2582,*) hyd_hdr
        write (9000,*) 'HYDOUT              hydout_yr_hyd.txt'
          if (pco%csvout == 'y') then
            open (2586,file="hydout_yr_hyd.csv",recl=800)
            write (2586,'(*(G0.3,:","))') hyd_hdr
            write (9000,*)   'HYDOUT              hydout_yr_hyd.csv'
          end if
     end if
     
     if (pco%hyd%a == 'y') then
        open (2583,file="hydout_aa_hyd.txt",recl=800)
        write (2583,*) hyd_hdr
        write (9000,*) 'HYDOUT              hydout_aa_hyd.txt'
          if (pco%csvout == 'y') then
            open (2587,file="hydout_aa_hyd.csv",recl=800)
            write (2587,'(*(G0.3,:","))') hyd_hdr
            write (9000,*)   'HYDOUT              hydout_aa_hyd.csv'
          end if
       end if
        

     !! HYDIN 
       if (pco%hyd%d == 'y') then
        open (2560,file="hydin_day_hyd.txt",recl=800)
        write (2560,*) hyd_hdr
        write (9000,*)     'HYDIN               hydin_day_hyd.txt'
          if (pco%csvout == 'y') then
            open (2564,file="hydin_day_hyd.csv",recl=800)
            write (2564,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'HYDIN               hydin_day_hyd.csv'
          end if
       endif
       
      if (pco%hyd%m == 'y') then
        open (2561,file="hydin_mon_hyd.txt",recl=800)
        write (2561,*) hyd_hdr
        write (9000,*)     'HYDIN               hydin_mon_hyd.txt'
          if (pco%csvout == 'y') then
            open (2565,file="hydin_mon_hyd.csv",recl=800)
            write (2565,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'HYDIN               hydin_mon_hyd.csv'
          end if
      endif
      
      if (pco%hyd%y == 'y') then
        open (2562,file="hydin_yr_hyd.txt",recl=800)
        write (2562,*) hyd_hdr
        write (9000,*)     'HYDIN               hydin_yr_hyd.txt'
          if (pco%csvout == 'y') then
            open (2566,file="hydin_yr_hyd.csv",recl=800)
            write (2566,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'HYDIN               hydin_yr_hyd.csv'
          end if
      endif
      
      if (pco%hyd%a == 'y') then
        open (2563,file="hydin_aa_hyd.txt",recl=800)
        write (2563,*) hyd_hdr
        write (9000,*)     'HYDIN               hydin_aa_hyd.txt'
          if (pco%csvout == 'y') then
            open (2567,file="hydin_aa_hyd.csv",recl=800)
            write (2567,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'HYDIN               hydin_aa_hyd.csv'
          end if
      endif
      
      !! hydrograph deposition - DAILY
      if (pco%hyd%d == 'y') then
        open (2700,file="deposition_day.txt",recl=800)
        write (2700,*) hyd_hdr
        write (9000,*) 'DEPO                deposition_day.txt'
          if (pco%csvout == 'y') then
            open (2704,file="deposition_day.csv",recl=800)
            write (2704,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'DEPO                deposition_day.csv'
          end if
      end if
       
      !! hydrograph deposition - MONTLY
      if (pco%hyd%m == 'y') then
        open (2701,file="deposition_mon.txt",recl=800)
        write (2701,*) hyd_hdr
        write (9000,*) 'DEPO                deposition_mon.txt'
          if (pco%csvout == 'y') then
            open (2705,file="deposition_mon.csv",recl=800)
            write (2705,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'DEPO                deposition_mon.csv'
          end if
       end if
       
      !! hydrograph deposition - YEARLY
       if (pco%hyd%y == 'y') then
        open (2702,file="deposition_yr.txt",recl=800)
        write (2702,*) hyd_hdr
        write (9000,*) 'DEPO                deposition_yr.txt'
          if (pco%csvout == 'y') then
            open (2706,file="deposition_yr.csv",recl=800)
            write (2706,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'DEPO                deposition_yr.csv'
          end if
       end if
       
      !! hydrograph deposition - ANNUAL
       if (pco%hyd%a == 'y') then
        open (2703,file="deposition_aa.txt",recl=800)
        write (2703,*) hyd_hdr
        write (9000,*) 'DEPO                deposition_aa.txt'
          if (pco%csvout == 'y') then
            open (2707,file="deposition_aa.csv",recl=800)
            write (2707,'(*(G0.3,:","))') hyd_hdr
            write (9000,*) 'DEPO                deposition_aa.csv'
          end if
       end if
  
      return
      end subroutine header_hyd 