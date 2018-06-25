     subroutine header_cs
   
     use hydrograph_module
     use constituent_mass_module
   
     implicit none 
           
     !! daily - HYDIN
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          open (2708,file="hydin_pests_day.txt")
            write (9000,*) "HYDIN_PESTS              hydin_pests_day.txt"
            if (pco%csvout == "y") then
              open (2724,file="hydin_pests_day.csv")
              write (9000,*) "HYDIN_PESTS              hydin_pests_day.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2712,file="hydin_paths_day.txt")
            write (9000,*) "HYDIN_PATHS              hydin_paths_day.txt"
            if (pco%csvout == "y") then
              open (2728,file="hydin_paths_day.csv")
              write (9000,*) "HYDIN_PATHS            hydin_paths_day.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2716,file="hydin_metals_day.txt")
            write (9000,*) "HYDIN_METALS              hydin_metals_day.txt"
            if (pco%csvout == "y") then
              open (2732,file="hydin_metals_day.csv")
              write (9000,*) "HYDIN_METALS              hydin_metals_day.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2720,file="hydin_salts_day.txt")
            write (9000,*) "HYDIN_SALTS              hydin_salts_day.txt"
            if (pco%csvout == "y") then
              open (2736,file="hydin_salts_day.csv")
              write (9000,*) "HYDIN_SALTS              hydin_salts_day.csv"
            end if
        end if
     end if      !! end daily - HYDIN

     !! monthly - HYDIN
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          open (2709,file="hydin_pests_mon.txt")
            write (9000,*) "HYDIN_PESTS              hydin_pests_mon.txt"
            if (pco%csvout == "y") then
              open (2725,file="hydin_pests_mon.csv")
              write (9000,*) "HYDIN_PESTS              hydin_pests_mon.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2713,file="hydin_paths_mon.txt")
            write (9000,*) "HYDIN_PATHS              hydin_paths_mon.txt"
            if (pco%csvout == "y") then
              open (2729,file="hydin_paths_mon.csv")
              write (9000,*) "HYDIN_PATHS            hydin_paths_mon.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2717,file="hydin_metals_mon.txt")
            write (9000,*) "HYDIN_METALS              hydin_metals_mon.txt"
            if (pco%csvout == "y") then
              open (2733,file="hydin_metals_mon.csv")
              write (9000,*) "HYDIN_METALS              hydin_metals_mon.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2721,file="hydin_salts_mon.txt")
            write (9000,*) "HYDIN_SALTS              hydin_salts_mon.txt"
            if (pco%csvout == "y") then
              open (2737,file="hydin_salts_mon.csv")
              write (9000,*) "HYDIN_SALTS              hydin_salts_mon.csv"
            end if
        end if
     end if         !! end monthly - HYDIN

     
     !! yearly - HYDIN
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          open (2710,file="hydin_pests_yr.txt")
            write (9000,*) "HYDIN_PESTS              hydin_pests_yr.txt"
            if (pco%csvout == "y") then
              open (2726,file="hydin_pests_yr.csv")
              write (9000,*) "HYDIN_PESTS              hydin_pests_yr.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2714,file="hydin_paths_yr.txt")
            write (9000,*) "HYDIN_PATHS              hydin_paths_yr.txt"
            if (pco%csvout == "y") then
              open (2730,file="hydin_paths_yr.csv")
              write (9000,*) "HYDIN_PATHS            hydin_paths_yr.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2718,file="hydin_metals_yr.txt")
            write (9000,*) "HYDIN_METALS              hydin_metals_yr.txt"
            if (pco%csvout == "y") then
              open (2734,file="hydin_metals_yr.csv")
              write (9000,*) "HYDIN_METALS              hydin_metals_yr.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2722,file="hydin_salts_yr.txt")
            write (9000,*) "HYDIN_SALTS              hydin_salts_yr.txt"
            if (pco%csvout == "y") then
              open (2738,file="hydin_salts_yr.csv")
              write (9000,*) "HYDIN_SALTS              hydin_salts_yr.csv"
            end if
        end if
     end if         !! end yearly - HYDIN

     !! average annual - HYDIN
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          open (2711,file="hydin_pests_aa.txt")
            write (9000,*) "HYDIN_PESTS              hydin_pests_aa.txt"
            if (pco%csvout == "y") then
              open (2727,file="hydin_pests_aa.csv")
              write (9000,*) "HYDIN_PESTS              hydin_pests_aa.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2715,file="hydin_paths_aa.txt")
            write (9000,*) "HYDIN_PATHS              hydin_paths_aa.txt"
            if (pco%csvout == "y") then
              open (2731,file="hydin_paths_aa.csv")
              write (9000,*) "HYDIN_PATHS            hydin_paths_aa.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2719,file="hydin_metals_aa.txt")
            write (9000,*) "HYDIN_METALS              hydin_metals_aa.txt"
            if (pco%csvout == "y") then
              open (2735,file="hydin_metals_aa.csv")
              write (9000,*) "HYDIN_METALS              hydin_metals_aa.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2723,file="hydin_salts_aa.txt")
            write (9000,*) "HYDIN_SALTS              hydin_salts_aa.txt"
            if (pco%csvout == "y") then
              open (2739,file="hydin_salts_aa.csv")
              write (9000,*) "HYDIN_SALTS              hydin_salts_aa.csv"
            end if
        end if
     end if         !! end average annual - HYDIN

     
          
     !! daily - HYDOUT
     if (pco%hyd%d == "y") then   
        if (cs_db%num_pests > 0) then
          open (2740,file="hydout_pests_day.txt")
            write (9000,*) "HYDOUT_PESTS              hydout_pests_day.txt"
            if (pco%csvout == "y") then
              open (2756,file="hydout_pests_day.csv")
              write (9000,*) "HYDOUT_PESTS              hydout_pests_day.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2744,file="hydout_paths_day.txt")
            write (9000,*) "HYDOUT_PATHS              hydout_paths_day.txt"
            if (pco%csvout == "y") then
              open (2760,file="hydout_paths_day.csv")
              write (9000,*) "HYDOUT_PATHS            hydout_paths_day.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2748,file="hydout_metals_day.txt")
            write (9000,*) "HYDOUT_METALS              hydout_metals_day.txt"
            if (pco%csvout == "y") then
              open (2764,file="hydout_metals_day.csv")
              write (9000,*) "HYDOUT_METALS              hydout_metals_day.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2752,file="hydout_salts_day.txt")
            write (9000,*) "HYDOUT_SALTS              hydout_salts_day.txt"
            if (pco%csvout == "y") then
              open (2768,file="hydout_salts_day.csv")
              write (9000,*) "HYDOUT_SALTS              hydout_salts_day.csv"
            end if
        end if
     end if      !! end daily - HYDOUT

     !! monthly - HYDOUT
     if (pco%hyd%m == "y") then   
        if (cs_db%num_pests > 0) then
          open (2741,file="hydout_pests_mon.txt")
            write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.txt"
            if (pco%csvout == "y") then
              open (2757,file="hydout_pests_mon.csv")
              write (9000,*) "HYDOUT_PESTS              hydout_pests_mon.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2745,file="hydout_paths_mon.txt")
            write (9000,*) "HYDOUT_PATHS              hydout_paths_mon.txt"
            if (pco%csvout == "y") then
              open (2761,file="hydout_paths_mon.csv")
              write (9000,*) "HYDOUT_PATHS            hydout_paths_mon.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2749,file="hydout_metals_mon.txt")
            write (9000,*) "HYDOUT_METALS              hydout_metals_mon.txt"
            if (pco%csvout == "y") then
              open (2765,file="hydout_metals_mon.csv")
              write (9000,*) "HYDOUT_METALS              hydout_metals_mon.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2753,file="hydout_salts_mon.txt")
            write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.txt"
            if (pco%csvout == "y") then
              open (2769,file="hydout_salts_mon.csv")
              write (9000,*) "HYDOUT_SALTS              hydout_salts_mon.csv"
            end if
        end if
     end if         !! end monthly - HYDOUT

     
     !! yearly - HYDOUT
     if (pco%hyd%y == "y") then   
        if (cs_db%num_pests > 0) then
          open (2742,file="hydout_pests_yr.txt")
            write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.txt"
            if (pco%csvout == "y") then
              open (2758,file="hydout_pests_yr.csv")
              write (9000,*) "HYDOUT_PESTS              hydout_pests_yr.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2746,file="hydout_paths_yr.txt")
            write (9000,*) "HYDOUT_PATHS              hydout_paths_yr.txt"
            if (pco%csvout == "y") then
              open (2762,file="hydout_paths_yr.csv")
              write (9000,*) "HYDOUT_PATHS            hydout_paths_yr.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2750,file="hydout_metals_yr.txt")
            write (9000,*) "HYDOUT_METALS              hydout_metals_yr.txt"
            if (pco%csvout == "y") then
              open (2766,file="hydout_metals_yr.csv")
              write (9000,*) "HYDOUT_METALS              hydout_metals_yr.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2754,file="hydout_salts_yr.txt")
            write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.txt"
            if (pco%csvout == "y") then
              open (2770,file="hydout_salts_yr.csv")
              write (9000,*) "HYDOUT_SALTS              hydout_salts_yr.csv"
            end if
        end if
     end if         !! end yearly - HYDOUT

     !! average annual - HYDOUT
     if (pco%hyd%a == "y") then   
        if (cs_db%num_pests > 0) then
          open (2743,file="hydout_pests_aa.txt")
            write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.txt"
            if (pco%csvout == "y") then
              open (2759,file="hydout_pests_aa.csv")
              write (9000,*) "HYDOUT_PESTS              hydout_pests_aa.csv"
            end if
        end if

        if (cs_db%num_paths > 0) then
          open (2747,file="hydout_paths_aa.txt")
            write (9000,*) "HYDOUT_PATHS              hydout_paths_aa.txt"
            if (pco%csvout == "y") then
              open (2763,file="hydout_paths_aa.csv")
              write (9000,*) "HYDOUT_PATHS            hydout_paths_aa.csv"
            end if
        end if
        
        if (cs_db%num_metals > 0) then
          open (2751,file="hydout_metals_aa.txt")
            write (9000,*) "HYDOUT_METALS              hydout_metals_aa.txt"
            if (pco%csvout == "y") then
              open (2767,file="hydout_metals_aa.csv")
              write (9000,*) "HYDOUT_METALS              hydout_metals_aa.csv"
            end if
        end if
        
        if (cs_db%num_salts > 0) then
          open (2755,file="hydout_salts_aa.txt")
            write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.txt"
            if (pco%csvout == "y") then
              open (2771,file="hydout_salts_aa.csv")
              write (9000,*) "HYDOUT_SALTS              hydout_salts_aa.csv"
            end if
        end if
      end if         !! end average annual - HYDOUT     
     
      return
      end subroutine header_cs