      module tiles_data_module
    
      implicit none
     
        type subsurface_drainage
        character(len=13) :: name = "default"
        real :: depth = 0.    !! |mm            |depth of drain tube from the soil surface
        real :: time = 0.     !! |hrs           |time to drain soil to field capacity
        real :: lag = 0.      !! |hours         |drain tile lag time
        real :: radius =0.    !! |mm		       effective radius of drains
        real :: dist = 0.     !! |mm            |distance between two drain tubes or tiles
        real :: drain_co      !! |mm/day        |drainage coefficient 
        real :: pumpcap = 0.  !! |mm/hr         |pump capacity (default pump capacity = 1.042mm/hr or 25mm/day)
        real :: latksat = 0.  !! |none          |multiplication factor to determine conk(j1,j) from sol_k(j1,j) for HRU 
      end type subsurface_drainage
      type (subsurface_drainage), dimension (:), allocatable :: sdr
       
      type pothole_db
          character(len=13) :: name
          real :: frac  = 0.       !! km2/km2       |frac of HRU area that drains into pothole
          real :: tilemm = 0.      !! m3/d          |average daily outflow to main channel from tile flow
                                   !!                   if drainage tiles are installed in pothole
          real :: volxmm = 0.      !! m**3 H2O      |max volume of water stored in the depression/imp area
          real :: volmm = 0.       !! m**3 H2O      |current volume of water stored in the depression/imp area 
          real :: nsed = 0.        !! mg/L          |normal sed conc in impounded water
          real :: no3l = 0.        !! 1/day         |nitrate decay rate in impounded area
          real :: solpl = 0.       !! kg N          |amount of soluble p in pothold water body
          real :: k = -1.          !! mm/hr         |saturated hydraulic condcutivity of soil layer
      end type pothole_db
      type (pothole_db), dimension (:), allocatable :: potdb  
         
      end module tiles_data_module 