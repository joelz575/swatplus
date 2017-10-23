      module bacteria_module
    
      use jrw_datalib_module
    
      type bacteria_db
        character(len=15) :: bactnm         
        real :: do_soln = 0.       !! 1/day         |Die-off factor for pers bac in soil solution
        real :: gr_soln = 0.       !! 1/day         |Growth factor for pers bac in soil solution
        real :: do_sorb = 0.       !! 1/day         |Die-off factor for pers bac adsorbed to soil part
        real :: gr_sorb = 0.       !! 1/day         |Growth factor for pers bac adsorbed to soil part
        real :: kd = 0.            !! none          |Bact part coeff bet sol and sorbed phase in surf runoff
        real :: t_adj = 0.         !! none          |temp adj factor for bac die-off/growth
        real :: washoff = 0.       !! none          |frac of pers bac on foliage washed off by a rainfall event
        real :: do_plnt = 0.       !! 1/day         |Die-off factor for pers bac on foliage
        real :: gr_plnt = 0.       !! 1/day         |Growth factor for persistent bacteria on foliage
        real :: fr_manure = 0.     !! none          |frac of manure containing active colony forming units (cfu)
        real :: perco = 0.         !! none          |bacteria perc coeff ratio of solution bacteria in surf layer
        real :: det_thrshd         !! # cfu/m^2     |Threshold detection level for less pers bac when bact levels
                                                    !drop to this amt the model considers bac in the soil to be 
                                                    !insignificant and sets the levels to zero
        real :: do_stream = 0.     !! 1/day         |Die-off factor for persistent bacteria in streams
        real :: gr_stream = 0.     !! 1/day         |growth factor for persistent bacteria in streams
        real :: do_res = 0.        !! 1/day         |Die-off factor for less persistent bacteria in reservoirs
        real :: gr_res = 0.        !! 1/day         |growth factor for less persistent bacteria in reservoirs
        real :: swf = 0.           !! cfu           |fraction of manure containing active colony forming units
        real :: conc_min
      end type bacteria_db
      type (bacteria_db), dimension(:), allocatable  :: bac_db

      contains
      !include 'bac_lsparms_read.f90' 
      
      end module bacteria_module 