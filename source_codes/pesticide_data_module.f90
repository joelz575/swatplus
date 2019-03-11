      module pesticide_data_module
    
      implicit none    
          
      type pesticide_db
        character(len=16) :: name   !!                      |pesticide name
        real :: koc = 0.            !! (mg/kg)/(mg/L)       |soil adsorption coeff normalized for soil org carbon content
        real :: washoff = 0.        !! none                 |frac of pesticide on foliage which is washed off by rainfall event 
        real :: foliar_hlife = 0.   !! days                 |half-life of pest on foliage
        real :: soil_hlife = 0.     !! days                 |half-life of pest in soil
        real :: solub = 0.          !! mg/L (ppm)           |solubility of chemical in water
        real :: aq_reac = 0.        !! 1/day                |aquatic pesticide reaction coeff
        real :: aq_volat = 0.       !! m/day                |aquatic volatilization coeff
        real :: mol_wt = 0.         !! ??                   |molecular weight - to calulate mixing velocity
        real :: aq_resus = 0.       !! m/day                |aquatic resuspension velocity for pesticide sorbed to sediment
        real :: aq_settle = 0.      !! m/day                |aquatic settling velocity for pesticide sorbed to sediment
        real :: ben_act_dep = 0.    !! m                    |depth of active benthic layer
        real :: ben_bury = 0.       !! m/day                |burial velocity in benthic sediment
        real :: ben_reac = 0.       !! 1/day                |reaction coeff in benthic sediment
        character(len=32) :: descrip                        !pesticide description
      end type pesticide_db
      type (pesticide_db), dimension(:),allocatable, save :: pestdb
      
      type pesticide_cp         !! calculated parameters from input parms
        real :: decay_f = 0.        !! none                 |exp of the rate const for degradation of the pest on foliage
        real :: decay_s = 0.        !! none                 |exp of the rate const for degradation of the pest in soil
      end type pesticide_cp
      type (pesticide_cp), dimension(:),allocatable, save:: pestcp
      
      end module pesticide_data_module 