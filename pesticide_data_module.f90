      module pesticide_data_module
    
      implicit none    
          
      type pesticide_db
        character(len=16) :: pestnm   !!                     |pesticide name
        real :: skoc = 0.             !! (mg/kg)/(mg/L)      |soil adsorption coeff normalized for soil org carbon content
        real :: pst_wof = 0.          !! none                |frac of pesticide on foliage which is washed off by rainfall event 
        real :: hlife_f = 0.          !! days                |half-life of pest on foliage
        real :: hlife_s = 0.          !! days                |half-life of pest in soil
        real :: ap_ef = 0.            !! none                |application efficiency (0-1) 
        real :: pst_wsol = 0.         !! mg/L (ppm)          |solubility of chemical in water
      end type pesticide_db
      type (pesticide_db), dimension(:),allocatable, save :: pestdb
      
      type pesticide_cp
        real :: decay_f = 0.          !! none                |exp of the rate const for degradation of the pest on foliage
        real :: decay_s = 0.          !! none                |exp of the rate const for degradation of the pest in soil
        integer :: nope  = 0          !! none                |seq number of pest       
      end type pesticide_cp
      type (pesticide_cp), dimension(:),allocatable, save:: pstcp
      
      end module pesticide_data_module 