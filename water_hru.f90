      subroutine water_hru    
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
  
        use hru_module, only : tmpav, ihru, hru_ra, albday, etday, pet_day 
        
        implicit none
        
        integer :: j               !none            |hru number
        real :: tmpk               !                | 
        real :: d                  !m               |depth of flow
        real :: gma                !kPa/deg C       |psychrometric constant
        real :: ho                 !none            |variable to hold intermediate calculation
                                   !                |result
        real :: pet_alpha          !none            |alpha factor in Priestley-Taylor PET equation       
        
	    j = ihru
!! if the HRU is water compute only pet and et
!! using Priestly-Taylor and a coefficient
        tmpk = 0.
        d = 0.
        gma = 0.
        ho = 0.
        albday = .08
        pet_alpha = 1.28
        tmpk = tmpav(j) + 273.15
        d = Exp(21.255 - 5304. / tmpk) * 5304. / tmpk ** 2
        gma = d / (d + .68)
        ho = hru_ra(j) * (1. - albday) / 2.44
        pet_day = pet_alpha * ho * gma
        etday = .7 * pet_day
      
      return
      end subroutine water_hru