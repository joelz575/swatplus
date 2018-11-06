       subroutine path_apply (frt_kg)
          
      !! calculate ground cover
      !! graze only if adequate biomass in HRU
            
!!    this subroutine applies pathogens leached to the plants and soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    fert_kg      |kg/ha         |manure applied

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : ipl, manure_kg
      use soil_module
      use plant_module
      use pathogen_data_module
      use output_ls_pathogen_module
      use constituent_mass_module

      implicit none
            
      real, intent (in)  :: frt_kg
      real :: frt_t        !          |
      real :: gc           !none      |fraction of ground covered by plant foliage
      real :: gc1          !          |
      integer :: ipath     !none      |counter
      integer :: ipath_db  !          |pathogen type from pathogens.pth data input file
      real :: j            !          |
      real :: path_kd

      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.
      gc1 = 1. - gc
      do ipath = 1, cs_db%num_paths
        ipath_db = cs_db%path_num(ipath)
        frt_t = path_db(ipath_db)%swf * frt_kg / 1000.
        !! add pathogens - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.
        hpath_bal(j)%path(ipath)%plant = gc * (soil(j)%ly(1)%bacsol(ipath) + soil(j)%ly(1)%bacsor(ipath))* frt_t * 100.
        hpath_bal(j)%path(ipath)%soil = gc1 * (soil(j)%ly(1)%bacsol(ipath) + soil(j)%ly(1)%bacsor(ipath))* frt_t * 100.        
          
        pcom(j)%path(ipath) = pcom(j)%path(ipath) + hpath_bal(j)%path(ipath)%plant

        path_kd = path_db(ipath_db)%kd
        soil(j)%ly(1)%bacsol(ipath) = path_kd * hpath_bal(j)%path(ipath)%soil + soil(j)%ly(1)%bacsol(ipath)
        soil(j)%ly(1)%bacsor(ipath) = (1. - path_kd) * hpath_bal(j)%path(ipath)%soil + soil(j)%ly(1)%bacsor(ipath)
      end do

      return
      
      end subroutine path_apply