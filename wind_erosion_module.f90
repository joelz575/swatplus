      module wind_erosion_module

      use parm
      use climate_parms
      use jrw_datalib_module
      
      type wind_erosion_factors
        real :: bare             !!t/d      |erosion from bare soil
        real :: veg              !!         |vegetation factor
        real :: rough            !!         |roughness factor
        real :: unshelt          !!         |unsheltered distance factor
        real :: erod             !!         |wind erodibility factor
      end type wind_erosion_factors
      type (wind_erosion_factors) :: wind_factors
         
      contains
        
      !! routines for shallow aquifer module
      include 'wind_ero_control.f90'
      include 'wind_ero_bare.f90'
      include 'wind_ero_veg.f90'
      include 'wind_ero_rough.f90'
      include 'wind_ero_unshelt.f90'
      include 'wind_ero_erod.f90'
   
      end module wind_erosion_module