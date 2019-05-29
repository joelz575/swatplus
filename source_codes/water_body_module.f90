      module water_body_module
    
      !! water body (reservoir, wetland, and channel) output not uncluded in hyd_output object

      type water_body
        real :: area_ha = 0.            !ha         |water body surface area
        real :: precip = 0.             !ha-m       |precip on the water body
        real :: evap = 0.               !ha-m       |evaporation from the water surface
        real :: seep = 0.               !ha-m       |seepage from bottom of water body
      end type water_body
      type (water_body) :: wbodz
      
      type (water_body), dimension(:),allocatable :: res_wat_d
      type (water_body), dimension(:),allocatable :: res_wat_m
      type (water_body), dimension(:),allocatable :: res_wat_y
      type (water_body), dimension(:),allocatable :: res_wat_a
      type (water_body), dimension(:),allocatable :: wet_wat_d
      type (water_body), dimension(:),allocatable :: wet_wat_m
      type (water_body), dimension(:),allocatable :: wet_wat_y
      type (water_body), dimension(:),allocatable :: wet_wat_a
      type (water_body), dimension(:),allocatable :: ch_wat_d
      type (water_body), dimension(:),allocatable :: ch_wat_m
      type (water_body), dimension(:),allocatable :: ch_wat_y
      type (water_body), dimension(:),allocatable :: ch_wat_a
      type (water_body) :: bch_wat_d
      type (water_body) :: bch_wat_m
      type (water_body) :: bch_wat_y
      type (water_body) :: bch_wat_a

       type water_body_hdr
        character (len=10) :: area_ha    = " surf_area"             !ha     |water body surface area
        character (len=10) :: precip     = "    precip"             !ha-m   |precip on the water body
        character (len=10) :: evap       = "      evap"             !ha-m   |evaporation from the water surface
        character (len=10) :: seep       = "      seep"             !ha-m   |seepage from bottom of water body
      end type water_body_hdr
      type (water_body_hdr) :: wbod_hdr

       type water_body_unit
        character (len=10) :: area_ha    = "        ha"             !ha     |water body surface area
        character (len=10) :: precip     = "      ha-m"             !ha-m   |precip on the water body
        character (len=10) :: evap       = "      ha-m"             !ha-m   |evaporation from the water surface
        character (len=10) :: seep       = "      ha-m"             !ha-m   |seepage from bottom of water body
      end type water_body_unit
      type (water_body_unit) :: wbod_hdr2

       interface operator (+)
        module procedure watbod_add
      end interface

      interface operator (/)
        module procedure watbod_div
      end interface   
      
      interface operator (//)
        module procedure watbod_ave
      end interface   
      
      contains
      
     !! routines for reservoir module
      function watbod_add (wbod1, wbod2) result (wbod3)
        type (water_body), intent (in) :: wbod1
        type (water_body), intent (in) :: wbod2
        type (water_body) :: wbod3
        wbod3%area_ha = wbod1%area_ha + wbod2%area_ha
        wbod3%precip = wbod1%precip + wbod2%precip
        wbod3%evap = wbod1%evap + wbod2%evap        
        wbod3%seep = wbod1%seep + wbod2%seep   
      end function watbod_add
      
      function watbod_div (wbod1,const) result (wbod2)
        type (water_body), intent (in) :: wbod1
        real, intent (in) :: const
        type (water_body) :: wbod2
        wbod2%area_ha = wbod1%area_ha
        wbod2%precip = wbod1%precip / const
        wbod2%evap = wbod1%evap / const
        wbod2%seep = wbod1%seep / const
      end function watbod_div
            
      function watbod_ave (wbod1,const) result (wbod2)
        type (water_body), intent (in) :: wbod1
        real, intent (in) :: const
        type (water_body) :: wbod2
        wbod2%area_ha = wbod1%area_ha / const
        wbod2%precip = wbod1%precip
        wbod2%evap = wbod1%evap
        wbod2%seep = wbod1%seep
      end function watbod_ave
      
      end module water_body_module