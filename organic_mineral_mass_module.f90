      module organic_mineral_mass_module
    
      type organic_mass
        real :: m = 0.              !kg or kg/ha      |total object mass
        real :: c = 0.              !kg or kg/ha      |carbon mass
        real :: n = 0.              !kg or kg/ha      |organic nitrogen mass
        real :: p = 0.              !kg or kg/ha      |organic phosphorus mass
      end type organic_mass
      type (organic_mass)  :: om_mass_z
      
      !!new stuff
      type clay_mass
        real :: m               !kg or kg/ha      |total object mass
        real :: nh4             !kg or kg/ha      |ammonium mass
      end type clay_mass
      
      type sediment
        real :: m                   !kg or kg/ha      |total object mass
        real :: sand                !kg or kg/ha      |sand mass
        real :: silt                !kg or kg/ha      |silt mass
        type (clay_mass) :: clay    !kg or kg/ha      |clay mass
        real :: gravel              !kg or kg/ha      |gravel mass
      end type sediment
      
      type soil_profile_mass1
        character (len=16) :: name
        real, dimension(:), allocatable :: sw                       !mm     |soil water dimensioned by layer
        real, dimension(:), allocatable :: no3                      !kg/ha  |nitrate dimensioned by layer
        real, dimension(:), allocatable :: nh4                      !kg/ha  |ammonium dimensioned by layer
        real, dimension(:), allocatable :: po4                      !kg/ha  |phosphate dimensioned by layer
        type (sediment), dimension(:), allocatable :: sed           !       |sediment
        type (organic_mass), dimension(:), allocatable :: sta       !       |stable humus pool
        type (organic_mass), dimension(:), allocatable :: act       !       |active humus pool
      end type soil_profile_mass1
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      type (soil_profile_mass1), dimension(:), allocatable :: soil1
      
      type residue_mass1
        character (len=16) :: name
        type (organic_mass), dimension(:), allocatable :: man       !kg/ha            |manure (by soil layer)
        type (organic_mass), dimension(:), allocatable :: rsd_fl    !kg/ha            |flat residue on soil surface for individual plant in community
        type (organic_mass), dimension(:), allocatable :: rsd_st    !kg/ha            |standing residue for individual plant in community
        type (organic_mass), dimension(:,:), allocatable :: rsd_s   !kg/ha            |root and incorporated residue for individual plant in community (by soil layer)
        type (organic_mass) :: rsd_tfl                              !kg/ha            |total flat residue on surface (all plants)
        type (organic_mass) :: rsd_tst                              !kg/ha            |total standing residue (all plants)
        type (organic_mass), dimension(:), allocatable :: rsd_ts    !kg/ha            |total residue in each soil layer
      end type residue_mass1
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      type (residue_mass1), dimension(:), allocatable :: rsd
      
      type plant_community_mass1
        character (len=4) :: name                                   !                 |same as plant_community object
        !live biomass
        type (organic_mass), dimension(:), allocatable :: tot    !kg/ha            |total biomass for individual plant in community
        type (organic_mass), dimension(:), allocatable :: veg    !kg/ha            |vegetative mass for individual plant in community
        type (organic_mass), dimension(:), allocatable :: grain  !kg/ha            |grain mass for individual plant in community
        type (organic_mass), dimension(:,:), allocatable :: root !kg/ha            |root mass for individual plant in community (by soil layer)
        type (organic_mass) :: tot_com                           !kg/ha            |total biomass for entire community
        type (organic_mass) :: veg_com                           !kg/ha            |vegetative mass for entire community
        type (organic_mass) :: root_com                          !kg/ha            |root mass for entire community for entire soil profile
        type (organic_mass) :: grain_com                         !kg/ha            |grain mass for entire community
      end type plant_community_mass1
      !plant community object - dimensioned to number of hrus, using the hru pointer
      type (plant_community_mass1), dimension(:), allocatable :: plnt1
            
      type organic_mineral_hydrograph1
        real :: vol = 0.                    ! m^3           |volume of water
        type (sediment) :: sed              !               |sediment
        type (organic_mass) :: algae        !               |algae
        type (organic_mass) :: biofilm      !               |biofilm
        real :: chla = 0.                   ! kg            |chlorophyll-a
        real :: cbod = 0.                   ! kg            |carbonaceous biological oxygen demand
        real :: dox = 0.                    ! kg            |dissolved oxygen
        real :: temp = 0.                   ! deg c         |temperature
      end type organic_mineral_hydrograph1
      
      !!end of new stuff
      
      
      type mineral_mass
        real :: m               !kg or kg/ha      |total object mass
        real :: no3             !kg or kg/ha      |nitrate mass
        real :: no2             !kg or kg/ha      |nitrite mass
        real :: nh4             !kg or kg/ha      |ammonium mass  
        real :: po4             !kg or kg/ha      |phosphate mass 
      end type mineral_mass

      type organic_mineral_mass
        real :: vol
        type (organic_mass) :: hum
        type (organic_mass) :: hum_act
        type (mineral_mass) :: min
      end type organic_mineral_mass
      
      !type soil_profile_mass
      !  character (len=16) :: name
      !  type (organic_mineral_mass), dimension(:), allocatable :: sol       !soil matrix dimensioned by layer
      !  type (organic_mineral_mass), dimension(:), allocatable :: sw        !soil water dimensioned by layer
     ! end type soil_profile_mass
      !soil profile object - dimensioned to number of hrus, using the hru pointer
      !type (soil_profile_mass), dimension(:), allocatable :: soil
      
      type plant_community_mass
        character (len=4) :: name                           !                 |same as plant_community object
        !live biomass
        type (organic_mass), dimension(:), allocatable :: tot    !kg/ha            |total biomass for individual plant in community
        type (organic_mass), dimension(:), allocatable :: veg    !kg/ha            |vegetative mass for individual plant in community
        type (organic_mass), dimension(:,:), allocatable :: root !kg/ha            |root mass for individual plant in community (by soil layer)
        type (organic_mass), dimension(:), allocatable :: grain  !kg/ha            |grain mass for individual plant in community
        type (organic_mass) :: tot_com                           !kg/ha            |total biomass for entire community
        type (organic_mass) :: veg_com                           !kg/ha            |vegetative mass for entire community
        type (organic_mass) :: root_com                          !kg/ha            |root mass for entire community
        type (organic_mass) :: grain_com                         !kg/ha            |grain mass for entire community
        !dead biomass - residue
        type (organic_mass), dimension(:,:), allocatable :: rsd  !kg/ha            |flat residue for individual plant in community (by soil layer)
        type (organic_mass), dimension(:), allocatable :: rsd_st !kg/ha            |standing residue for individual plant in community
      end type plant_community_mass
      !plant community object - dimensioned to number of hrus, using the hru pointer
      type (plant_community_mass), dimension(:), allocatable :: plnt
      
      !hru will point diretly to herds - managed in schedule_ops and ultimately can be managed in conditional subroutine
      !herds are different from soil and plant in that they can move from hru to hru
      type animal_herds
        character(len=16) :: name                                           !           |herd name (small_dairy, )
        integer :: num_tot                                                  !           |total number of animals in the here
        type (organic_mass) :: herd_mass                                    !kg         |total mass of herd
        character(len=16), dimension(:), allocatable :: typ                 !           |animal type (points to animal.hrd)
        integer, dimension(:), allocatable :: num                           !           |number of each type of animal
        type (organic_mass), dimension(:), allocatable :: mass              !           |mass of each type of animal
        type (organic_mass), dimension(:), allocatable :: eat               !           |biomass eaten by each type of animal
        type (organic_mineral_mass), dimension(:), allocatable :: manure    !           |manure from each type of animal
      end type animal_herds
      
      !fertilizer object      
      type fertilizer_mass
        character (len=16) :: name
        type (mineral_mass) :: org       !soil matrix dimensioned by layer
        type (organic_mass) :: min       !soil water dimensioned by layer
      end type fertilizer_mass
      !fertilizer object should be used as database input from fert.dat
      type (fertilizer_mass), dimension(:), allocatable :: fert         !dimension to number of fertilzers in database
      
      !manure object should be used as database input from manure.dat
      type (organic_mineral_mass), dimension(:), allocatable :: manure  !dimension to number of manures in database
      
      type organic_mineral_hydrograph
        real :: flo = 0.               !! m^3          |volume of water
        real :: sed = 0.               !! metric tons  |sediment
        type (organic_mass) :: org
        type (mineral_mass) :: min
        real :: chla = 0.              !! kg           |chlorophyll-a
        real :: cbod = 0.              !! kg           |carbonaceous biological oxygen demand
        real :: dox = 0.               !! kg           |dissolved oxygen
        real :: temp = 0.              !! deg c        |temperature
        real :: san = 0.               !! tons         |detached sand
        real :: sil = 0.               !! tons         |detached silt
        real :: cla = 0.               !! tons         |detached clay
        real :: sag = 0.               !! tons         |detached small ag
        real :: lag = 0.               !! tons         |detached large ag
        real :: grv = 0.               !! tons         |gravel
      end type organic_mineral_hydrograph
      
      type spatial_object_hydrographs
        character (len=16) :: name                                      !should match the object_connectivity object
        !water and soluble components
        type (organic_mineral_hydrograph) :: hin                                 !inflow hydrograph for surface runon - sum of all inflow hyds
        type (organic_mineral_hydrograph) :: hin_s                               !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hd        !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (organic_mineral_hydrograph), dimension(:,:),allocatable :: ts      !subdaily hydrographs
        type (organic_mineral_hydrograph), dimension(:),allocatable :: tsin      !inflow subdaily hydrograph
        !sediment (sorbed) in the water components
        type (organic_mineral_hydrograph) :: hins                                 !inflow hydrograph for surface runon - sum of all inflow hyds
        type (organic_mineral_hydrograph) :: hin_ss                               !inflow hydrograph for lateral soil flow - sum of all lateral inflow hyds
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hds        !generated hydrograph (ie 1=tot, 2= recharge, 3=surf, etc)
        type (organic_mineral_hydrograph), dimension(:,:),allocatable :: tss      !subdaily hydrographs
        type (organic_mineral_hydrograph), dimension(:),allocatable :: tsins      !inflow subdaily hydrograph
        !hydrograph output variables
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_m
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_y
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hin_a
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_m
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_y
        type (organic_mineral_hydrograph), dimension(:),allocatable :: hout_a
        type (organic_mineral_hydrograph) :: hdep_m
        type (organic_mineral_hydrograph) :: hdep_y
        type (organic_mineral_hydrograph) :: hdep_a
      end type spatial_object_hydrographs
      !track spatial_object_hydrographs with ob - use same pointer
      type (spatial_object_hydrographs), dimension(:),allocatable :: obom
      
      !recall organic-mineral inputs
      type recall_organic_mineral_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (organic_mineral_hydrograph), dimension (:,:), allocatable :: hd_om     !export coefficients
      end type recall_organic_mineral_inputs
      type (recall_organic_mineral_inputs),dimension(:),allocatable:: rec_om

      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: exco_om
      
      !export coefficient and delivery ratio pesticides
      type (organic_mineral_hydrograph), dimension(:,:), allocatable :: dr_om
      
      type subbasin_elements_hydrographs
        character (len=16) :: name                                   !should match the object_connectivity object
        type (organic_mineral_mass), dimension(:), allocatable :: hd
      end type subbasin_elements_hydrographs
      !point to subbasin element objects - same as sub_elem
      type (subbasin_elements_hydrographs), dimension(:), allocatable :: sub_e_hd
      
      type channel_surface_elements_hydrographs
        character (len=16) :: name                                   !should match the channel_surface_elements object
        type (organic_mineral_mass), dimension(:), allocatable :: hd
      end type channel_surface_elements_hydrographs
      !point to channel-surface objects - same as ch_sur
      type (channel_surface_elements_hydrographs), dimension(:), allocatable :: ch_sur_hd
      
      !objects needed for operators
      type (organic_mineral_mass) :: o_m1, o_m2, o_m3
      type (organic_mineral_mass) :: o1, o2, o3

      !we may also need operators for organic and mineral operations
      
      interface operator (+)
        module procedure o_add
      end interface
            
      interface operator (**)
        module procedure om_mult
        end interface 
      
      interface operator (.add.)
        module procedure om_add_const
      end interface 

      interface operator (*)
        module procedure om_mult_const
      end interface 

      interface operator (/)
        module procedure om_div_const
      end interface   
             
      interface operator (//)
        module procedure om_div_conv
      end interface   
             
      contains
            
      !! function to convert concentration to mass
      subroutine om_convert (o_m1)
        type (organic_mineral_mass), intent (inout) :: o_m1
        ! m3/s to m3
        o_m1%vol = o_m1%vol * 86400.
        ! kg = ppm * m3 / 1000.
        o_m1%hum%m = o_m1%hum%m * o_m1%vol / 1000.
        o_m1%hum%c = o_m1%hum%c * o_m1%vol / 1000.
        o_m1%hum%n = o_m1%hum%n * o_m1%vol / 1000.
        o_m1%hum%p = o_m1%hum%p * o_m1%vol / 1000.
        o_m1%hum_act%m = o_m1%hum_act%m * o_m1%vol / 1000.
        o_m1%hum_act%c = o_m1%hum_act%c * o_m1%vol / 1000.
        o_m1%hum_act%n = o_m1%hum_act%n * o_m1%vol / 1000.
        o_m1%hum_act%p = o_m1%hum_act%p * o_m1%vol / 1000.
        o_m1%min%m = o_m1%min%m * o_m1%vol / 1000.
        o_m1%min%no3 = o_m1%min%no3 * o_m1%vol / 1000.
        o_m1%min%no2 = o_m1%min%no2 * o_m1%vol / 1000.
        o_m1%min%nh4 = o_m1%min%nh4 * o_m1%vol / 1000.
        o_m1%min%po4 = o_m1%min%po4 * o_m1%vol / 1000.
      end subroutine om_convert
         
      !! routines for hydrograph module
      function o_add (o1, o2) result (o3)
        type (organic_mass), intent (in) :: o1
        type (organic_mass), intent (in) :: o2
        type (organic_mass) :: o3
        o3%m = o1%m + o2%m
        o3%c = o1%c + o2%c
        o3%n = o1%n + o2%n
        o3%p = o1%p + o2%p
      end function o_add
            
      !! routines for hydrograph module
      function om_mult (o_m1, o_m2) result (o_m3)
        type (organic_mineral_mass), intent (in) :: o_m1
        type (organic_mineral_mass), intent (in) :: o_m2
        type (organic_mineral_mass) :: o_m3
        o_m3%vol = o_m1%vol * o_m2%vol
        o_m3%hum%m = o_m1%hum%m * o_m2%hum%m
        o_m3%hum%c = o_m1%hum%c * o_m2%hum%c
        o_m3%hum%n = o_m1%hum%n * o_m2%hum%n
        o_m3%hum%p = o_m1%hum%p * o_m2%hum%p
        o_m3%hum_act%m = o_m1%hum_act%m * o_m2%hum_act%m
        o_m3%hum_act%c = o_m1%hum_act%c * o_m2%hum_act%c
        o_m3%hum_act%n = o_m1%hum_act%n * o_m2%hum_act%n
        o_m3%hum_act%p = o_m1%hum_act%p * o_m2%hum_act%p
        o_m3%min%m = o_m1%min%m * o_m2%min%m
        o_m3%min%no3 = o_m1%min%no3 * o_m2%min%no3
        o_m3%min%no2 = o_m1%min%no2 * o_m2%min%no2
        o_m3%min%nh4 = o_m1%min%nh4 * o_m2%min%nh4
        o_m3%min%po4 = o_m1%min%po4 * o_m2%min%po4
      end function om_mult
            
      !! routines for hydrograph module
      function om_add_const (const, o_m1) result (o_m2)
        real, intent (in) :: const
        type (organic_mineral_mass), intent (in) :: o_m1
        type (organic_mineral_mass) :: o_m2
        o_m2%vol = const + o_m1%vol
        o_m2%hum%m = const + o_m1%hum%m
        o_m2%hum%c = const + o_m1%hum%c
        o_m2%hum%n = const + o_m1%hum%n
        o_m2%hum%p = const + o_m1%hum%p
        o_m2%hum_act%m = const + o_m1%hum_act%m
        o_m2%hum_act%c = const + o_m1%hum_act%c
        o_m2%hum_act%n = const + o_m1%hum_act%n
        o_m2%hum_act%p = const + o_m1%hum_act%p
        o_m2%min%m = const + o_m1%min%m
        o_m2%min%no3 = const + o_m1%min%no3
        o_m2%min%no2 = const + o_m1%min%no2
        o_m2%min%nh4 = const + o_m1%min%nh4
        o_m2%min%po4 = const + o_m1%min%po4
      end function om_add_const
      
      function om_mult_const (const, o_m1) result (o_m2)
        type (organic_mineral_mass), intent (in) :: o_m1
        real, intent (in) :: const
        type (organic_mineral_mass) :: o_m2
        o_m2%vol = const * o_m1%vol
        o_m2%hum%m = const * o_m1%hum%m
        o_m2%hum%c = const * o_m1%hum%c
        o_m2%hum%n = const * o_m1%hum%n
        o_m2%hum%p = const * o_m1%hum%p
        o_m2%hum_act%m = const * o_m1%hum_act%m
        o_m2%hum_act%c = const * o_m1%hum_act%c
        o_m2%hum_act%n = const * o_m1%hum_act%n
        o_m2%hum_act%p = const * o_m1%hum_act%p
        o_m2%min%m = const * o_m1%min%m
        o_m2%min%no3 = const * o_m1%min%no3
        o_m2%min%no2 = const * o_m1%min%no2
        o_m2%min%nh4 = const * o_m1%min%nh4
        o_m2%min%po4 = const * o_m1%min%po4
      end function om_mult_const
      
      function om_div_const (o_m1,const) result (o_m2)
        type (organic_mineral_mass), intent (in) :: o_m1
        real, intent (in) :: const
        type (organic_mineral_mass) :: o_m2
        o_m2%vol = o_m1%vol / const
        o_m2%hum%m = o_m1%hum%m / const
        o_m2%hum%c = o_m1%hum%c / const
        o_m2%hum%n = o_m1%hum%n / const
        o_m2%hum%p = o_m1%hum%p / const
        o_m2%hum_act%m = o_m1%hum_act%m / const
        o_m2%hum_act%c = o_m1%hum_act%c / const
        o_m2%hum_act%n = o_m1%hum_act%n / const
        o_m2%hum_act%p = o_m1%hum_act%p / const
        o_m2%min%m = o_m1%min%m / const
        o_m2%min%no3 = o_m1%min%no3 / const
        o_m2%min%no2 = o_m1%min%no2 / const
        o_m2%min%nh4 = o_m1%min%nh4 / const
        o_m2%min%po4 = o_m1%min%po4 / const
      end function om_div_const
            
      !function to convert m^3-> mm and kg(or t)->kg(or t)/ha
      function om_div_conv (o_m1,const) result (o_m2)
        type (organic_mineral_mass), intent (in) :: o_m1
        real, intent (in) :: const  !ha
        type (organic_mineral_mass) :: o_m2
        o_m2%vol = o_m1%vol / (10. * const)
        o_m2%hum%m = o_m1%hum%m / const
        o_m2%hum%c = o_m1%hum%c / const
        o_m2%hum%n = o_m1%hum%n / const
        o_m2%hum%p = o_m1%hum%p / const
        o_m2%hum_act%m = o_m1%hum_act%m / const
        o_m2%hum_act%c = o_m1%hum_act%c / const
        o_m2%hum_act%n = o_m1%hum_act%n / const
        o_m2%hum_act%p = o_m1%hum_act%p / const
        o_m2%min%m = o_m1%min%m / const
        o_m2%min%no3 = o_m1%min%no3 / const
        o_m2%min%no2 = o_m1%min%no2 / const
        o_m2%min%nh4 = o_m1%min%nh4 / const
        o_m2%min%po4 = o_m1%min%po4 / const
      end function om_div_conv
      
      !function to set dr to a constant
 !     function om_constant (o_m1, const)
 !       type (organic_mineral_mass) :: o_m1
 !       real, intent (in) :: const
 !       o_m1%vol = const
 !       o_m1%hum%m = const
 !       o_m1%hum%c = const
 !       o_m1%hum%n = const
 !       o_m1%hum%p = const
 !       o_m1%hum_act%m = const
 !       o_m1%hum_act%c = const
 !       o_m1%hum_act%n = const
 !       o_m1%hum_act%p = const
 !       o_m1%min%m = const
 !       o_m1%min%no3 = const
 !       o_m1%min%no2 = const
 !       o_m1%min%nh4 = const
 !       o_m1%min%po4 = const
 !     end function om_constant
      
      end module organic_mineral_mass_module 