      module plant_parameter_module
    
      type plant_growth
         character(len=4) :: cpnm       !! N/A          4 letter char code represents crop name 
         real :: cht = 0.               !! m            canopy height 
         real :: lai = 0.               !! m**2/m**2    leaf area index
         real :: yield = 0.             !! kg/ha        land cover/crop yield (dry weight)
         real :: plet = 0.              !! mm H2O       actual ET simulated during life of plant
         real :: plpet = 0.             !! mm H2O       potential ET simulated during life of plant
         real :: laimxfr = 0.           !! 
         real :: hvstiadj = 0.          !! (kg/ha)/(kg/ha)  optimal harvest index for current time during growing season
         real :: olai = 0.              !!
         real :: rwt = 0.               !! none         fraction of total plant biomass that is in roots
         real :: bio_leaf = 0.          !! none         fraction of above ground tree biomass that is leaf
         real, dimension(:),allocatable :: bac     !!             bacteria concentration on plant
      end type plant_growth
      
      type plant_mass
        character(len=4) :: cpnm      !! N/A              |4 letter char code represents organic name
        real :: mass = 0.             !!kg/ha             |biomass
        real :: cmass = 0.            !!kg/ha             |carbon mass
        real :: nmass = 0.            !!kg/ha             |nitrogen mass
        real :: pmass = 0.            !!kg/ha             |phosphorus mass
        real :: c_fr = 0.             !!none              |carbon fraction
        real :: n_fr = 0.             !!none              |nitrogen fraction
        real :: p_fr = 0.             !!none              |phosphorus fraction
      end type plant_mass
!      type (plant_mass) :: plt_mass_z
      type (plant_mass) :: yld_tbr
      type (plant_mass) :: yld_grn
      type (plant_mass) :: yld_veg
      type (plant_mass) :: yld_rsd
      type (plant_mass), pointer :: pl_tot
      type (plant_mass), pointer :: veg_ag
      type (plant_mass), pointer :: grain
      type (plant_mass), pointer :: root
      type (plant_mass), pointer :: rsd_flt
      
      type plant_status
        integer :: idplt = 0           !! none         land cover code from plants.plt
        integer :: gro = 0             !! none         land cover status code 
                                       !!                0 = no land cover currently growing 
                                       !!                1 = land cover growing
        integer :: idorm = 0           !! none         dormancy status code; 0=land cover growing 1=land cover dormant
        real :: phumat = 0.            !! C            heat units to maturity
        real :: phuacc = 0.            !! fraction     fraction of plant heatunit accumulated
        real :: laimx_pop = 0.         !!
        integer :: harv_num = 0        !!              number of harvest operations
        integer :: curyr_mat = 0.      !! 
        real :: pop_com = 0.           !! none 
        integer :: monsoon_init = 0    !! julian day   monsoon initiation period
      end type plant_status
      
      type plant_stress
        real :: strsw = 1.             !! none         frac of potential plant growth achieved on the day where the
                                       !!                reduction is caused by water stress
        real :: strsa = 1.             !!              frac of potential plant growth achieved on the day where the
                                       !!                reduction is caused by air stress
        real :: strsn = 1.             !! none         frac of potential plant growth achieved on the day where the reduction
                                       !!                is caused by nit stress
        real :: strsp = 1.             !! none         frac of potential plant growth achieved on the day where the reduction 
                                       !!                is caused by phos stress
        real :: strst = 1.             !! none         frac of potential plant growth achieved on the day where the reduction
                                       !!                is caused by temp stress   
      end type plant_stress
      
      type plant_community
       character(len=4) :: name
       integer :: mseas = 0            !! none         monsoon season to initiate tropical plant growth
                                       !!                0 = outside monsoon period and during monsoon after growth is triggered
                                       !!                1 = in monsoon period but new growth not triggered
       type (plant_growth), dimension(:), allocatable :: plg
       type (plant_mass), dimension(:), allocatable :: plm
       type (plant_stress), dimension(:), allocatable :: plstr
       type (plant_status), dimension(:), allocatable :: plcur
      end type plant_community
      type (plant_community), dimension (:), allocatable :: pcom

      end module plant_parameter_module