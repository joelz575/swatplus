      module reservoir_data_module
 
    type reservoir_initial
        character(len=16) :: name
        real :: vol = 0.          !m**3          |res vol (read in as frac principal and converted to m^3)
        real :: sed = 0.          !kg/L          |amt of sed in res (read in as mg/L and converted to kg/L)
        real :: orgn = 0.         !kg N          |amt of org N in res (read in as mg/L and converted to kg/L)
        real :: no3 = 0.          !kg N          |amt of nitrate in res (read in as mg/L and converted to kg/L)
        real :: no2 = 0.          !kg N          |amt of nitrite in res (read in as mg/L and converted to kg/L)
        real :: nh3 = 0.          !kg N          |amt of ammonia in res (read in as mg/L and converted to kg/L)
        real :: orgp = 0.         !kg P          |amt of org P in res (read in as mg/L and converted to kg/L)
        real :: solp = 0.         !kg P          |amt of soluble P in res (read in as mg/L and converted to kg/L)
        real :: seci = 0.         !m             |secchi-disk depth 
        real :: san = 0.          !kg/L          |amt of san in res (read in as mg/L and converted to kg/L)
        real :: sil = 0.          !kg/L          |amt of sil in res (read in as mg/L and converted to kg/L)
        real :: cla = 0.          !kg/L          |amt of cla in res (read in as mg/L and converted to kg/L)
        real :: sag = 0.          !kg/L          |amt of sag in res (read in as mg/L and converted to kg/L)
        real :: lag = 0.          !kg/L          |amt of lag in res (read in as mg/L and converted to kg/L)
        real :: gra = 0.          !kg/L          |amt of gra in res (read in as mg/L and converted to kg/L)
        real :: chla = 0.         !kg chl-a      |amt of chlorophyll-a in res
        real :: psol = 0.         !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: psor = 0.         !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: bactlp = 0.       !# cfu/100ml   |less persistent bacteria stored in res
        real :: bactp = 0.        !# cfu/100ml   |persistent bacteria stored in res
      end type reservoir_initial
      type (reservoir_initial), dimension(:),allocatable :: res_init
      type (reservoir_initial) :: resz
      
      type reservoir_data_char_input
        character (len=16) :: name = "default"
        character (len=16) :: init                  !initial data-points to initial.res
        character (len=16) :: hyd                   !points to hydrology.res for hydrology inputs
        character (len=16) :: release               !0=simulated; 1=measured outflow
        character (len=16) :: sed                   !sediment inputs-points to sediment.res
        character (len=16) :: nut                   !nutrient inputs-points to nutrient.res
        character (len=16) :: pst                   !pesticide inputs-points to pesticide.res      
      end type reservoir_data_char_input
      type (reservoir_data_char_input), dimension(:), allocatable :: res_dat_c
      type (reservoir_data_char_input), dimension(:), allocatable :: wet_dat_c

      type reservoir_data
        character(len=16) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: release = 0                !0=simulated; 1=measured outflow
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
        integer :: pst = 0                    !pesticide inputs-points to pesticide.res
      end type reservoir_data
      type (reservoir_data), dimension(:), allocatable :: res_dat
      type (reservoir_data), dimension(:), allocatable :: wet_dat
      type (reservoir_data) :: res_datz
      
      type reservoir_hyd_data
        character(len=16) :: name = "default"
        integer :: iyres = 0      !none          |year of the sim that the res becomes operational
        integer :: mores = 0      !none          |month the res becomes operational
        real :: psa = 0.          !ha            |res surface area when res is filled to princ spillway
        real :: pvol = 0.         !ha-m          |vol of water needed to fill the res to the princ spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: esa = 0.          !ha            |res surface area when res is filled to emerg spillway 
        real :: evol = 0.         !ha-m          |vol of water needed to fill the res to the emerg spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: k = .01           !mm/hr         |hydraulic conductivity of the res bottom
        real :: evrsv = .7        !none          |lake evap coeff
        real :: br1 = 0.          !none          |vol-surface area coefficient for reservoirs (model estimates if zero)
        real :: br2 = 0.          !none          |vol-surface area coefficient for reservoirs (model estimates if zero)
      end type reservoir_hyd_data
      type (reservoir_hyd_data), dimension(:), allocatable :: res_hyd
      
      type wetland_hyd_data
        character(len=16) :: name = "default"
        real :: psa = 0.          !frac          |fraction of hru area at principal spillway (ie: when surface inlet riser flow starts)
        real :: pvol = 0.         !mm            |average depth of water at principal spillway
        real :: esa = 0.          !frac          |fraction of hru area at emergency spillway (ie: when starts to spill into ditch)
        real :: evol = 0.         !mm            |average depth of water at emergency spillway
        real :: k = .01           !mm/hr         |hydraulic conductivity of the res bottom
        real :: evrsv = .7        !none          |lake evap coeff
        real :: acoef = 1.        !none          |vol-surface area coefficient for hru impoundment
        real :: bcoef = 1         !none          |vol-depth coefficient for hru impoundment
        real :: ccoef = 1         !none          |vol-depth coefficient for hru impoundment
        real :: frac = .5         !none          |fraction of hru that drains into impoundment
      end type wetland_hyd_data
      type (wetland_hyd_data), dimension(:), allocatable :: wet_hyd
      
      type reservoir_sed_data
        character(len=16) :: name
        real :: nsed              !kg/L          |normal amt of sed in res (read in as mg/L and convert to kg/L)
        real :: d50               ! 
        real :: sed_stlr          !none          |sed settling rate
        real :: velsetlr
      end type reservoir_sed_data
      type (reservoir_sed_data), dimension(:), allocatable :: res_sed
            
      type reservoir_nut_data
        character(len=16) :: name
        integer :: ires1          !none          |beg of mid-year nutrient settling "season"
        integer :: ires2          !none          |end of mid-year nutrient settling "season"
        real :: nsetlr1               !m/day         |nit settling rate for mid-year period (read in as m/year and converted to m/day)
        real :: nsetlr2               !m/day         |nit settling rate for remainder of year (read in as m/year and converted to m/day)
        real :: psetlr1               !m/day         |phos settling rate for mid-year period (read in as m/year and converted to m/day)
        real :: psetlr2               !m/day         |phos settling rate for remainder of year (read in as m/year and converted to m/day)
        real :: chlar = 1.            !none          |chlorophyll-a production coeff for res
        real :: seccir = 1.0          !none          |water clarity coeff for res
        real :: theta_n = 1.          !none          |temperature adjustment for nitrogen loss (settling)
        real :: theta_p = 1.          !none          |temperature adjustment for phosphorus loss (settling)
        real :: conc_nmin = .1        !ppm           |minimum nitrogen concentration for settling
        real :: conc_pmin = .01       !ppm           |minimum phosphorus concentration for settling
      end type reservoir_nut_data
      type (reservoir_nut_data), dimension(:), allocatable :: res_nut
          
      type reservoir_pst_data
        character(len=16) :: name
        real :: pst_conc = 0.         !mg/m^3        |pest concentration in lake water
        real :: pst_koc = 0.          !m**3/g        |pest partition coeff between water and sed in lake water
        real :: pst_mix = 0.          !m/day         |mixing velocity (diffusion/dispersion)in lake water for pest
        real :: pst_rea = .007        !1/day         |pest reaction coeff in lake water
        real :: pst_rsp = .002        !m/day         |resuspension velocity in lake water for pest sorbed to sed
        real :: pst_stl = 1.0         !m/day         |settling velocity in lake water for pest sorbed to sed
        real :: pst_vol = .01         !m/day         |pest volatilization coeff in lake water
        real :: spst_act = .03        !m             |depth of active sed layer in lake for pest
        real :: spst_bry = .002       !m/day         |pest burial velocity in lake bed sed
        real :: spst_conc = 0.        !mg/m^3        |pest concentration in lake bed sed
        real :: spst_rea = .05        !1/day         |pest reaction coeff in lake bed sed
      end type reservoir_pst_data
      type (reservoir_pst_data), dimension(:), allocatable :: res_pst
            
      type reservoir_weir_outflow
        character(len=16) :: name
        real :: num_steps = 24        !              |number of time steps in day for weir routing
        real :: c = 1.                !              |weir discharge coefficient 
        real :: k = 150000.           !m^1/2 d^-1    |energy coefficient (broad_crested=147,000' sharp crested=153,000)
        real :: w = 2.                !(m)           |width
        real :: bcoef = 1.75          !              |velocity exponent coefficient for bedding material
        real :: ccoef = 1.            !              |depth exponent coefficient for bedding material
      end type reservoir_weir_outflow
      type (reservoir_weir_outflow),dimension(:),allocatable :: res_weir    
    
      end module reservoir_data_module 