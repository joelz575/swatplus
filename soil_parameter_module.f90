      module soil_parameter_module
    
      type soilayer
        real :: ec = 0.
        real :: cal = 0.
        real :: ph = 0.
        real :: alb = 0.         !! none          albedo when soil is moist
        real :: usle_k = 0.      !!               USLE equation soil erodibility (K) factor 
        real ::conk = 0.         !! mm/hr          lateral saturated hydraulic conductivity for each profile layer in a give HRU. 
        real ::flat = 0.         !! mm H2O         lateral flow storage array
        real ::pperco_sub = 0.   !!
        real ::hum = 0.          !! kg humus/ha    amount of organic matter in the soil layer classified as humic substances
        real :: prk = 0.         !! mm H2O         percolation from soil layer on current day
        real :: rsd = 0.         !! kg/ha          amount of organic matter in the soil classified as residue
        real :: volcr = 0.       !! mm             crack volume for soil layer 
        real :: n = 0.           !!
        real :: tillagef = 0. 
        real :: rtfr = 0.        !! none           root fraction
        real :: watp = 0.
        integer :: a_days = 0
        integer :: b_days = 0
        real :: psp_store = 0.
        real :: ssp_store = 0.    
        real :: hp = 0.          !!
        real :: hs = 0.          !!
        real :: bm = 0.          !!
        real :: percc = 0.       !!
        real :: latc = 0.        !!
        real :: vwt = 0.         !!
      end type soilayer
      type (soilayer), dimension(:), allocatable :: ly1
      
      type soil_physical_properties
        real :: d = 0.            !! mm            depth to bottom of soil layer
        real :: bd = 0.           !! Mg/m**3       bulk density of the soil
        real :: k = 0.            !! mm/hr         saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
        real :: clay = 0.         !! none          fraction clay content in soil material (UNIT CHANGE!)
        real :: silt = 0.         !! %             percent silt content in soil material
        real :: sand = 0.         !! none          fraction of sand in soil material
        real :: rock = 0.         !! %             percent of rock fragments in soil layer 
        real ::conv_wt = 0.       !! none          factor which converts kg/kg to kg/ha
        real ::crdep = 0.         !! mm            maximum or potential crack volume
        real ::awc = 0.           !! mm H20/mm     soil available water capacity of soil layer
        real ::fc = 0.           !! mm H2O         amount of water available to plants in soil layer at field capacity (fc - wp),Index:(layer,HRU)
        real ::hk = 0.           !! none           beta coefficent to calculate hydraulic conductivity
        real :: por = 0.         !! none           total porosity of soil layer expressed as a fraction of the total volume, Index:(layer,HRU)
        real :: st = 0.          !! mm H2O         amount of water stored in the soil layer on any given day (less wp water)
        real :: tmp = 0.         !! deg C          daily average temperature of second soil layer
        real :: ul = 0.          !! mm H2O         amount of water held in the soil layer at saturation (sat - wp water)
        real :: up = 0.          !! mm H2O/mm soil water content of soil at -0.033 MPa (field capacity)
        real :: wp = 0.          !! mm H20/mm soil water content of soil at -1.5 MPa (wilting point)
        real :: wpmm = 0.        !! mm H20         water content of soil at -1.5 MPa (wilting point)
      end type soil_physical_properties
      
      type soil_nutrients
        real ::actp = 0.         !! kg P/ha        amount of phosphorus stored in the active mineral phosphorus pool 
        real ::aorgn = 0.        !! kg N/ha        amount of nitrogen stored in the active organic (humic) nitrogen pool in soil layer
        real ::fon = 0.          !! kg N/ha        amount of nitrogen stored in the fresh organic (residue) pool in soil layer
        real ::fop = 0.          !! kg P/ha        amount of phosphorus stored in the fresh organic (residue) pool in soil layer
        real ::nh3 = 0.          !! kg N/ha        amount of nitrogen stored in the ammonium pool in soil layer
        real :: no3 = 0.         !! kg N/ha        amount of nitrogen stored in the nitrate pool in soil layer
        real :: orgn = 0.        !! kg N/ha        amount of nitrogen stored in the stable organic N pool
        real :: orgp = 0.        !! kg P/ha        amount of phosphorus stored in the organic P pool in soil layer
        real :: solp = 0.        !! kg P/ha        amount of phosohorus in solution in soil layer
        real :: stap = 0.        !! kg P/ha        amount of phosphorus in the soil layer stored in the stable mineral phosphorus pool
        real :: mn = 0.
        real :: mp = 0.
      end type soil_nutrients
      
      type soil_profile
        character(len=16) :: snam = ""     !! NA            soil series name  
        character(len=1) :: hydgrp = ""    !! NA            hydrologic soil group
        character(len=1) :: texture = ""
        integer ::  nly  = 0               !! none          number of soil layers 
        type (soil_physical_properties),dimension (:), allocatable::phys
        type (soil_nutrients), dimension (:), allocatable :: nut
        type (soilayer), dimension (:), allocatable :: ly
        real :: zmx = 0.
        real :: anion_excl = 0.            !! none          fraction of porosity from which anions are excluded
        real :: crk = 0.                   !! none          crack volume potential of soil
        real :: alb = 0.                   !! none          albedo when soil is moist
        real :: usle_k = 0.                !!               USLE equation soil erodibility (K) factor 
        real :: det_san = 0.
        real :: det_sil = 0.
        real :: det_cla = 0.
        real :: det_sag = 0.
        real :: det_lag = 0.
        real :: sumul = 0.                 !! mm H2O         amount of water held in soil profile at saturation
        real :: sumfc = 0.                 !! mm H2O         amount of water held in the soil profile at field capacity                  
        real :: sw = 0.                    !! mm H2O         amount of water stored in soil profile on any given day
        real :: sumwp = 0.                 !!
        real :: swpwt = 0.                 !!
        real :: ffc = 0.                   !! none           initial HRU soil water content expressed as fraction of field capacity
        real :: wat_tbl = 0.               !! 
        real :: avpor = 0.                 !! none           average porosity for entire soil profile
        real :: avbd = 0.                  !! Mg/m^3         average bulk density for soil profile
      end type soil_profile
      type (soil_profile), dimension(:), allocatable :: soil
      
      type soil_hru_database
         type (soil_profile) :: s
         type (soil_physical_properties),dimension(:), allocatable::phys
         type (soil_nutrients), dimension (:), allocatable :: nut
         type (soilayer), dimension(:), allocatable :: ly
      end type soil_hru_database
      type (soil_hru_database), dimension(:), allocatable :: sol
    
      end module soil_parameter_module