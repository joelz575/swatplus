      module parm
   
      integer :: mscheds, isep, mcom, isolt
      integer :: ith, ilu, ulu, iadep, ipot, iwgen
      character (len=1) :: timest
      integer :: iyr, imo
     
      type plant_growth
         character(len=4) :: cpnm       !! N/A          4 letter char code represents crop name 
         real :: cht = 0.               !! m            canopy height 
         real :: lai = 0.               !! m**2/m**2    leaf area index
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
      !type (plant_mass) :: plt_mass_z
      !type (plant_mass) :: yld_tbr
      !type (plant_mass) :: yld_grn
      !type (plant_mass) :: yld_veg
      !type (plant_mass) :: yld_rsd
      !type (plant_mass), pointer :: pl_tot
      
      type plant_status
        integer :: idplt = 0           !! none         land cover code from plants.plt
        integer :: gro = 0             !! none         land cover status code 
                                       !!                0 = no land cover currently growing 
                                       !!                1 = land cover growing
        integer :: idorm = 0           !! none         dormancy status code; 0=land cover growing 1=land cover dormant
        real :: phumat = 0.            !! C            heat units to maturity
        real :: phuacc = 0.            !! fraction     fraction of plant heatunit accumulated
        real :: laimx_pop = 0.         !!
        real :: yield = 0.             !! kg/ha        land cover/crop yield (dry weight)
        integer :: harv_num = 0        !!              number of harvest operations
        integer :: curyr_mat = 1       !! 
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
       integer :: npl                  !! number of plants in community
       integer :: pcomdb               !! current plant community database number
       integer :: mseas = 0            !! none         monsoon season to initiate tropical plant growth
                                       !!                0 = outside monsoon period and during monsoon after growth is triggered
                                       !!                1 = in monsoon period but new growth not triggered
       type (plant_growth), dimension(:), allocatable :: plg
       type (plant_mass), dimension(:), allocatable :: plm
       type (plant_stress), dimension(:), allocatable :: plstr
       type (plant_status), dimension(:), allocatable :: plcur
      end type plant_community
      type (plant_community), dimension (:), allocatable :: pcom
      type (plant_community), dimension (:), allocatable :: pcom_init
      type (plant_growth) :: plgz
      type (plant_mass) :: plmz
      type (plant_stress) :: plstrz
      type (plant_status) :: plcurz
                 
      type pesticide
        character(len=10) :: name
        integer :: num_db
        real :: plt = 0.         !! |kg/ha        |pesticide on plant foliage
        real :: enr = 0.         !! |none         |pesticide enrichment ratio
        real :: sed = 0.         !! |kg/ha        |pesticide loading from HRU sorbed onto sediment
        real :: surq = 0.        !! |kg/ha        |amount of pesticide type lost in surface runoff on current day in HRU
        real :: latq = 0.        !! |kg pst/ha     |amount of pesticide in lateral flow in HRU for the day
        real :: zdb = 0.         !! |mm           |division term from net pesticide equation
      end type pesticide

      type soilayer
        real :: ec = 0.
        real :: cal = 0.
        real :: ph = 0.
        real :: alb = 0.         !! none          albedo when soil is moist
        real :: usle_k = 0.      !!               USLE equation soil erodibility (K) factor 
        real ::conk = 0.         !! mm/hr          lateral saturated hydraulic conductivity for each profile layer in a give HRU. 
        real ::flat = 0.         !! mm H2O         lateral flow storage array
        real ::pperco_sub = 0.   !!
        real :: prk = 0.         !! mm H2O         percolation from soil layer on current day
        real :: rsd = 0.         !! kg/ha          amount of organic matter in the soil classified as residue
        real :: volcr = 0.       !! mm             crack volume for soil layer 
        real :: tillagef = 0. 
        real :: rtfr = 0.        !! none           root fraction
        real :: watp = 0.
        integer :: a_days = 0
        integer :: b_days = 0
        real :: psp_store = 0.
        real :: ssp_store = 0.    
        real :: percc = 0.       !!
        real :: latc = 0.        !!
        real :: vwt = 0.         !!
        type (plant_mass) :: hum_sl                        !! slow humus
        type (plant_mass) :: hum_pa                        !! passive humus
        type (plant_mass) :: man                           !! manure organics
        type (plant_mass), dimension(:),allocatable :: rs  !! residue mass type
        real, dimension(:),allocatable :: kp
        real, dimension(:),allocatable :: pst
        real, dimension(:),allocatable :: bacsol
        real, dimension(:),allocatable :: bacsor
      end type soilayer
      type (soilayer), dimension(:), allocatable :: ly1
      
      type soil_physical_properties
        real :: d = 0.            !! mm            depth to bottom of soil layer
        real :: thick = 0.        !! mm            thichness of soil layer
        real :: bd = 0.           !! Mg/m**3       bulk density of the soil
        real :: k = 0.            !! mm/hr         saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
        real :: clay = 0.         !! none          fraction clay content in soil material (UNIT CHANGE!)
        real :: silt = 0.         !! %             percent silt content in soil material
        real :: sand = 0.         !! none          fraction of sand in soil material
        real :: rock = 0.         !! %             percent of rock fragments in soil layer 
        real :: conv_wt = 0.       !! none          factor which converts kg/kg to kg/ha
        real :: crdep = 0.         !! mm            maximum or potential crack volume
        real :: awc = 0.           !! mm H20/mm     soil available water capacity of soil layer
        real :: fc = 0.           !! mm H2O         amount of water available to plants in soil layer at field capacity (fc - wp),Index:(layer,HRU)
        real :: hk = 0.           !! none           beta coefficent to calculate hydraulic conductivity
        real :: por = 0.         !! none           total porosity of soil layer expressed as a fraction of the total volume, Index:(layer,HRU)
        real :: st = 0.          !! mm H2O         amount of water stored in the soil layer on any given day (less wp water)
        real :: tmp = 0.         !! deg C          daily average temperature of second soil layer
        real :: ul = 0.          !! mm H2O         amount of water held in the soil layer at saturation (sat - wp water)
        real :: up = 0.          !! mm H2O/mm soil water content of soil at -0.033 MPa (field capacity)
        real :: wp = 0.          !! mm H20/mm soil water content of soil at -1.5 MPa (wilting point)
        real :: wpmm = 0.        !! mm H20         water content of soil at -1.5 MPa (wilting point)
      end type soil_physical_properties

      type soil_profile
        character(len=16) :: snam = ""     !! NA            soil series name  
        character(len=16) :: hydgrp = ""    !! NA            hydrologic soil group
        character(len=16) :: texture = ""
        integer ::  nly  = 0               !! none          number of soil layers 
        type (soil_physical_properties),dimension (:), allocatable::phys
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
      type (soil_profile), dimension(:), allocatable :: soil_init
      
      type soil_hru_database
         character(len=16) :: snam = ""     !! NA            soil series name  
         character(len=16) :: hydgrp = ""    !! NA            hydrologic soil group
         character(len=16) :: texture = ""
         type (soil_profile) :: s
         type (soil_physical_properties),dimension(:), allocatable::phys
         type (soilayer), dimension(:), allocatable :: ly
      end type soil_hru_database
      type (soil_hru_database), dimension(:), allocatable :: sol

      type irrigation_sources
        integer :: flag = 0   !0= don't irrigate, 1=irrigate
        integer, dimension(:), allocatable :: chan
        integer, dimension(:), allocatable :: res
        integer, dimension(:), allocatable :: pond
        integer, dimension(:), allocatable :: shal
        integer, dimension(:), allocatable :: deep
      end type irrigation_sources
      
      type topography
           character(len=13) :: name
           real :: elev = 0.         !!               |m             |elevation of HRU
           real :: slope = 0.        !!	hru_slp(:)  |m/m           |average slope steepness in HRU
           real :: slope_len = 0.    !! slsubbsn(:)   |m             |average slope length for erosion
           real :: dr_den = 0.       !!               |km/km2        |drainage density
           real :: lat_len = 0.      !! slsoil(:)     |m             |slope length for lateral subsurface flow
           real :: dis_stream = 0.   !! dis_stream(:) | m            |average distance to stream
           real :: dep_co = 1.       !!               |              |deposition coefficient
           integer :: field_db = 0   !!               |              |pointer to field.fld
           integer :: channel_db=0   !!               |              |pointer to channel.dat
      end type topography
      
      type field
           character(len=13) :: name = "rep field"
           real :: length = 0.2  !!            |km            |field length for wind erosion
           real :: wid = 0.2  !!               |km            |field width for wind erosion
           real :: ang = 60.  !!               |deg           |field angle for wind erosion
      end type field
      
      type hydrology
           character(len=13) :: name
           real :: lat_ttime = 0.  !! lat_ttime(:)  |none          |Exponential of the lateral flow travel time
           real :: lat_sed = 0.    !! lat_sed(:)    |g/L           |sediment concentration in lateral flow
           real :: canmx = 0.      !! canmx(:)      |mm H2O        |maximum canopy storage
           real :: esco = 0.       !! esco(:)       |none          |soil evaporation compensation factor
           real :: epco = 0.       !! epco(:)       |none          |plant water uptake compensation factor (0-1)
           real :: erorgn = 0.     !! erorgn(:)     |none          |organic N enrichment ratio, if left blank
                                   !!                              |the model will calculate for every event
           real :: erorgp = 0.     !! erorgp(:)     |none          |organic P enrichment ratio, if left blank
                                   !!                              |the model will calculate for every event
           real :: cn3_swf = 0.    !! evpot(:)      |none          |pothole evaporation coefficient
           real :: biomix = 0.     !! biomix(:)     |none          |biological mixing efficiency.
                                   !!                              |Mixing of soil due to activity of earthworms
                                   !!                              |and other soil biota. Mixing is performed at
                                   !!                              |the end of every calendar year.
           real :: dep_imp = 0.    !! dep_imp(:)    |mm            |depth to impervious layer
           real :: dep_imp_init = 0.    !! dep_imp(:)    |mm       |initial depth to impervious layer- for calibration
           real :: lat_orgn = 0.
           real :: lat_orgp = 0.
           real :: harg_pet  = .0023  
           real :: cncoef = 0.3    !!               |              |plant ET curve number coefficient
           real :: perco = 1.      !!               |              |percolation coefficient-adjusts soil mositure
                                   !!                              | for perc to occur (1.0 = fc)
      end type hydrology
      
      type landuse
          character(len=15) :: name
          integer :: cn_lu = 0
          integer :: cons_prac = 0
          real :: usle_p = 0.           !! none     | USLE equation support practice (P) factor daily
          character (len=16) :: urb_ro  !! none     | urban runoff model
                                        !!          | "usgs_reg", simulate using USGS regression eqs
                                        !!          | "buildup_washoff", simulate using build up/wash off alg 
          integer ::  urb_lu = 0        !! none     | urban land type identification number
          real :: ovn = 0.05            !! none     | Manning's "n" value for overland flow
      end type landuse
      type (landuse), dimension (:), allocatable :: luse

      type hru_databases
        character(len=13) :: name = ""
        integer :: topo = 1
        integer :: hyd = 1
        integer :: soil = 1
        integer :: land_use_mgt = 1
        integer :: soil_nutr_init = 1
        integer :: surf_stor = 0
        integer :: snow = 1
        integer :: field = 0
      end type hru_databases
      
      type hru_databases_char
        character(len=16) :: name = ""
        character(len=16) :: topo = ""
        character(len=16) :: hyd = ""
        character(len=16) :: soil = ""
        character(len=16) :: land_use_mgt = ""
        character(len=16) :: soil_nutr_init = ""
        character(len=16) :: surf_stor = ""
        character(len=16) :: snow = ""
        character(len=16) :: field = ""
      end type hru_databases_char
        
      type hru_parms_db
        real :: co2 = 350.
      end type hru_parms_db
      
      type hydrologic_response_unit_db
        character(len=13) :: name = "default"
        type (hru_databases) :: dbs
        type (hru_databases_char) :: dbsc
        type (hru_parms_db) :: parms
      end type hydrologic_response_unit_db
      type (hydrologic_response_unit_db), dimension(:),allocatable :: hru_db
      
      type land_use_mgt_variables
        real :: usle_p = 0.                 !! |none          |USLE equation comservation practice (P) factor
        real :: usle_ls = 0.                !! |none          |USLE equation length slope (LS) factor
        real :: usle_mult = 0.              !! |none          |product of USLE K,P,LS,exp(rock)
        real :: sdr_dep = 0.                !! |
        integer :: ldrain= 0.               !! |none          |soil layer where drainage tile is located
        real :: tile_ttime = 0.             !! |none          |Exponential of the tile flow travel time
        real :: vfsi = 0.                   !! |none          |initial SCS curve number II value
        real :: vfsratio = 0.               !! |none          |contouring USLE P factor
        real :: vfscon = 0.                 !! |none          |fraction of the total runoff from the entire field
        real :: vfsch = 0;                  !! |none          |fraction of flow entering the most concentrated 10% of the VFS.
                                            !!                     which is fully channelized
        integer :: ngrwat = 0
        real :: grwat_i = 0.                !! |none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.                !! |none          |Mannings's n for grassed waterway
        real :: grwat_spcon = 0.            !! |none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.                !! |m             |depth of Grassed waterway
        real :: grwat_w = 0.                !! |none          |Width of grass waterway
        real :: grwat_l = 0.                !! |km            |length of Grass Waterway
        real :: grwat_s = 0.                !! |m/m           |slope of grass waterway
        real :: bmp_flag = 0.  
        real :: bmp_sed = 0.                !! |%             | Sediment removal by BMP 
        real :: bmp_pp = 0.                 !! |%             | Particulate P removal by BMP
        real :: bmp_sp = 0.                 !! |%             | Soluble P removal by BMP
        real :: bmp_pn = 0.                 !! |%             | Particulate N removal by BMP 
        real :: bmp_sn = 0.                 !! |%             | Soluble N removal by BMP  
        real :: bmp_bac = 0.                !! |%             | Bacteria removal by BMP
      end type land_use_mgt_variables
     
      type hydrologic_response_unit
        character(len=13) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        integer :: surfstor               !!points to res() for surface storage
        type (hru_databases) :: dbs       !!database pointers
        type (hru_databases_char) :: dbsc !!database pointers
        type (hru_parms_db) :: parms      !!calibration parameters
        integer :: land_use_mgt
        character(len=16) :: land_use_mgt_c
        integer :: lum_group
        character(len=16) :: lum_group_c        !!land use group for soft cal and output
        character(len=16) :: region
        integer :: plant_cov
        integer :: mgt_ops
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
        !! impunded water - points to res()
        integer :: res
        !! plants
        !type (plant_growth), dimension(:), allocatable :: pl
        !type (plant_mass), dimension(:), allocatable :: pl_tot
        !type (plant_mass), dimension(:), allocatable :: rsd_flt
        !type (plant_mass), dimension(:), allocatable :: rsd_std
        !type (plant_mass) :: rsd     !total flat residue of all plants
        !type (plant_mass) :: std     !total standing dead biomass of all plants
        !type (plant_mass) :: stl     !total standing live biomass of all plants
        type (pesticide), dimension(:), allocatable :: pst  !pest names simulated in the hru

        !! other data
        type (topography) :: topo
        type (field) :: field
        type (hydrology) :: hyd
        type (landuse) :: luse
        type (land_use_mgt_variables) :: lumv
        integer :: irrsrc
      end type hydrologic_response_unit
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru
      type (hydrologic_response_unit), dimension(:), allocatable, target :: hru_init
      type (plant_growth), pointer :: plt


      type pothole_dynamic
          real :: seep = 0.
          real :: vol = 0.            !! m**3 H2O     |current vol of water stored in the depression/impounded area
          real :: evap = 0.
          real :: sedin = 0.
          real :: solp = 0.           !! kg N         |amount of soluble p in pothole water body
          real :: solpi = 0.
          real :: orgp = 0.           !! kg N         |amount of organic P in pothole water body
          real :: orgpi = 0.
          real :: orgn = 0.           !! kg N         |amount of organic N in pothole water body
          real :: orgni = 0.
          real :: mps = 0.            !! kg N         |amount of stable mineral pool P in pothole water body
          real :: mpsi = 0.
          real :: mpa = 0.            !! kg N         |amount of active mineral pool P in pothole water body
          real :: mpai = 0.
          real :: no3i = 0.
          real :: sa = 0.             !! ha           |surface area of impounded water body
          real :: volx = 0.
          real :: flwi = 0.           !! m^3 H2O      |water entering pothole on day
          real :: sedi = 0.           !! metric tons  |sediment entering pothole on day
          real :: tile = 0.           !! m3/d         |average daily outflow to main channel from tile flow if drainage tiles are installed
                                                        !! in pothole (needed only if current HRU is  IPOT)
          real :: sed = 0.            !! metric tons  | amount of sediment in pothole water body
          real :: no3 = 0.            !! kg N         | amount of nitrate in pothole water body
          real :: san = 0.
          real :: sil = 0.
          real :: cla = 0.
          real :: lag = 0.
          real :: sag = 0.
          real :: sani = 0.
          real :: sili = 0.
          real :: clai = 0.
          real :: sagi = 0.
          real :: lagi = 0.
      end type pothole_dynamic
      type (pothole_dynamic), dimension (:), allocatable :: pot
       
      type pestinit
        character(len=13) :: name
        integer :: num_db     !!          |pesticide number in pesticide.pst
        real :: plt           !! kg/ha    |amount of pesticide on plant at start of simulation
        real :: soil          !! kg/ha    |amount of pesticide in soil at start of simulation
        real :: enr           !!          | pesticide enrichment ratio
      end type pestinit
      
      type pestinit_db
        character(len=16) :: name        !!      |name of pesticide community
        integer :: num                   !!      |number of pesticides in community
        character (len=16) :: exco_df    !!      |name of export coefficient file for pesticide community
        character (len=16) :: dr_df      !!      |name of delivery ratio file for pesticide community
        type (pestinit), dimension (:), allocatable :: pesti
      end type pestinit_db
      type (pestinit_db), dimension (:), allocatable :: pesti_db
     
!!    change per JGA 8/31/2011 gsm for output.mgt 
      real :: yield,  pst_kg
      
!!    new/modified arrays for plant competition
      integer :: idp, ipl, icom, isol

      real :: sumlai,strsa_av,strsn_av,strsp_av,strstmp_av
      real :: rto_no3,rto_solp,uno3d_tot,uapd_tot,sum_no3
      real :: sum_solp
      real, dimension (:), allocatable :: cht_mx,epmax,cvm_com,blai_com
      real, dimension (:), allocatable :: rsdco_plcom, translt
      real, dimension (:), allocatable :: strsw_av,uno3d,uapd
      real, dimension (:), allocatable :: par,htfac,un2,up2
      integer, dimension (:), allocatable :: iplt_afert,iseptic
     
!! septic variables for output.std
      real :: peakr, sw_excess, albday
      real :: wt_shall
      real :: sq_rto
      real :: tloss, inflpcp, snomlt, snofall, fixn, qtile, latlyr
      real :: inflrout, surfqout
      real :: fertn, sol_rd, cfertn, cfertp, sepday, bioday
      real :: sepcrk, sepcrktot, fertno3, fertnh3, fertorgn, fertsolp
      real :: fertorgp
      real :: fertp, grazn, grazp, soxy, sdti
      real :: voltot, volcrmin
      real :: canev, usle, rcn, precipday
      real :: thbact, bactrop, bactsedp
      real :: enratio
      real :: da_ha, vpd
      real :: bactrolp, bactsedlp, pet_day, ep_day
      real :: snoev, sno3up
      real :: es_day, ls_overq, latqrunon
      real :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max
      real :: sbactlchlp
      real :: bsprev
      real :: qday, usle_ei, al5, no3pcp, rcharea
      real :: snocov1, snocov2, rcor, lyrtile

      real :: autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      real :: rmptl, wdntl, rmp1tl, roctl, gwseep, revapday
      real :: petmeas, wdlprch
      real :: qdbank
      real :: pest_sol
      real :: chla_subco
      real :: rch_sag, rch_lag, rch_gra
      integer :: mo_atmo
      integer :: ifirstatmo, iyr_atmo
      integer :: mcr
      integer :: myr
      integer :: nhru,  mo, nrch
      integer :: inum1, ihru
      integer :: npmx, curyr
      integer :: iopera
      integer :: nd_30
      integer :: iscen
      integer :: msub, mpst, mlyr
      integer, dimension(100) :: ida_lup, iyr_lup
      integer :: no_up
!  routing 5/3/2010 gsm per jga    
! date
      character(len=8) :: date
      character(len=80) :: prog
!     apex/command output files
      integer :: mapex
      real, dimension (:), allocatable :: hi_targ, bio_targ, tnyld
      integer, dimension (:), allocatable :: idapa, iypa, ifirsta
!     apex/command output files
!  septic inputs
!! septic change added iseptic 1/28/09 gsm
      integer :: isep_ly
      real, dimension (:), allocatable :: percp    
      real, dimension (:), allocatable :: qstemm
!! septic changes added 1/28/09 gsm
      real, dimension (:), allocatable :: bio_bod, biom,rbiom
      real, dimension (:), allocatable :: fcoli, bz_perc, plqm
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep
      integer, dimension (:), allocatable :: sep_tsincefail
      
 !!   change per JGA 9/8/2011 gsm for output.mgt 
      real, dimension (:), allocatable :: sol_sumno3, sol_sumsolp
      real, dimension (:), allocatable :: strsw_sum, strstmp_sum
      real, dimension (:), allocatable :: strsn_sum, strsp_sum
      real, dimension (:), allocatable :: strsa_sum
! output files 
!!  added for binary files 3/25/09 gsm
      real, dimension (:,:), allocatable :: wpstaao

!     Sediment parameters added by Balaji for the new routines

      real, dimension (:), allocatable :: sanyld,silyld,clayld,sagyld
      real, dimension (:), allocatable :: lagyld,grayld
      real :: ressano,ressilo,resclao,ressago,reslago, resgrao
      real :: ressani, ressili, resclai, ressagi, reslagi,resgrai

      integer, dimension (:), allocatable :: itb
! msub = max number of subbasins
      real, dimension (:), allocatable :: sub_fr
      real, dimension (:), allocatable :: sub_km,sub_pet
      real, dimension (:), allocatable :: sub_etday
      real, dimension (:), allocatable :: qird
      
!!!!!! drains
      real, dimension (:), allocatable :: wnan
      real, dimension (:,:), allocatable :: sub_hhwtmp
      real, dimension (:,:), allocatable :: ch_k
      real, dimension (:,:), allocatable :: uh
      integer, dimension (:), allocatable :: hrutot,hru1
      integer, dimension (:), allocatable :: irelh

      real, dimension (:), allocatable :: irramt
      real, dimension (:), allocatable :: phusw
      integer, dimension (:), allocatable :: nop
      integer, dimension (:), allocatable :: yr_skip, isweep
      real :: sweepeff,frt_kg, pst_dep

      real, dimension (:), allocatable :: ranrns_hru
      integer, dimension (:), allocatable :: itill

      real, dimension (:), allocatable :: tc_gwat
      real, dimension (:), allocatable :: wfsh
      real, dimension (:), allocatable :: fsred
      real, dimension (:), allocatable :: sed_con, orgn_con, orgp_con
      real, dimension (:), allocatable :: soln_con, solp_con
      real, dimension (:), allocatable :: filterw
      real, dimension (:), allocatable :: flowfr
      real, dimension (:), allocatable :: flowmin
      real, dimension (:), allocatable :: divmax, cn1,cn2
      real, dimension (:), allocatable :: sol_cov
      real, dimension (:), allocatable :: driftco,cn3
      real, dimension (:), allocatable :: smx,sci
      real, dimension (:), allocatable :: bactpq
      real, dimension (:), allocatable :: cnday
      real, dimension (:), allocatable :: bactlpq,auto_eff
      real, dimension (:), allocatable :: bactps,bactlps,tmpav
      real, dimension (:), allocatable :: sno_hru,sno_init,hru_ra
      real, dimension (:), allocatable :: tmx,tmn,tmp_hi,tmp_lo
      real, dimension (:), allocatable :: tconc,hru_rmx
      real, dimension (:), allocatable :: usle_cfac,usle_eifac
      real, dimension (:), allocatable :: anano3,aird,t_ov
      real, dimension (:), allocatable :: u10,rhd
      real, dimension (:), allocatable :: canstor,ovrlnd
      real, dimension (:), allocatable :: irr_mx, auto_wstr
      real, dimension (:), allocatable :: cfrt_id, cfrt_kg, cpst_id
      real, dimension (:), allocatable :: cpst_kg
      real, dimension (:), allocatable :: irr_asq, irr_eff
      real, dimension (:), allocatable :: irrsq
      real, dimension (:), allocatable :: bio_eat, bio_trmp
      integer, dimension (:), allocatable :: ifrt_freq,ipst_freq,irr_noa
      integer, dimension (:), allocatable :: irr_sc,irr_no
      integer, dimension (:), allocatable :: imp_trig, fert_days,irr_sca
      integer, dimension (:), allocatable :: pest_days, wstrs_id
!    Drainmod tile equations  08/2006 
	  real, dimension (:), allocatable :: cumei,cumeira
	  real, dimension (:), allocatable :: cumrt, cumrai
!    Drainmod tile equations  08/2006
      real, dimension (:), allocatable :: bio_min,surqsolp
      real, dimension (:), allocatable :: cklsp
      real, dimension (:), allocatable :: trapeff
      real, dimension (:), allocatable :: pplnt,snotmp
      real, dimension (:), allocatable :: dayl,brt
!    Drainmod tile equations  01/2006 
	real, dimension (:), allocatable :: sstmaxd
	real, dimension (:), allocatable :: stmaxd
!    Drainmod tile equations  01/2006
      real, dimension (:), allocatable :: twash,doxq
      real, dimension (:), allocatable :: percn
      real, dimension (:), allocatable :: cbodu,chl_a,qdr
      real, dimension (:), allocatable :: latno3,latq,nplnt
      real, dimension (:), allocatable :: tileq, tileno3
      real, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      real, dimension (:), allocatable :: sedorgp,sedyld,sepbtm
      real, dimension (:), allocatable :: surfq,surqno3
      real, dimension (:), allocatable :: hru_dafr
      real, dimension (:), allocatable :: phubase
      real, dimension (:), allocatable :: lai_yrmx,dormhr
      real, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      real, dimension (:), allocatable :: tnylda, afrt_surface
      real :: frt_surface
      real, dimension (:), allocatable :: auto_nyr, auto_napp
      real, dimension (:), allocatable :: manure_kg, auto_nstrs
      real, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      real, dimension (:,:), allocatable :: wgncur,wgnold,wrt
      real, dimension (:,:), allocatable :: phi
      real, dimension (:,:), allocatable :: wat_phi
      real, dimension (:,:), allocatable :: bss,surf_bs  
      real, dimension (:,:,:), allocatable :: pst_lag
      integer, dimension (:), allocatable :: swtrg,hrupest
      integer, dimension (:), allocatable :: iafrttyp, nstress
      !! burn
      integer, dimension (:), allocatable :: grz_days
      integer, dimension (:), allocatable :: icr
      integer, dimension (:), allocatable :: irrno,npcp
      integer, dimension (:), allocatable :: igrz,ndeat
      integer, dimension (:), allocatable :: hru_sub
      integer, dimension (:), allocatable :: iday_fert,icfrt
      integer, dimension (:), allocatable :: ndcfrt
      integer, dimension (:), allocatable :: ntil,irrsc
      integer, dimension (:), allocatable :: icpst,ndcpst
      integer, dimension (:), allocatable :: iday_pest, irr_flag
      integer, dimension (:), allocatable :: manure_id

!!     gsm added for sdr (drainage) 7/24/08
      integer, dimension (:,:), allocatable :: mgt_ops
      integer, dimension (:), allocatable :: npno

      real, dimension (:,:), allocatable :: hhqday
! additional reach variables , added by Ann van Griensven
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
!Additional buffer and filter strip variables Mike White
      real, dimension (:), allocatable :: stsol_rd
!! Armen Jan 08 end
	real, dimension (:), allocatable :: ubnrunoff,ubntss
	real, dimension (:,:), allocatable :: ovrlnd_dt,hhsurfq	
	real, dimension (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
	real, dimension(:,:), allocatable:: hhsedy, sub_subp_dt
	real, dimension(:,:), allocatable:: sub_hhsedy
	real, dimension(:), allocatable:: init_abstrc
	real, dimension(:), allocatable:: dratio
	
!! bmp modeling by jaehak jeong
      real, dimension(:), allocatable :: sub_cn2, sub_ha_urb
      !sed-fil
      real, dimension(:), allocatable:: sub_ha_imp,subdr_km,subdr_ickm
      real, dimension(:,:), allocatable:: sf_im,sf_iy,sp_sa,                &
        sp_pvol,sp_pd,sp_sedi,sp_sede,ft_sa,ft_fsa,                         &
        ft_dep,ft_h,ft_pd,ft_k,ft_dp,ft_dc,ft_por,                          &
        tss_den,ft_alp,sf_fr,sp_qi,sp_k,ft_qpnd,sp_dp,                      &
        ft_qsw,ft_qin,ft_qout,ft_sedpnd,sp_bpw,ft_bpw,                      &
        ft_sed_cumul,sp_sed_cumul
      integer, dimension(:), allocatable:: num_sf
      integer, dimension(:,:), allocatable:: sf_typ,sf_dim,ft_qfg,          &
        sp_qfg,sf_ptp,ft_fc 
      
      !detention pond
	integer, dimension(:), allocatable :: dtp_subnum,dtp_imo,               &
        dtp_iyr,dtp_numweir,dtp_numstage,dtp_stagdis,                       &
        dtp_reltype,dtp_onoff                                         
!! sj & armen changes for SWAT-C
	real, dimension (:), allocatable :: cf, cfh, cfdec
!! sj & armen changes for SWAT-C end

	integer, dimension(:,:), allocatable :: dtp_weirtype,dtp_weirdim
	
	real, dimension(:), allocatable ::dtp_evrsv,                         &
       dtp_totwrwid,dtp_parm,                                 &
       dtp_intcept,dtp_expont,dtp_coef1,    &
       dtp_coef2,dtp_coef3,dtp_ivol,dtp_ised
 
      real, dimension (:), allocatable :: min_res             
      real, dimension(:,:), allocatable:: dtp_wdratio,dtp_depweir,       &
        dtp_diaweir,dtp_pcpret,dtp_cdis,dtp_flowrate,        &
        dtp_wrwid,dtp_addon

      !retention irrigation
      real, dimension(:), allocatable:: ri_subkm,ri_totpvol,             &
        irmmdt
      real, dimension(:,:), allocatable:: ri_sed,ri_fr,ri_dim,          &
        ri_im,ri_iy,ri_sa,ri_vol,ri_qi,ri_k,ri_dd,ri_evrsv,             & 
        ri_dep,ri_ndt,ri_pmpvol,ri_sed_cumul,hrnopcp,ri_qloss,          &
        ri_pumpv,ri_sedi
      integer, dimension(:), allocatable:: num_ri,ri_luflg
      
      !wet pond
      integer, dimension(:), allocatable:: wtp_onoff,wtp_imo,  &
       wtp_iyr,wtp_dim,wtp_stagdis,wtp_sdtype      
      real, dimension(:), allocatable:: wtp_pvol,wtp_pdepth,wtp_sdslope,   &
        wtp_lenwdth,wtp_extdepth,wtp_hydeff,wtp_evrsv,wtp_sdintc,          &
        wtp_sdexp,wtp_sdc1,wtp_sdc2,wtp_sdc3,wtp_pdia,wtp_plen,            &
        wtp_pmann,wtp_ploss,wtp_k,wtp_dp,wtp_sedi,wtp_sede,wtp_qi 
     
      real :: bio_init, lai_init, cnop, harveff, frac_harvk

      integer, dimension(:), allocatable :: tillage_switch
      real, dimension(:), allocatable :: tillage_depth
      integer, dimension(:), allocatable :: tillage_days
      real, dimension(:), allocatable :: tillage_factor

      end module parm