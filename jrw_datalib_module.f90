      module jrw_datalib_module
      
      integer :: msoils, mpl
      
      type data_files_max_elements
        integer :: topo = 0           !!nubz
        integer :: hyd = 0            !!nubz
        integer :: soil = 0           !! none     |number of types of soils 
        integer :: landuse = 0        !! none     |number of landuse types
        integer :: mgt_ops = 0        !! none     |number of records in management
        integer :: cn_lu = 0          !! none     |number of records in cntable.lum
        integer :: cons_prac = 0      !! none     |number of records in conservation practice table
        integer :: pothole = 0        !! none     |number of potholes
        integer :: sdr = 0            !! none     |number of types of susbsurface drain systems
        integer :: str_ops = 0        !! none     |number of management ops
        integer :: urban = 0          !! none     |number of urban land use types in urban.urb
        integer :: ovn = 0            !! none     |number of overland flow n types in ovn_table.lum
        integer :: septic = 0         !! none     |number of types of septic systems
        integer :: plantparm = 0      !! none     |number of total plants in plants.plt
        integer :: fertparm = 0       !! none     |number of total fertilizer in fertilizer.frt
        integer :: tillparm = 0       !! none     |number of total tillages in tillage.til
        integer :: pestparm = 0       !! none     !number of total pesticides in pesticide.pst
        integer :: pestcom = 0        !! none     !number of total pesticides communities in pesticide.com
        integer :: plantcom = 0       !! none     |number of plant communities
        integer :: soiltest = 0       !! none     |number of soiltest 
        integer :: pestdb = 0         !! none     |number of pesticide ops
        integer :: bactdb = 0         !! none     |number of bacteria props
        integer :: sno = 0            !! none     |number of snow props
        integer :: field = 0          !! none     |number of field props
        integer :: atmodep = 0        !! none     |atmospheric deposition props
        integer :: chemapp_db = 0     !! none     |chemical application (fert and pest) operations
        integer :: grazeop_db = 0     !! none     |grazing operations
        integer :: harvop_db = 0      !! none     |harvest only operations
        integer :: irrop_db = 0       !! none     |irrigation operations
        integer :: sweepop_db = 0     !! none     |sweep operations
        integer :: filtop_db = 0      !! none     |filter strip data
        integer :: fireop_db = 0      !! none     |fire data
        integer :: grassop_db = 0     !! none     |grassed waterways data
        integer :: plparmop_db = 0    !! none     |plant parms update data
        integer :: rsdmgtop_db = 0    !! none     |residue mangement data
        integer :: bmpuserop_db = 0   !! none     |user defined upland CP removal
        integer :: cond = 0           !! none     |conditional data
        integer :: initop_db = 0      !! none     |initial.str
        integer :: wgnsta = 0         !! none     |max wgn stations included in weather-wgn.cli
        integer :: wst = 0            !! none     |max weather stations include in weather-sta.cli
        integer :: pcpfiles = 0       !! none     |max pcp files included in pcp.cli
        integer :: tmpfiles = 0       !! none     |max tmp files included in tmp.cli
        integer :: rhfiles = 0        !! none     |max relative humidity files included in hmd.cli
        integer :: slrfiles = 0       !! none     |max solar radiation files included in slr.cli
        integer :: wndfiles = 0       !! none     |max wind files included in the wnd.cli
        integer :: cal_parms = 0      !! none     |max number of calibration parameters in cal_parms_upd
        integer :: cal_upd = 0        !! none     |max number of calibration parameter updates
        integer :: sched_up = 0       !! none     |max number of scheduled updates (paramters, structures, land_use_mgt)
        integer :: cond_up = 0        !! none     |max number of conditional updates (paramters, structures, land_use_mgt)
        integer :: wr_ob = 0          !! none     |max number of water rights objects
        integer :: irr_nosrc = 0      !! none     |max number of hru's with unlimited water source for irrigation
        integer :: d_tbl = 0          !! none     |max number of decision tables
        integer :: cs_db = 0
        integer :: pathcom = 0
        integer :: hmetcom = 0
        integer :: saltcom = 0
        integer :: res = 0            !! none     |max number of reservoir data
        integer :: lsu_elem = 0
        integer :: lsu_out = 0        !! none     |max number of landscape regions for output
        integer :: lsu_reg = 0        !! none     |max number of landscape regions for soft cal and output by lum
        integer :: lscal_reg = 0      !! none     |max number of soft data for landscape calibration (for each cal region)
        integer :: aqu_elem = 0
        integer :: aqu_out = 0        !! none     |max number of aquifer regions for output
        integer :: aqu_reg = 0        !! none     |max number of aquifer regions for soft cal and output by aquifer type
        integer :: cha_out = 0        !! none     |max number of channel regions for output
        integer :: cha_reg = 0        !! none     |max number of channel regions for soft cal and output by channel order
        integer :: res_out = 0        !! none     |max number of reservoir regions for output
        integer :: res_reg = 0        !! none     |max number of reservoir regions for soft cal and output by reservoir type
        integer :: rec_out = 0        !! none     |max number of recall regions for output
        integer :: rec_reg = 0        !! none     |max number of recall regions for soft cal and output by recall type
        integer :: plcal_reg = 0      !! none     |max number of regions for plant calibration
        integer :: ch_reg = 0         !! none     |max number of regions for channel calibration
        integer :: lscal_prms = 0     !! none     |max number of parameters for landscape hru calibration
        integer :: res_dat       
        integer :: res_init
        integer :: res_hyd
        integer :: res_sed
        integer :: res_nut
        integer :: res_pst
        integer :: res_weir
        integer :: wet_dat       
        integer :: wet_hyd
        integer :: ch_surf
        integer :: ch_dat
        integer :: ch_init
        integer :: ch_hyd
        integer :: ch_sed
        integer :: ch_nut
        integer :: ch_pst
        integer :: bac
        integer :: exco
        integer :: sep
        integer :: ch_lte
      end type data_files_max_elements
      type (data_files_max_elements), save :: db_mx

      type bacteria_db
        character(len=15) :: bactnm         
        real :: do_soln = 0.       !! 1/day         |Die-off factor for pers bac in soil solution
        real :: gr_soln = 0.       !! 1/day         |Growth factor for pers bac in soil solution
        real :: do_sorb = 0.       !! 1/day         |Die-off factor for pers bac adsorbed to soil part
        real :: gr_sorb = 0.       !! 1/day         |Growth factor for pers bac adsorbed to soil part
        real :: kd = 0.            !! none          |Bact part coeff bet sol and sorbed phase in surf runoff
        real :: t_adj = 0.         !! none          |temp adj factor for bac die-off/growth
        real :: washoff = 0.       !! none          |frac of pers bac on foliage washed off by a rainfall event
        real :: do_plnt = 0.       !! 1/day         |Die-off factor for pers bac on foliage
        real :: gr_plnt = 0.       !! 1/day         |Growth factor for persistent bacteria on foliage
        real :: fr_manure = 0.     !! none          |frac of manure containing active colony forming units (cfu)
        real :: perco = 0.         !! none          |bacteria perc coeff ratio of solution bacteria in surf layer
        real :: det_thrshd         !! # cfu/m^2     |Threshold detection level for less pers bac when bact levels
                                                    !drop to this amt the model considers bac in the soil to be 
                                                    !insignificant and sets the levels to zero
        real :: do_stream = 0.     !! 1/day         |Die-off factor for persistent bacteria in streams
        real :: gr_stream = 0.     !! 1/day         |growth factor for persistent bacteria in streams
        real :: do_res = 0.        !! 1/day         |Die-off factor for less persistent bacteria in reservoirs
        real :: gr_res = 0.        !! 1/day         |growth factor for less persistent bacteria in reservoirs
        real :: swf = 0.           !! cfu           |fraction of manure containing active colony forming units
        real :: conc_min
      end type bacteria_db
      type (bacteria_db), dimension(:), allocatable  :: bac_db
           
      type fertilizer_db
        character(len=16) :: fertnm = ' '
        real :: fminn = 1.            !! kg minN/kg frt     |fract of fert which is mineral nit (NO3+NH3)
        real :: fminp = 0.            !! kg minN/kg frt     |frac of fert which is mineral phos
        real :: forgn = 0.            !! kg orgN/kg frt     |frac of fert which is org n
        real :: forgp = 0.            !! kg orgP/kg frt     |frac of fert which is org p
        real :: fnh3n = 0.            !! kg NH3-N/kg N      |frac of mineral N content of fert which is NH3
        real :: bactpdb = 0.          !! # bact/kg frt      |conc of persistent bacteria in fert
        real :: bactlpdb = 0.         !! # bact/kg frt      |conc of less persistent bacteria in fert
        real :: bactkddb = 0.         !! none               |frac of bacteria in solution (remaining is sorbed to soil part)
      end type fertilizer_db
      type (fertilizer_db), dimension(:),allocatable, save :: fertdb
      
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
      type (pesticide_cp), dimension(:),allocatable, save::pstcp
      
      type tillage_db
        character(len=8) :: tillnm = ' '
        real :: effmix = 0.          !! none               |mixing efficiency of tillage operation
        real :: deptil = 0.          !! mm                 |depth of mixing caused by tillage
        real :: ranrns = 0.          !! mm                 |random roughness
        real :: ridge_ht = 0.        !! mm                 |ridge height
        real :: ridge_sp = 0.        !! mm                 |ridge inteval (or row spacing)
      end type tillage_db
      type (tillage_db), dimension(:),allocatable, save :: tilldb  
      
      type urban_db
        character(len=16) :: urbnm
        real :: fimp = 0.05        !! fraction          |fraction of HRU area that is imp
        real :: fcimp = 0.05       !! fraction          |fraction of HRU that is classified as directly connected imp
        real :: curbden = 0.0      !! km/ha             |curb length density              
        real :: urbcoef = 0.0      !! 1/mm              |wash-off coefficient for removal of constituents from an imp surface
        real :: dirtmx = 1000.0    !! kg/curb km        |max amt of solids allowed to build up on imp surfaces
        real :: thalf = 1.0        !! days              |time for the amt of solids on imp areas to build up to 1/2 max level
        real :: tnconc = 0.0       !! mg N/kg sed       |conc of total nitrogen in suspended solid load from imp areas
        real :: tpconc = 0.0       !! mg P/kg sed       |conc of total phosphorus in suspened solid load from imp areas       
        real :: tno3conc = 0.0     !! mg NO3-N/kg sed   |conc of NO3-N in suspended solid load from imp areas
        real :: urbcn2 = 98.0      !! none              |moisture condiction II curve number for imp areas
      end type urban_db
      type (urban_db), dimension(:),allocatable, save :: urbdb
      
      type septic_db
          character(len=20) :: sepnm
          real :: qs = 0.               !! m3/d          |flow rate of the septic tank effluent per capita (sptq)
          real :: bodconcs = 0.         !! mg/l          |biological oxygen demand of the septic tank effluent
          real :: tssconcs = 0.         !! mg/l          |concentration of total suspended solid in the septic tank effluent
          real :: nh4concs = 0.         !! mg/l          |concentration of total phosphorus in the septic tank effluent
          real :: no3concs = 0.         !! mg/l          |concentration of nitrate in the septic tank effluent
          real :: no2concs = 0.         !! mg/l          |concentration of nitrite in the septic tank effluent
          real :: orgnconcs = 0.        !! mg/l          |concentration of organic nitrogen in the septic tank effluent
          real :: minps = 0.            !! mg/l          |concentration of mineral phosphorus in the septic tank effluent    
          real :: orgps = 0.            !! mg/l          |concentration of organic phosphorus in the septic tank effluent
          real :: fcolis = 0.           !! mg/l          |concentration of fecal coliform in the septic tank effluent
      end type septic_db
      type (septic_db), dimension (:), allocatable :: sepdb
      
      type septic_system
        character(len=13) :: name = "default"
        integer :: typ = 0      !! none            |septic system type
        integer :: yr = 0       !!                 |year the septic system became operational
        integer :: opt = 0      !!none             |Septic system operation flag (1=active,2=failing,0=not operated)
        real :: cap  = 0.       !!none             |Number of permanent residents in the house
        real :: area = 0.       !!m^2              |average area of drainfield of individual septic systems 
        integer :: tfail  = 0   !!days             |time until falling systems gets fixed
        real :: z  = 0.         !!mm               |depth to the top of the biozone layer from the ground surface
        real :: thk = 0.        !!mm               |thickness of biozone layer        
        real :: strm_dist = 0.  !!km               |distance to the stream from the septic
        real :: density = 0.    !!                 |number of septic systems per square kilometer
        real :: bd  = 0.        !!kg/m^3           |density of biomass 
        real :: bod_dc = 0.     !!m^3/day          |BOD decay rate coefficient
        real :: bod_conv        !!                 |a conversion factor representing the proportion of mass
        !!                                           bacterial growth and mass BOD degraded in the STE.
        real :: fc1 = 0.        !!none             |Linear coefficient for calculation of field capacity in the biozone
        real :: fc2 = 0.        !!none             |Exponential coefficient for calculation of field capacity in the biozone  
        real :: fecal = 0.      !!m^3/day          |fecal coliform bacteria decay rate coefficient  
        real :: plq = 0.        !!none             |conversion factor for plaque from TDS 
        real :: mrt = 0.        !!none             |mortality rate coefficient   
        real :: rsp = 0.        !!none             |respiration rate coefficient  
        real :: slg1 = 0.       !!none             |slough-off calibration parameter
        real :: slg2 = 0.       !!none             |slough-off calibration parameter
        real :: nitr = 0.       !!none             |nitrification rate coefficient
        real :: denitr = 0.     !!none             |denitrification rate coefficient
        real :: pdistrb = 0.    !!(L/kg)           |Linear P sorption distribution coefficient
        real :: psorpmax = 0.   !!(mg P/kg Soil)   |Maximum P sorption capacity 
        real :: solpslp = 0.    !!                 |Slope of the linear effluent soluble P equation
        real :: solpintc = 0.   !!                 |Intercept of the linear effluent soluble P equation
      end type septic_system
      type (septic_system), dimension (:), allocatable :: sep
      
      type irrigation_operation
        character (len=13) :: name
        real :: eff = 0.                !!        |irrigation in-field efficiency
        real :: surq = 0.               !! frac   |surface runoff ratio
        real :: dep_mm = 0.             !! mm     |depth of application for subsurface
        real :: salt = 0.               !! mg/kg  |concentration of salt in irrigation
        real :: no3 = 0.                !! mg/kg  |concentration of nitrate in irrigation
        real :: po4 = 0.                !! mg/kg  |concentration of phosphate in irrigation
      end type irrigation_operation
      type (irrigation_operation), dimension(:), allocatable :: irrop_db

      type filtstrip_operation
        character (len=13) :: name
        real :: vfsi = 0.               !       |initial SCS curve number II value
        real :: vfsratio = 0.           !       |contouring USLE P factor
        real :: vfscon                  !       |fraction of the total runoff from the entire field
        real :: vfsch                   !       |fraction of flow entering the most concentrated 10% of the VFS.
                                        !          which is fully channelized
      end type filtstrip_operation
      type (filtstrip_operation), dimension(:), allocatable :: filtstrip_db

      type fire_operation
        character (len=13) :: name
        real :: cn2_upd = 0.            !       |change in SCS curve number II value
        real :: fr_burn = 0.            !       |fraction burned
      end type fire_operation
      type (fire_operation),dimension(:), allocatable :: fire_db
      
      type grwaterway_operation
        character (len=13) :: name
        real :: grwat_i = 0.       !none          |On/off Flag for waterway simulation
        real :: grwat_n = 0.       !none          |Mannings's n for grassed waterway
        real :: grwat_spcon = 0.   !none          |sediment transport coefficant defined by user
        real :: grwat_d = 0.       !m             |depth of Grassed waterway
        real :: grwat_w = 0.       !none          |width of grass waterway
        real :: grwat_l = 0.       !km            |length of Grass Waterway
        real :: grwat_s = 0.       !m/m           |slope of grass waterway
      end type grwaterway_operation
      type (grwaterway_operation),dimension(:), allocatable :: grwaterway_db

      type bmpuser_operation  
        character (len=13) :: name
        integer :: bmp_flag = 0
        real :: bmp_sed = 0.       !%              | Sediment removal by BMP       
        real :: bmp_pp = 0.        !%              | Particulate P removal by BMP
        real :: bmp_sp = 0.        !%              | Soluble P removal by BMP
        real :: bmp_pn = 0.        !%              | Particulate N removal by BMP 
        real :: bmp_sn = 0.        !%              | Soluble N removal by BMP  
        real :: bmp_bac = 0.       !%              | Bacteria removal by BMP
      end type bmpuser_operation 
      
      type bmpuser_operation1  
        character (len=13) :: name
        integer :: bmp_flag = 0
        real :: surf_flo = 0.       !%              | Surface Flow removal by BMP  
        real :: surf_sed = 0.       !%              | Surface Sediment removal by BMP       
        real :: surf_pp = 0.        !%              | Surface Particulate P removal by BMP
        real :: surf_sp = 0.        !%              | Surface Soluble P removal by BMP
        real :: surf_pn = 0.        !%              | Surface Particulate N removal by BMP 
        real :: surf_sn = 0.        !%              | Surface Soluble N removal by BMP  
        real :: surf_bac = 0.       !%              | Surface Bacteria removal by BMP
        real :: sub_flo = 0.        !%              | Subsurface Flow removal by BMP  
        real :: sub_sed = 0.        !%              | Subsurface Sediment removal by BMP       
        real :: sub_pp = 0.         !%              | Subsurface Particulate P removal by BMP
        real :: sub_sp = 0.         !%              | Subsurface Soluble P removal by BMP
        real :: sub_pn = 0.         !%              | Subsurface Particulate N removal by BMP 
        real :: sub_sn = 0.         !%              | Subsurface Soluble N removal by BMP  
        real :: sub_bac = 0.        !%              | Subsurface Bacteria removal by BMP 
        real :: tile_flo = 0.       !%              | Tile Flow removal by BMP 
        real :: tile_sed = 0.       !%              | Tile Sediment removal by BMP       
        real :: tile_pp = 0.        !%              | Tile Particulate P removal by BMP
        real :: tile_sp = 0.        !%              | Tile Soluble P removal by BMP
        real :: tile_pn = 0.        !%              | Tile Particulate N removal by BMP 
        real :: tile_sn = 0.        !%              | Tile Soluble N removal by BMP  
        real :: tile_bac = 0.       !%              | Tile Bacteria removal by BMP 
      end type bmpuser_operation1
      type (bmpuser_operation),dimension(:), allocatable :: bmpuser_db
      
      type chemical_application_operation
        character (len=16) :: name
        character (len=16) :: form = ' '        !           |solid; liquid
        character (len=16) :: op_typ = ' '      !           |operation type-spread; spray; inject; direct
        real :: app_eff = 0.                    !           |application efficiency
        real :: foliar_eff = 0.                 !           |foliar efficiency
        real :: inject_dep = 0.                 !mm         |injection depth
        real :: surf_frac = 0.                  !           |surface fraction-amount in upper 10 mm
        real :: drift_pot = 0.                  !           |drift potential
        real :: aerial_unif = 0.                !           |aerial uniformity
      end type chemical_application_operation
      type (chemical_application_operation),dimension(:), allocatable :: chemapp_db

      type harvest_operation
        character (len=13) :: name
        character (len=13) :: typ   !none              |grain;biomass;residue;tree;tuber
        real :: hi_ovr = 0.         !(kg/ha)/(kg/ha)   |harvest index target specified at harvest
        real :: eff = 0.            !none              |harvest efficiency: fraction of harvested yield that is removed 
                                                       !the remainder becomes residue on the soil surface
        real :: bm_min = 0          !kg/ha             |minimum biomass to allow harvest
      end type harvest_operation
      type (harvest_operation), dimension(:), allocatable :: harvop_db
      type (harvest_operation) :: harvop
      type (harvest_operation) :: hkop
      
      type grazing_operation
        character (len=13) :: name
        character (len=13) :: fertnm = ' '
        real :: eat = 0.              !!(kg/ha)/day      |dry weight of biomass removed 
                                      !!                    by grazing daily
        real :: tramp = 0.            !!(kg/ha)/day      |dry weight of biomass removed
                                      !!                    by trampling daily
        real :: manure = 0.           !!(kg/ha)/day      |dry weight of manure deposited
        real :: biomin = 0.           !!kg/ha            |minimum plant biomass for grazing
      end type grazing_operation
      type (grazing_operation), dimension(:), allocatable :: grazeop_db
      
      type streetsweep_operation
        character (len=13) :: name
        real :: eff = 0.               !!none             |removal efficiency of sweeping operation
        real :: fr_curb = 0.           !!none             |availability factor, the fraction of the
                                       !!                    curb length that is sweepable
      end type streetsweep_operation
      type (streetsweep_operation), dimension(:), allocatable :: sweepop_db
      type (streetsweep_operation) :: sweepop
      
      type management_ops1
        character(len=16) :: name
        character(len=16) :: op  
        !! operation code 4-digit char name
        !!  1 pcom - establish plant community  
        !!  2 plnt - plant  
        !!  3 harv - harvest only  
        !!  4 kill - Kill  
        !!  5 hvkl - Harvest and kill   
        !!  6 till - Tillage
        !!  7 irrm - Irrigation manual
        !!  8 irra - Irrigation auto
        !!  9 rel     ??  REMOVE?
        !! 10 fert - Fertilizer
        !! 11 frta - Fertilizer auto
        !! 12 frtc - Fertilizer continuous 
        !! 13 pest - Pesticide application 
        !! 14 pstc - Pesticide continuous
        !! 15 graz - Grazing  
        !! 16 burn - Burn  
        !! 17 swep - Street Sweep  
        !! 18 prtp - Print plant vars
        !! 19 mons - ?? REMOVE ??
        !! 20 skip - Skip to end of the year
        integer :: mon = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: year = 0
        real :: husc = 0.
        character(len=16) :: op_char
        character (len=16) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
        integer :: op4 = 0              !! |none          |
      end type management_ops1
      
      type management_ops
        character(len=16) :: name
        character(len=16) :: op  
        !! operation code 4-digit char name
        !! plnt; autoplnt - plant
        !! harv; autoharv - harvest only
        !! kill; autokill - kill
        !! hvkl; autohk - harvest and kill
        !! till; autotill - tillage
        !! irr; autoirr - irrigation
        !! fert; autofert - fertlizer
        !! pest; pestauto - pesticide application
        !! graz; autograz - grazing
        !! burn; autoburn - burn
        !! swep; autoswep - street sweep
        !! prtp - print plant vars
        !! skip - skip to end of the year
        integer :: mon = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: year = 0
        real :: husc = 0.
        character(len=16) :: op_char
        character (len=16) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
        integer :: op4 = 0              !! |none          |fert and pest type-point to fert and pest db
      end type management_ops
      type (management_ops) :: mgt
      type (management_ops) :: mgt1
      type (management_ops), dimension(1) :: mgt2
      
      type management_schedule
        character(len=35) :: name
        integer :: num_ops = 0
        integer :: num_autos = 0
        type (management_ops), dimension (:), allocatable :: mgt_ops
        character(len=16), dimension (:), allocatable :: auto_name
        integer, dimension (:), allocatable :: num_db
        integer :: irr
      end type management_schedule
      type (management_schedule), dimension (:), allocatable :: sched
      
      type calibration_parameters
        character(len=16) :: name       !! cn2, esco, awc, etc.
        character(len=16) :: ob_typ     !! object type the parameter is associated with (hru, chan, res, basin, etc)
        real :: absmin                  !! minimum range for variable
        real :: absmax                  !! maximum change for variable
        character(len=16) :: units      !! units used for each parameter
      end type calibration_parameters
      type (calibration_parameters), dimension (:), allocatable :: cal_parms    !dimensioned to db_mx%cal_parms_tot
      
      type calibration_conditions
        character(len=16) :: var
        character(len=16) :: alt
        real :: targ
        character(len=16) :: targc
      end type calibration_conditions   

      type update_parameters
        character(len=16) :: name       !! cn2, terrace, land use, mgt, etc.
        integer :: num_db = 0           !! crosswalk number of parameter, structure or land use to get database array number
        character(len=16) :: chg_typ    !! type of change (absval,abschg,pctchg)
        real :: val                     !! value of change
        integer :: conds                !! number of conditions
        integer :: lyr1                 !! first layer in range for soil variables (0 assumes all layers are modified)
        integer :: lyr2                 !! last layer in range for soil variables (0 assumes through last layer)
        integer :: year1                !! first year for precip and temp
        integer :: year2                !! last year for precip and temp
        integer :: day1                 !! first day in range for precip and temp
        integer :: day2                 !! last day in range for precip and temp
        integer :: num_tot = 0          !! total number of integers read in
        integer :: num_elem = 0         !! total number of elements modified (ie - 1 -5 18; num_tot=3 and num_elem=6)
        integer, dimension(:), allocatable :: num
        integer :: num_cond
        type (calibration_conditions), dimension(:), allocatable :: cond
      end type update_parameters

      type (update_parameters), dimension (:), allocatable :: cal_upd   !dimensioned to db_mx%cal_parms
      type (update_parameters) :: chg

      type update_conditional
        character(len=16) :: typ        !! type of update schedule (parameter, structure, land_use_mgt)
        character(len=16) :: name       !! name of update schedule
        character(len=16) :: cond       !! points to ruleset in conditional.ctl for scheduling the update
        integer :: cond_num             !! integer pointer to d_table in conditional.ctl
      end type update_conditional
      type (update_conditional), dimension (:), allocatable :: upd_cond
      
      type soft_calibration_codes
        character (len=1) :: hyd_hru = 'n'      !! if y, calibrate hydrologic balance for hru by land use in each region
        character (len=1) :: hyd_hrul = 'n'     !! if y, calibrate hydrologic balance for hru_lte by land use in each region
        character (len=1) :: plt = 'n'          !! if y, calibrate plant growth by land use (by plant) in each region
        character (len=1) :: sed = 'n'          !! if y, calibrate sediment yield by land use in each region  
        character (len=1) :: nut = 'n'          !! if y, calibrate nutrient balance by land use in each region
        character (len=1) :: chsed = 'n'        !! if y, calibrate channel widening and bank accretion by stream order
        character (len=1) :: chnut = 'n'        !! if y, calibrate channel nutrient balance by stream order
        character (len=1) :: res = 'n'          !! if y, calibrate reservoir budgets by reservoir
      end type soft_calibration_codes
      type (soft_calibration_codes) :: cal_codes
      
      type soft_calib_parms
        character(len=16) :: name       !! cn2, terrace, land use, mgt, etc.
        integer :: num_db = 0           !! crosswalk number of parameter, structure or land use to get database array number
        character(len=16) :: chg_typ    !! type of change (absval,abschg,pctchg)
        real :: neg                     !! negative limit of change
        real :: pos                     !! positive limit of change
        real :: lo                      !! lower limit of parameter
        real :: up                      !! upper limit of parameter
      end type soft_calib_parms
      type (soft_calib_parms), dimension(:), allocatable :: ls_prms
      type (soft_calib_parms), dimension(:), allocatable :: pl_prms
      type (soft_calib_parms), dimension(:), allocatable :: ch_prms
            
      type soft_calib_ls_adjust
        real :: cn = 0.         !+/- or 0/1       |cn2 adjustment or at limit
        real :: esco = 0.       !+/- or 0/1       |esco adjustment or at limit
        real :: lat_len = 0.    !+/- or 0/1       |lateral flow soil length adjustment or at limit
        real :: k_lo = 0.       !+/- or 0/1       |k (lowest layer) adjustment or at limit
        real :: slope = 0.      !+/- or 0/1       |slope adjustment or at limit        
        real :: tconc = 0.      !+/- or 0/1       |time of concentration adjustment or at limit
        real :: etco = 0.       !+/- or 0/1       |etco adjustment or at limit
        real :: perco = 0.      !+/- or 0/1       |percolation coefficient adjustment or at limit
        real :: revapc = 0.     !+/- or 0/1       |slope adjustment or at limit
        real :: cn3_swf = 0.    !+/- or 0/1       |cn3_swf adjustment or at limit
      end type soft_calib_ls_adjust

      type soft_calib_ls_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name = 'default'
        ! srr + lfr + pcr + etr + tfr = 1
        real :: srr = 0.    !- or m3        |surface runoff ratio - surface runoff/precip
        real :: lfr = 0.    !- or m3        |lateral flow ratio - soil lat flow/precip 
        real :: pcr = 0.    !- or m3        |percolation ratio - perc/precip
        real :: etr = 0.    !- or m3        |et ratio - et/precip
        real :: tfr = 0.    !- or m3        |tile flow ratio - tile flow/total runoff 
        real :: sed = 0.    !t/ha or t      |sediment yield
        real :: orgn = 0.   !kg/ha or kg    |organic n yield
        real :: orgp = 0.   !kg/ha or kg    |organic p yield
        real :: no3 = 0.    !kg/ha or kg    |nitrate yield
        real :: solp = 0.   !kg/ha or kg    |soluble p yield
      end type soft_calib_ls_processes
      type (soft_calib_ls_processes) :: lscal_z  !to zero values

      type ls_calib_regions
        character(len=16) :: name = 'default'
        integer :: lum_no                                       !xwalk lum()%name with lscal()%lum()%name
        real :: ha                                              !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_ls_processes) :: meas                  !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        real :: precip = 0.                                     !model precip for each land use to determine ratios
        real :: precip_aa = 0.                                  !model ave annual precip for each land use to determine ratios
        real :: precip_aa_sav = 0.                              !model ave annual precip for each land use to determine ratios for final output
        type (soft_calib_ls_processes) :: sim                   !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_ls_processes) :: aa                    !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_ls_processes) :: prev                  !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_ls_adjust) :: prm                      !parameter adjustments used in landscape calibration
        type (soft_calib_ls_adjust) :: prm_prev                 !parameter adjustments used in landscape calibration
        type (soft_calib_ls_adjust) :: prm_lim                  !code if parameters are at limits
      end type ls_calib_regions
      
      type cataloging_units
        character(len=16) :: name = 'basin'                     !name of region - (number of regions = db_mx%lsu_reg)
        real :: area_ha                                         !area of landscape cataloging unit -hectares
        integer :: num_tot                                      !number of hru's in each region
        integer, dimension(:), allocatable :: num               !hru's that are included in the region
        integer :: nlum                                         !number of land use and mgt in the region
        integer :: lscal                                        !points to soft calibration data
        integer, dimension(:), allocatable :: lum_num           !db nunmber of land use in the region - dimensioned by lum in the region
        integer, dimension(:), allocatable :: lum_num_tot       !db nunmber of land use in the region each year- dimensioned by lum in database
        real, dimension(:), allocatable :: lum_ha               !area (ha) of land use in the region - dimensioned by lum in the region
        real, dimension(:), allocatable :: lum_ha_tot           !sum of area (ha) of land use in the region each year- dimensioned by lum in database
        real, dimension(:), allocatable :: hru_ha               !area (ha) of hrus in the region 
      end type cataloging_units
      type (cataloging_units), dimension(:), allocatable :: region     !dimension by region for hru's
      type (cataloging_units), dimension(:), allocatable :: ccu_cal    !channel cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: acu_cal    !aquifer cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: rcu_cal    !reservoir cataoging unit region
      type (cataloging_units), dimension(:), allocatable :: pcu_cal    !point source cataoging unit region
      
      type landscape_units
        character(len=16) :: name = 'basin'                     !name of region - (number of regions = db_mx%lsu_out)
        real :: area_ha                                         !area of landscape cataloging unit -hectares
        integer :: num_tot                                      !number of hru's in each region
        integer, dimension(:), allocatable :: num               !hru's that are included in the region
      end type landscape_units
      type (landscape_units), dimension(:), allocatable :: lsu_out     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: lsu_reg     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: acu_out     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: acu_reg     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: ccu_out     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: ccu_reg     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: rcu_out     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: rcu_reg     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: pcu_out     !dimension by region for hru's
      type (landscape_units), dimension(:), allocatable :: pcu_reg     !dimension by region for hru's
      
      type landscape_elements
        character(len=16) :: name
        integer :: obj = 1              !object number
        character (len=3) :: obtyp      !object type- 1=hru, 2=hru_lte, 11=export coef, etc
        integer :: obtypno = 0          !2-number of hru_lte's or 1st hru_lte command
        real :: bsn_frac = 0            !fraction of element in basin (expansion factor)
        real :: sub_frac = 0            !fraction of element in sub (expansion factor)
        real :: reg_frac = 0            !fraction of element in calibration region (expansion factor)
      end type landscape_elements
      type (landscape_elements), dimension(:), allocatable :: lsu_elem       !landscape cataoging unit
      type (landscape_elements), dimension(:), allocatable :: ccu_elem       !channel cataoging unit
      type (landscape_elements), dimension(:), allocatable :: acu_elem       !aquifer cataoging unit
      type (landscape_elements), dimension(:), allocatable :: rcu_elem       !reservoir cataoging unit
      type (landscape_elements), dimension(:), allocatable :: pcu_elem       !point source cataoging unit
      
      type soft_data_calib_landscape
        character(len=16) :: name = 'default'                               !name of region - (number of regions = db_mx%lsu_reg)
        integer :: lum_num                                                  !number of land uses in each region
        integer :: num_tot                                                  !number of hru's in each region
        integer, dimension(:), allocatable :: num                           !hru's that are included in the region
        integer :: num_reg                                                  !number of regions the soft data applies to
        character(len=16), dimension(:), allocatable :: reg                 !name of regions the soft data applies to
        integer, dimension(:), allocatable :: ireg                          !name of regions the soft data applies to
        type (ls_calib_regions), dimension(:), allocatable :: lum           !dimension for land uses within a region
      end type soft_data_calib_landscape
      type (soft_data_calib_landscape), dimension(:), allocatable :: lscal  !dimension by region for hru's
      type (soft_data_calib_landscape), dimension(:), allocatable :: lscalt !dimension by region for hru_lte's

      type soft_calib_pl_adjust
        real :: stress = 0.     !+/- or 0/1     |plant stress (pest, soil, etc) or at limit
      end type soft_calib_pl_adjust
      
      type soft_calib_pl_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name = 'default'
        real :: yield = 0.      !t/ha or t      |crop yield
        real :: npp = 0.        !t/ha or t      |net primary productivity (biomass) dry weight
        real :: lai_mx = 0.     !               |maximum leaf area index
        real :: wstress = 0.    !               |sum of water (drought) stress
        real :: astress = 0.    !               |sum of water (aeration) stress
        real :: tstress = 0.    !               |sum of temperature stress
      end type soft_calib_pl_processes
      type (soft_calib_pl_processes) :: plcal_z  !to zero values

      type pl_calib_regions
        character(len=16) :: name = 'default'
        integer :: lum_no                                       !xwalk lum()%name with lscal()%lum()%name
        real :: ha                                              !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_pl_processes) :: meas                  !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        real :: precip = 0.                                     !model precip for each land use to determine ratios
        real :: precip_aa = 0.                                  !model ave annual precip for each land use to determine ratios
        real :: precip_aa_sav = 0.                              !model ave annual precip for each land use to determine ratios for final output
        type (soft_calib_pl_processes) :: sim                   !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_pl_processes) :: aa                    !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_pl_processes) :: prev                  !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_pl_adjust) :: prm                      !parameter adjustments used in landscape calibration
        type (soft_calib_pl_adjust) :: prm_prev                 !parameter adjustments used in landscape calibration
        type (soft_calib_pl_adjust) :: prm_lim                  !code if parameters are at limits
      end type pl_calib_regions
      
      type soft_data_calib_plant
        character(len=16) :: name = 'default'   !name of region - (number of regions = db_mx%lsu_reg)
        integer :: lum_num                                                  !number of land uses in each region
        integer :: num_tot                                                  !number of hru's in each region
        integer, dimension(:), allocatable :: num                           !hru's that are included in the region
        type (pl_calib_regions), dimension(:), allocatable :: lum           !dimension for land uses within a region
      end type soft_data_calib_plant
      type (soft_data_calib_plant), dimension(:), allocatable :: plcal      !dimension by region for plants

      type soft_calib_chan_adjust
        real :: cov = 0.            !+/- or 0/1     |cover adjustment or at limit
        real :: erod = 0.           !+/- or 0/1     |channel erodibility adjustment or at limit
        real :: shear_bnk = 0.      !+/- or 0/1     |bank shear coefficient adjustment or at limit
        real :: hc_erod = 0.        !+/- or 0/1     |head cut erodibility adjustment or at limit
      end type soft_calib_chan_adjust
      
      type soft_calib_chan_processes
        !database of soft ave annual landscape calibration values
        character(len=16) :: name
        real :: chw = 0.    !mm/yr          |channel widening 
        real :: chd = 0.    !mm/yr          |channel downcutting or accretion
        real :: hc = 0.     !m/yr           |head cut advance
        real :: fpd = 0.    !mm/yr          |flood plain accretion 
      end type soft_calib_chan_processes
      type (soft_calib_chan_processes) :: chcal_z  !to zero values

      type chan_calib_regions
        character(len=16) :: name = 'default'
        real :: length                                          !ha of each land use
        integer :: nbyr = 0                                     !number of years the land use occurred 
        type (soft_calib_chan_processes) :: meas                !input soft calibration parms of each land use - ratio,t/ha,kg/ha
        type (soft_calib_chan_processes) :: sim                 !simulated sum of soft calibration parms of each land use - m3,t,kg
        type (soft_calib_chan_processes) :: aa                  !average annual soft calibration parms of each land use - mm,t/ha,kg/ha
        type (soft_calib_chan_processes) :: prev                !simulated sum of soft calibration parms of previous run - m3,t,kg
        type (soft_calib_chan_adjust) :: prm                    !parameter adjustments used in landscape calibration
        type (soft_calib_chan_adjust) :: prm_prev               !parameter adjustments used in landscape calibration
        type (soft_calib_chan_adjust) :: prm_lim                !code if parameters are at limits
      end type chan_calib_regions
      
      type soft_data_calib_channel
        character(len=16) :: name = 'default'   !name of region - (number of regions = db_mx%lsu_reg)
        integer :: ord_num                                                  !number of stream orders in each region
        integer :: num_tot                                                  !number of channels in each region
        integer, dimension(:), allocatable :: num                           !channels that are included in the region
        type (chan_calib_regions), dimension(:), allocatable :: ord         !dimension for stream order within a region
      end type soft_data_calib_channel
      type (soft_data_calib_channel), dimension(:), allocatable :: chcal  !dimension by region
      
      type structural_practices
        character(len=13) :: name = 'default'
        integer :: num_pr                                 
        character(len=16), dimension(:), allocatable :: prac        !! terrace, tile, contour, filter, stripcrop
                                                                    !! fire, grassww, plantup, resman, user_def, septic       
        character(len=13), dimension(:), allocatable :: prac_typ    !! points to appropriate data file 
        integer, dimension(:), allocatable :: prac_num
      end type structural_practices
      type (structural_practices),dimension (:), allocatable :: str_init
      
      type structural_ops
        character(len=16) :: name
        integer :: op = 0         !! |none          |operation code number
        integer :: mon = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: year = 0
        integer :: husc = 0
        integer :: op1 = 0        !! |none          |database number (locator)
      end type structural_ops
      type (structural_ops) :: str

      type soiltest_db
        character(len=16) :: name = 'default'
        real :: exp_co = .001         !	     |depth coefficient to adjust concentrations for depth
        real :: totaln = 13.          !ppm     |total N in soil
        real :: inorgn = 6.           !ppm     |inorganic N in soil surface
        real :: orgn = 3.             !ppm     |organic N in soil surface
        real :: totalp = 3.           !ppm     |total P in soil surface
        real :: inorgp = 3.5          !ppm     |inorganic P in soil surface
        real :: orgp = .4             !ppm     |organic P in soil surface
        real :: watersol_p = .15      !ppm     |water soluble P in soil surface    
        real :: h3a_p = .25           !ppm     |h3a P in soil surface        
        real :: mehlich_p = 1.2       !ppm     |Mehlich P in soil surface
        real :: bray_strong_p = .85   !ppm     |Bray P in soil surface
      end type soiltest_db
      type (soiltest_db), dimension (:), allocatable :: solt_db

      type topography_db
        character(len=16) :: name = 'default'
        real :: slope = .02       !!	hru_slp(:) |m/m           |average slope steepness in HRU
        real :: slope_len = 50.   !! slsubbsn(:)   |m             |average slope length for erosion
        real :: lat_len = 50.     !! slsoil(:)     |m             |slope length for lateral subsurface flow
        real :: dis_stream = 100. !! dis_stream(:) |m             |average distance to stream
        real :: dep_co = 1.       !!               |              |deposition coefficient
      end type topography_db
      type (topography_db), dimension (:), allocatable :: topo_db

      type fields_db
           character(len=16) :: name = 'default'
           real :: length = 500. !!               |m             |field length for wind erosion
           real :: wid = 100.    !!               |m             |field width for wind erosion
           real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type fields_db
      type (fields_db), dimension (:), allocatable :: field_db
      
      type hydrology_db
         character(len=16) :: name
         real :: lat_ttime = 0.  !! lat_ttime(:)  |none          |Exponential of the lateral flow travel time
         real :: lat_sed = 0.    !! lat_sed(:)    |g/L           |sediment concentration in lateral flow
         real :: canmx = 0.      !! canmx(:)      |mm H2O        |maximum canopy storage
         real :: esco = 0.       !! esco(:)       |none          |soil evaporation compensation factor (0-1)
         real :: epco = 0.       !! epco(:)       |none          |plant water uptake compensation factor (0-1)
         real :: erorgn = 0.     !! erorgn(:)     |none          |organic N enrichment ratio, if left blank
                                 !!                              |the model will calculate for every event
         real :: erorgp = 0.     !! erorgp(:)     |none          |organic P enrichment ratio, if left blank
                                 !!                              |the model will calculate for every event
         real :: cn3_swf = 0.    !!               |none          |soil water at cn3 - 0=fc; .99=near saturation
         real :: biomix = 0.     !! biomix(:)     |none          |biological mixing efficiency.
                                 !!                              |Mixing of soil due to activity of earthworms
                                 !!                              |and other soil biota. Mixing is performed at
                                 !!                              |the end of every calendar year.
         real :: dep_imp = 0.    !! dep_imp(:)    |mm            |depth to impervious layer
         real :: lat_orgn = 0.   !!               |ppm           |organic N concentration in lateral flow
         real :: lat_orgp = 0.   !!               |ppm           |organic P concentration in lateral flow
         real :: harg_pet  = .0023  !!            |              |coefficient related to radiation used in 
                                 !!                              | Hargreaves equation
         real :: cncoef = 0.3    !!               |              |plant ET curve number coefficient
         real :: perco = 1.      !!               |              |percolation coefficient-adjusts soil mositure
                                 !!                              | for perc to occur (1.0 = fc)
       end type hydrology_db
        type (hydrology_db), dimension (:), allocatable :: hyd_db

      type land_use_management
        character (len=16) :: name = " "
        character (len=16) :: plant_cov = ""
        character (len=35) :: mgt_ops = ""
        character (len=16) :: cn_lu      !! none     | land use for curve number table (cntable.lum)
        character (len=16) :: cons_prac  !! none     | conservation practice from table (cons_practice.lum)
        character (len=16) :: urb_lu     !! none     | type of urban land use- ie. residential, industrial, etc (urban.urb)
        character (len=16) :: urb_ro     !! none     | urban runoff model
                                         !!          | "usgs_reg", simulate using USGS regression eqs
                                         !!          | "buildup_washoff", simulate using build up/wash off alg       
        character (len=16) :: ovn        !! none     | Manning's "n" land use type for overland flow (ovn_table.lum)
        !integer :: urb_lu = 0           !! none     | urban land type identification number
        !integer :: iurban = 0           !! none     | urban simulation code 
        !                                !!          | 0  no urban sections in HRU
        !                                !!          | 1  urban sections in HRU, simulate using USGS regression eqs
        !                                !!          | 2  urban sections in HRU, simulate using build up/wash off alg
        !real :: ovn = 0.1               !! none     | Manning's "n" value for overland flow
        character (len=25) :: tiledrain
        character (len=25) :: septic
        character (len=25) :: fstrip
        character (len=25) :: grassww
        character (len=25) :: bmpuser
      end type land_use_management
      type (land_use_management), dimension (:), allocatable :: lum
      
      type land_use_structures
        integer :: plant_cov = 0
        integer :: mgt_ops = 0
        integer :: cn_lu = 0
        integer :: cons_prac = 0
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: bmpuser = 0
      end type land_use_structures
      type (land_use_structures), dimension (:), allocatable :: lum_str
           
      type curvenumber_table
        character(len=16) :: name                      !name includes abbrev for lu/treatment/condition 
        real, dimension(4) :: cn = (/30.,55.,70.,77./) !curve number
      end type curvenumber_table
      type (curvenumber_table), dimension (:), allocatable :: cn
                 
      type conservation_practice_table
        character(len=16) :: name                   !name of conservation practice
        real :: pfac = 1.0                          !usle p factor
        real :: sl_len_mx = 1.0             !m      !maximum slope length
      end type conservation_practice_table
      type (conservation_practice_table), dimension (:), allocatable :: cons_prac
                       
      type overlandflow_n_table
        character(len=16) :: name                   !name of conservation practice
        real :: ovn = 0.5                           !overland flow mannings n - mean
        real :: ovn_min = 0.5                       !overland flow mannings n - min
        real :: ovn_max = 0.5                       !overland flow mannings n - max
      end type overlandflow_n_table
      type (overlandflow_n_table), dimension (:), allocatable :: overland_n
      
      type subsurface_drainage
        character(len=13) :: name = "default"
        real :: depth = 0.    !! |mm            |depth of drain tube from the soil surface
        real :: time = 0.     !! |hrs           |time to drain soil to field capacity
        real :: lag = 0.      !! |hours         |drain tile lag time
        real :: radius =0.    !! |mm		       effective radius of drains
        real :: dist = 0.     !! |mm            |distance between two drain tubes or tiles
        real :: drain_co      !! |mm/day        |drainage coefficient 
        real :: pumpcap = 0.  !! |mm/hr         |pump capacity (default pump capacity = 1.042mm/hr or 25mm/day)
        real :: latksat = 0.  !! |none          |multiplication factor to determine conk(j1,j) from sol_k(j1,j) for HRU 
      end type subsurface_drainage
      type (subsurface_drainage), dimension (:), allocatable :: sdr
       
      type pothole_db
          character(len=13) :: name
          real :: frac  = 0.       !! km2/km2       |frac of HRU area that drains into pothole
          real :: tilemm = 0.      !! m3/d          |average daily outflow to main channel from tile flow
                                   !!                   if drainage tiles are installed in pothole
          real :: volxmm = 0.      !! m**3 H2O      |max volume of water stored in the depression/imp area
          real :: volmm = 0.       !! m**3 H2O      |current volume of water stored in the depression/imp area 
          real :: nsed = 0.        !! mg/L          |normal sed conc in impounded water
          real :: no3l = 0.        !! 1/day         |nitrate decay rate in impounded area
          real :: solpl = 0.       !! kg N          |amount of soluble p in pothold water body
          real :: k = -1.          !! mm/hr         |saturated hydraulic condcutivity of soil layer
      end type pothole_db
      type (pothole_db), dimension (:), allocatable :: potdb  
      
      type snow_database
         character (len=16) :: name
         real :: falltmp = 0.     !deg C         |snowfall temp
         real :: melttmp = 0.     !deg C         |snow melt base temp 
         real :: meltmx = 0.      !mm/deg C/day  |Max melt rate for snow during year (June 21)
         real :: meltmn = 0.      !mm/deg C/day  |Min melt rate for snow during year (Dec 21)
         real :: timp             !none          |snow pack temp lag factor (0-1)
         real :: covmx = 0.       !mm H20        |Min snow water content
         real :: cov50 = 0.       !none          |frac of COVMX
      end type snow_database
      type (snow_database), dimension (:), allocatable :: snodb
       
      type soilayer_db
        real :: z = 1500.           !! mm             |depth to bottom of soil layer
        real :: bd = 1.3            !! Mg/m**3        |bulk density of the soil
        real :: awc = 0.2           !! mm H20/mm soil |available water capacity of soil layer
        real :: k = 10.0            !! mm/hr          |saturated hydraulic conductivity of soil layer. Index:(layer,HRU)
        real :: cbn = 2.0           !! %              |percent organic carbon in soil layer
        real :: clay = 10.          !! none           |fraction clay content in soil material (UNIT CHANGE!)
        real :: silt = 60.          !! %              |percent silt content in soil material 
        real :: sand = 30.          !! none           |fraction of sand in soil material
        real :: rock = 0.           !! %              |percent of rock fragments in soil layer      
        real :: alb = 0.1           !! none           |albedo when soil is moist
        real :: usle_k = 0.2        !!                |USLE equation soil erodibility (K) factor 
        real :: ec = 0.             !! dS/m           |electrical conductivity of soil layer
        real :: cal = 0.            !! %              |soil CaCo3
        real :: ph = 0.             !!                |soil Ph
      end type soilayer_db
      
      type soil_profile_db
        character(len=20) :: snam = " "       !! NA            |soil series name 
        integer ::  nly  = 1                  !! none          |number of soil layers  
        character(len=16) :: hydgrp = "A"      !! NA            |hydrologic soil group
        real :: zmx = 1500.                   !! mm            |maximum rooting depth
        real :: anion_excl = 0.5              !! none          |fraction of porosity from which anions are excluded
        real :: crk = 0.01                    !! none          |crack volume potential of soil
        character(len=16) :: texture = " "    !!               |texture of soil
      end type soil_profile_db
      
      type soil_database
         type (soil_profile_db) :: s
         type (soilayer_db), dimension(:), allocatable :: ly
      end type soil_database
      type (soil_database), dimension(:), allocatable :: soildb
      
      type plant_db
        character(len=16) :: plantnm = 'frsd ' !N/A    |4 letter char code represents crop name
        integer :: idc = 7        !none               |crop/landcoover category
                                  !                   |1 warm annual legume
                                  !                   |2 cold season annual legume
                                  !                   |3 perennial legume
                                  !                   |4 warm season annual
                                  !                   |5 cold season annual
                                  !                   |6 perennial
                                  !                   |7 trees
                                  !                   |8 tropical trees
                                  !                   |9 tropical grasses
        real :: phu = 2500.        !heat units   |total number of heat units to bring crop to maturity
        real :: bio_e = 15.0             !(kg/ha/(MJ/m**2)  |biomass-energy ratio
        real :: hvsti = 0.76             !(kg/ha)/(kg/ha)   |harvest index: crop yield/aboveground biomass
        real :: blai = 5.0               !none              |max (potential) leaf area index
        real :: frgrw1 = 0.05            !none              |fraction of the growing season corresponding to the
                                         !                  |  1st point on optimal leaf area development curve
        real :: laimx1 = 0.05            !none              |frac of max leaf area index corresponding to the 
                                         !                  |  1st point on optimal leaf area development curve
        real :: frgrw2 = 0.4             !none              |fraction of the growing season corresponding to the 
                                         !                  |  2nd point on optimal leaf area development curve 
        real :: laimx2 = 0.95            !none              |fraction of max leaf area index corresponding to the
                                         !                  |  2nd point on optimal leaf area development curve
        real :: dlai = 0.99              !none              |frac of growing season when leaf are declines
        real :: chtmx = 6.0              !m                 |maximum canopy height
        real :: rdmx = 3.5               !m                 |maximum root depth
        real :: t_opt = 30.              !deg C             |optimal temp for plant growth
        real :: t_base = 10.             !deg C             |minimum temp for plant growth
        real :: cnyld = 0.0015           !kg N/kg yld       |frac of nitrogen in yield
        real :: cpyld = 0.0003           !kg P/kg yld       |frac of phosphorus in yield
        real :: pltnfr1 = 0.006          !kg N/kg biomass   |nitrogen uptake parm #1
        real :: pltnfr2 = 0.002          !kg N/kg biomass   |nitrogen uptake parm #2 
        real :: pltnfr3 = 0.0015         !kg N/kg/biomass   |nitrogen uptake parm #3
        real :: pltpfr1 = 0.0007         !kg P/kg/biomass   |phoshorus uprake parm #1
        real :: pltpfr2 = 0.0004         !kg P/kg/biomass   |phoshorus uprake parm #2
        real :: pltpfr3 = 0.0003         !kg P/kg/biomass   |phoshorus uprake parm #3
        real :: wsyf = 0.01              !(kg/ha)/(kg/ha)   |value of harvest index bet 0 and HVSTI
        real :: usle_c = 0.001           !none              |minimum value of the USLE C factor for water erosion
        real :: gsi = 0.002              !m/s               |maximum stomatal conductance
        real :: vpdfr = 4.               !kPa               |vapor pressure deficit at which GMAXFR is valid
        real :: gmaxfr = 0.75            !none              |fraction of max stomatal conductance that is 
                                         !                    achieved at the vapor pressue deficit defined by VPDFR
        real :: wavp = 8.                !none              |rate of decline in radiation use efficiency
        real :: co2hi = 660.             !uL CO2/L air      |CO2 concentration higher than the ambient corresponding
                                         !                    to the 2nd point on radiation use efficiency curve             
        real :: bioehi = 16.             !(kg/ha)/(MJ/m**2) |biomass-energy ratio when plant is in an environment with 
                                         !                    CO2 level equal to the value of CO2HI.
        real :: rsdco_pl = 0.05          !none              |plant residue decomposition coeff
        real :: alai_min = 0.75          !m**2/m**2         |min LAI during winter dormant period
        real :: laixco_tree = 0.3        !none              |coefficient to estimate max lai during tree growth
        integer :: mat_yrs = 10          !years             |year to maturity  
        real :: bmx_peren = 1000.        !metric tons/ha    |max biomass for forest (trees only)
        real :: ext_coef = 0.65          !                  |light extinction coefficient
        real :: bm_dieoff = 1.           !frac              |above ground biomass that dies off at dormancy
        real :: rsr1 = 0.                !                  |initial root to shoot ratio at the beg of growing season
        real :: rsr2 = 0.                !                  |root to shoot ratio at the end of the growing season
        real :: pop1 = 0.                !plants/m^2        |plant population corresponding to the 1st point on the
                                         !                             population lai curve
        real :: frlai1 = 0.              !frac              |frac of max leaf area index corresponding to the 1st 
                                         !                     point on the leaf area development curve
        real :: pop2 = 0.                !plants/m^2        |plant population corresponding to the 2nd point on the
                                         !                             population lai curve
        real :: frlai2 = 0.              !frac              |frac of max leaf area index corresponding to the 2nd 
                                         !                     point on the leaf area development curve
        real :: frsw_gro = .5            !frac              |frac of field capacity to initiate growth of tropical 
                                         !                     plants during monsoon season - pcom()%plcur()%iseason
        real :: wind_stl = 0.            !                  |wind erosion factor for standing live biomass
        real :: wind_std = 0.            !                  |wind erosion factor for standing dead residue
        real :: wind_flat = 0.           !                  |wind erosion factor for flat residue
      end type plant_db
      type (plant_db), dimension(:),allocatable, target, save :: pldb
      type (plant_db), pointer :: pl_db
         
      type plant_cp
        real :: popsc1 = 0.
        real :: popsc2 = 0.
        real :: leaf1 = 0.       !none      |1st shape parameter for leaf area
        real :: leaf2 = 0.       !none      |2nd leaf parameter for leaf area
        real :: ruc1 = 0.        !none      |1st shape parameter for radiation use efficiency equation
        real :: ruc2 = 0.        !none      |2nd shape parameter for radiation use efficiency equation
        real :: nup1 = 0.        !none      |1st shape parameter for plant N uptake equation      
        real :: nup2 = 0.        !none      |2nd shape parameter for plant N uptake equation
        real :: pup1 = 0.        !none      |1st shape parameter for plant P uptake equation
        real :: pup2 = 0.        !none      |2nd shape parameter for plant P uptake equation
        real :: gmaxfr = 0.      !none      |fraction of max stomatal conductance that is 
                                 !            achieved at the vapor pressue deficit defined by VPDFR
        real :: vpdfr = 0.       !kPa       |vapor pressure deficit at which GMAXFR is valid
        real :: cvm = 0.         !frac      |fraction of the maximum leaf area index corresponding
                                 !            to the second point of the optimal leaf area dev curve
        real :: vpd2 = 0.        !kPa       |vapor pressure deficit corresponding to the second point
                                 !            on the stomatal conductance curve
      end type plant_cp
      type (plant_cp), dimension(:),allocatable, target, save :: plcp
      type (plant_cp), pointer :: pl_cp
            
      type plant_init_db
        character(len=16) :: cpnm = "frsd"
        integer :: db_num = 1      !             |plant object
        integer :: igro = 1        !             |land cover status
                                   !               0 = no land cover growing
                                   !               1 = land cover growing
        real :: lai = 0.           !m**2/m**2    |leaf area index
        real :: bioms = 0.         !kg/ha        |land cover/crop biomass
        real :: phuacc = 0.        !             |frac of plant heat unit acc.
        real :: pop = 0.
        real :: yrmat = 20.        !years        |years to maturity 
        real :: rsdin = 10000.     !kg/ha        |initial residue cover
      end type plant_init_db
      
      type plant_community_db   
        character(len=20) :: name = "frsd_frsd"
        integer :: plants_com = 1
        type (plant_init_db), dimension(:), allocatable :: pl
      end type plant_community_db
      type (plant_community_db), dimension(:), allocatable :: pcomdb

      type routing_nut_data         ! used for 2-stage ditch in chandeg and overland flow
        character(len=16) :: name = 'Drainage_Ditch'
        real :: len_inc = 250       ! m               |segment length for reduction
        real :: no3_slp = 0.86      ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int = 0.17      ! mgN/m2/h        |intercept of denitrification rate equation
        real :: no3_slp_ob = 0.48   ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int_ob = 1.30   ! mgN/m2/h        |intercept of denitrification rate equation
        real :: no3_slp_ub = 1.50   ! (mgN/m2/h)/ppm  |slope of denitrification (y-axis) and inflow no3 (x-axis)
        real :: no3_int_ub = 0.03   ! mgN/m2/h        |intercept of denitrification rate equation
        real :: turb_slp = -.0002   ! (del ppm/ppm)   |slope of turbidity reduction (y) and inflow turbidity (x)
        real :: turb_int = 0.175    ! ppm             |intecept of turbidity reduction equation
        real :: tss_slp = 0.457     ! (del ppm/ppm)   |slope of total suspended solids (y) and inflow turbidity (x)
        real :: tss_int = 0.534     ! ppm             |intecept of tss reduction equation
        real :: tp_slp = 0.375      ! (del ppm/ppm)   |slope of total P reduction (y) and turbidity reduction (x)
        real :: tp_int = 1.312      ! ppm             |intecept of total P reduction equation
        real :: srp_slp = 0.646     ! (del ppm/ppm)   |slope of soluble reactive P reduction (y) and total P reduction (x)
        real :: srp_int = 0.207     ! ppm             |intecept of soluble reactive P reduction equation
        real :: turb_tss_slp = .35  ! ppm             |slope of turbidity and total suspended solids (0.2-0.4)
        real :: no3_min_conc = .05  ! ppm             |minimum no3 concentration
        real :: tp_min_conc = .06   ! ppm             |minimum tp concentration
        real :: tss_min_conc = 5    ! ppm             |minimum tss concentration
        real :: srp_min_conc = .015 ! ppm             |minimum srp concentration
      end type routing_nut_data
      type (routing_nut_data), dimension(:), allocatable :: rte_nut
      
      type channel_initial
        character(len=13) :: name
        real :: vol               !m**3          |res vol (read in as frac principal and converted to m^3)
        real :: sed               !kg/L          |amt of sed in res (read in as mg/L and converted to kg/L)
        real :: orgn              !kg N          |amt of org N in res (read in as mg/L and converted to kg/L)
        real :: no3               !kg N          |amt of nitrate in res (read in as mg/L and converted to kg/L)
        real :: no2               !kg N          |amt of nitrite in res (read in as mg/L and converted to kg/L)
        real :: nh3               !kg N          |amt of ammonia in res (read in as mg/L and converted to kg/L)
        real :: orgp              !kg P          |amt of org P in res (read in as mg/L and converted to kg/L)
        real :: solp              !kg P          |amt of soluble P in res (read in as mg/L and converted to kg/L)
        real :: seci              !m             |secchi-disk depth 
        real :: san               !kg/L          |amt of san in res (read in as mg/L and converted to kg/L)
        real :: sil               !kg/L          |amt of sil in res (read in as mg/L and converted to kg/L)
        real :: cla               !kg/L          |amt of cla in res (read in as mg/L and converted to kg/L)
        real :: sag               !kg/L          |amt of sag in res (read in as mg/L and converted to kg/L)
        real :: lag               !kg/L          |amt of lag in res (read in as mg/L and converted to kg/L)
        real :: gra               !kg/L          |amt of gra in res (read in as mg/L and converted to kg/L)
        real :: chla              !kg chl-a      |amt of chlorophyll-a in res
        real :: psol              !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: psor              !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: bactlp            !# cfu/100ml   |less persistent bacteria stored in res
        real :: bactp             !# cfu/100ml   |persistent bacteria stored in res
      end type channel_initial
      type (channel_initial), dimension(:),allocatable :: ch_init
      
      
      type channel_data_char_input
        character(len=13) :: name = "default"
        character(len=16) :: init                       !initial data-points to initial.res
        character(len=16) :: hyd                        !points to hydrology.res for hydrology inputs
        character(len=16) :: sed                        !sediment inputs-points to sediment.res
        character(len=16) :: nut                        !nutrient inputs-points to nutrient.res
        character(len=16) :: pst                        !pesticide inputs-points to pesticide.res
        character(len=16) :: ls_lnk                     !landscape linkage-points to ch_ls_link?
        character(len=16) :: aqu_lnk                    !aquifer linkage-points to ch_aqu_link
      end type channel_data_char_input
      type (channel_data_char_input), dimension(:), allocatable :: ch_dat_c


      type channel_data
        character(len=13) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
        integer :: pst = 0                    !pesticide inputs-points to pesticide.res
        integer :: ls_lnk = 0                 !landscape linkage-points to ch_ls_link?
        integer :: aqu_lnk = 0                !aquifer linkage-points to ch_aqu_link
      end type channel_data
      type (channel_data), dimension(:), allocatable :: ch_dat
            
      type channel_hyd_data
        !variables are conditional on res_dat()%hyd = 0 for reservoirs and 1 for hru impounding
        !surface areas are ha for 0 and frac of hru for 1; volumes are ha-m for 0 and mm for 1
        !br1 and br2 are used for 0 and acoef for 0 -- for surface area - volume relationship
        character(len=13) :: name = "default"
        real :: w = 2.           ! m             |average width of main channel
        real :: d = .5           ! m             |average depth of main channel
        real :: s = .01          ! m/m           |average slope of main channel
        real :: l = 0.1          ! km            |main channel length in subbasin
        real :: n = .05          ! none          |Manning's "n" value for the main channel
        real :: k = 0.01         ! mm/hr         |effective hydraulic conductivity of main channel alluvium
        real :: wdr = 6.         ! m/m           |channel width to depth ratio
        real :: alpha_bnk = 0.03 ! days          |alpha factor for bank storage recession curve
        real :: side = 0.        !               |change in horizontal distance per unit
      end type channel_hyd_data
      type (channel_hyd_data), dimension(:), allocatable :: ch_hyd
      
      type channel_sed_data
        character(len=13) :: name
        integer :: eqn  = 0      !               |sediment routine methods: 
                                   !                   0 = original SWAT method
                                   !                   1 = Bagnold's
                                   !                   2 = Kodatie
                                   !                   3 = Molinas WU
                                   !                   4 = Yang
        real :: cov1 = 0.1       ! none          |channel erodibility factor (0.0-1.0)
        real :: cov2 = 0.1       ! none          |channel cover factor (0.0-1.0)
        real :: bnk_bd  = 0.     ! (g/cc)        |bulk density of channel bank sediment (1.1-1.9)
        real :: bed_bd  = 0.     ! (g/cc)        |bulk density of channel bed sediment (1.1-1.9)
        real :: bnk_kd  = 0.     !               |erodibility of channel bank sediment by jet test
        real :: bed_kd  = 0.     !               |erodibility of channel bed sediment by jet test
        real :: bnk_d50  = 0.    !               |D50(median) particle size diameter of channel 
        real :: bed_d50  = 0.    !               |D50(median) particle size diameter of channel
        real :: tc_bnk  = 0.     ! N/m2          |critical shear stress of channel bank
        real :: tc_bed  = 0.     ! N/m2          |critical shear stress of channel bed 
        real, dimension(12) :: erod  = 0.  !     |value of 0.0 indicates a non-erosive channel while a value
                                                     !of 1.0 indicates no resistance to erosion
      end type channel_sed_data
      type (channel_sed_data), dimension(:), allocatable :: ch_sed
            
      type channel_nut_data
        character(len=13) :: name
        real :: onco = 0.        ! ppm           |channel organic n concentration
        real :: opco = 0.        ! ppm           |channel organic p concentration
        real :: rs1 = 1.          ! m/day or m/hr   |local algal settling rate in reach at 20 deg C
        real :: rs2 = .05         ! (mg disP-P)/    |benthos source rate for dissolved phos ((m**2)*day)|in reach at 20 deg C
        !                                              or (mg disP-P)/((m**2)*hr)|
        real :: rs3 = .5          ! (mg NH4-N)/     |benthos source rate for ammonia nit in ((m**2)*day)|reach at 20 deg C
        !                                              or (mg NH4-N)/((m**2)*hr)|
        real :: rs4 = .05         ! 1/day or 1/hr   |rate coeff for organic nitrogen settling in reach at 20 deg C
        real :: rs5 = .05         ! 1/day or 1/hr   |org phos settling rate in reach at 20 deg C
        real :: rs6 = 2.5         ! 1/day           |rate coeff for settling of arbitrary non-conservative constituent in reach
        real :: rs7 = 2.5         ! (mg ANC)/       |benthal source rate for arbitrary ((m**2)*day)|non-conservative constituent in reach
        real :: rk1 = 1.71        ! 1/day or 1/hr   |CBOD deoxygenation rate coeff in reach at 20 deg C
        real :: rk2 = 1.          ! 1/day or 1/hr   |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
        real :: rk3 = 2.          ! 1/day or 1/hr   |rate of loss of CBOD due to settling in reach at 20 deg C
        real :: rk4 = 0.          ! mg O2/          |sed oxygen demand rate in reach ((m**2)*day)|at 20 deg C or mg O2/((m**2)*hr)
        real :: rk5 = 1.71        ! 1/day           |coliform die-off rate in reach
        real :: rk6 = 1.71        ! 1/day           |decay rate for arbitrary non-conservative constituent in reach
        real :: bc1 = .55         ! 1/hr            |rate constant for biological oxidation of NH3 to NO2 in reach at 20 deg C
        real :: bc2 = 1.1         ! 1/hr            |rate constant for biological oxidation of NO2 to NO3 in reach at 20 deg C
        real :: bc3 = .21         ! 1/hr            |rate constant for hydrolysis of organic N to ammonia in reach at 20 deg C
        real :: bc4 = .35         ! 1/hr            |rate constant for the decay of organic P to dissolved P in reach at 20 deg C
        real :: lao  = 2          ! NA              |Qual2E light averaging option. Qual2E defines four light averaging options. The only option
                                                    !currently available in SWAT is #2.
        integer :: igropt = 2     ! none            |Qual2E option for calculating the local specific growth rate of algae
                                                    ! 1: multiplicative: u = mumax * fll * fnn * fpp
                                                    ! 2: limiting nutrient: u = mumax * fll * Min(fnn, fpp)
                                                    ! 3: harmonic mean: u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
        real :: ai0 = 50.         ! ug chla/mg alg  |ratio of chlorophyll-a to algal biomass
        real :: ai1 = 0.08        ! mg N/mg alg     |fraction of algal biomass that is nitrogen
        real :: ai2 = 0.015       ! mg P/mg alg     |fraction of algal biomass that is phosphorus
        real :: ai3 = 1.60        ! mg O2/mg alg    |the rate of oxygen production per unit of algal photosynthesis
        real :: ai4 = 2.0         ! mg O2/mg alg    |the rate of oxygen uptake per unit of algae respiration
        real :: ai5 = 3.5         ! mg O2/mg N      |the rate of oxygen uptake per unit of NH3 nitrogen oxidation
        real :: ai6 = 1.07        ! mg O2/mg N      |the rate of oxygen uptake per unit of NO2 nitrogen oxidation
        real :: mumax = 2.0       ! 1/hr            |maximum specific algal growth rate at 20 deg C
        real :: rhoq = 2.5        ! 1/day or 1/hr   |algal respiration rate
        real :: tfact = 0.3       ! none            |fraction of solar radiation computed in the temperature heat balance that is 
                                                    ! photosynthetically active
        real :: k_l = 0.75        ! MJ/(m2*hr)      |half-saturation coefficient for light
        real :: k_n = 0.02        ! mg N/L          |michaelis-menton half-saturation constant for nitrogen
        real :: k_p = 0.025       ! mg P/L          |michaelis-menton half saturation constant for phosphorus
        real :: lambda0 = 1.0     ! 1/m             |non-algal portion of the light extinction coefficient
        real :: lambda1 = 0.03    ! 1/(m*ug chla/L) |linear algal self-shading coefficient
        real :: lambda2 = 0.054   ! (1/m)(ug chla/L)**(-2/3) |nonlinear algal self-shading coefficient
        real :: p_n = 0.5         ! none            |algal preference factor for ammonia
      end type channel_nut_data
      type (channel_nut_data), dimension(:), allocatable :: ch_nut
          
      type channel_pst_data
        character(len=13) :: name
        real :: pst_rea = .007    ! 1/day           |pesticide reaction coeff in reach
        real :: pst_vol = .01     ! m/day           |pesticide volatilization coeff in reach
        real :: pst_koc = 0.      ! m**3/g          |pesticide partition coeff between water and sediment in reach
        real :: pst_mix = .001    ! m/day           |mixing velocity (diffusion/dispersion) for pesticide in reach
        real :: pst_rsp = .002    ! m/day           |resuspension velocity in reach for pesticide sorbed to sediment 
        real :: pst_stl = 1.      ! m/day           |settling velocity in reach for pesticide sorbed to sediment
        real :: sedpst_act = .03  ! m               |depth of active sediment layer in reach for pesticide
        real :: sedpst_bry = .002 ! m/day           |pesticide burial velocity in river bed sediment
        real :: sedpst_conc = 0.  ! mg/(m**3)       |inital pesticide concentration in river bed sediment
        real :: sedpst_rea = .05  ! 1/day           |pesticide reaction coeff in river bed sediment  
      end type channel_pst_data
      type (channel_pst_data), dimension(:), allocatable :: ch_pst
       
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

    contains

      include 'bac_lsparms_read.f90'
      include 'fertparm_read.f90'
      include 'pestparm_read.f90'
      include 'plantparm_read.f90'
      include 'plantparm_init.f90'
      include 'tillparm_read.f90'
      include 'urbanparm_read.f90'
      include 'septicparm_read.f90'
      include 'mgt_mgtops_read.f90'
      include 'mgt_irrops_read.f90'
      include 'mgt_chemapp_read.f90'
      include 'mgt_harvops_read.f90'
      include 'mgt_grazeops_read.f90'
      include 'mgt_sweepops_read.f90'
      include 'scen_filtstrip_read.f90'
      include 'mgt_fireops_read.f90'
      include 'scen_grwway_read.f90'
      include 'scen_bmpuser_read.f90'
      include 'sep_read.f90'
      include 'solt_db_read.f90'
      include 'topo_read.f90'
      include 'field_read.f90'
      include 'hydrol_read.f90'
      include 'landuse_read.f90'
      include 'cntbl_read.f90'
      include 'cons_prac_read.f90'
      include 'sdr_read.f90'
!      include 'potdb_read.f'
      include 'snowdb_read.f90'
      include 'soil_db_read.f90'
      include 'cli_atmodep_read.f90'
      include 'res_init_read.f90'
     ! include 'res_read.f90'
      include 'res_hyd_read.f90'
      include 'res_sed_read.f90'
      include 'res_nut_read.f90'
      include 'res_pst_read.f90'
      include 'res_weir_read.f90'
      include 'ch_hyd_read.f90'
      include 'ch_sed_read.f90'
      include 'ch_nut_read.f90'
      include 'ch_pst_read.f90'
      include 'rte_read_nut.f90'
      include 'exco_read.f90'
      
      end module jrw_datalib_module 