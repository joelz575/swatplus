      module jrw_datalib_module
      
      integer :: msoils, mpl
      
      type data_files_max_elements
        integer :: topo = 0           !!nubz
        integer :: hyd = 0            !!nubz
        integer :: soil = 0           !! none     |number of types of soils 
        integer :: landuse = 0        !! none     |number of landuse types
        integer :: mgt_ops = 0        !! none     |number of records in management
        integer :: pothole = 0        !! none     |number of potholes
        integer :: sdr = 0            !! none     |number of types of susbsurface drain systems
        integer :: str_ops = 0        !! none     |number of management ops
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
        integer :: autofertop_db = 0  !! none     |auto fertilization operations
        integer :: airrop_db = 0      !! none     |auto irrigation operations
        integer :: contfertop_db = 0  !! none     |continuous fertilization operations
        integer :: contpestop_db = 0  !! none     |continuous pesticide operations
        integer :: fertop_db = 0      !! none     |fertlizer operations
        integer :: pstop_db = 0
        integer :: grazeop_db = 0     !! none     |grazing operations
        integer :: hkop_db = 0        !! none     |harvest kill operations
        integer :: harvop_db = 0      !! none     |harvest only operations
        integer :: irrop_db = 0       !! none     |irrigation operations
        integer :: sweepop_db = 0     !! none     |sweep operations
        integer :: terrop_db = 0      !! none     |terrace data
        integer :: contop_db = 0      !! none     |contour data
        integer :: filtop_db = 0      !! none     |filter strip data
        integer :: stripop_db = 0     !! none     |strip cropping data
        integer :: fireop_db = 0      !! none     |fire data
        integer :: grassop_db = 0     !! none     |grassed waterways data
        integer :: plparmop_db = 0    !! none     |plant parms update data
        integer :: rsdmgtop_db = 0    !! none     |residue mangement data
        integer :: bmpuserop_db = 0   !! none     |user defined upland CP removal
        integer :: cond = 0           !! none     |conditional data
        integer :: initop_db = 0      !! none     |initial.str
        integer :: wgnsta = 0         !! none     |max wgn stations included in weather-wgn.cli
        integer :: pcpfiles = 0       !! none     |max pcp files included in pcp.cli
        integer :: tmpfiles = 0       !! none     |max tmp files included in tmp.cli
        integer :: rhfiles = 0        !! none     |max relative humidity files included in hmd.cli
        integer :: slrfiles = 0       !! none     |max solar radiation files included in slr.cli
        integer :: wndfiles = 0       !! none     |max wind files included in the wnd.cli
        integer :: toposub = 0        !! none     |max in toposub.top file
        integer :: cal_parms = 0      !! none     |max number of calibration parameters in cal_parms_upd
        integer :: cal_upd = 0        !! none     |max number of calibration parameter updates
        integer :: updates = 0        !! none     |max number of basin updates (paramters, structures, land_use_mgt)
        integer :: wr_ob = 0          !! none     |max number of water rights objects
        integer :: irr_nosrc = 0      !! none     |max number of hru's with unlimited water source for irrigation
        integer :: d_tbl = 0          !! none     |max number of decision tables
        integer :: cs_db = 0
        integer :: pathcom = 0
        integer :: hmetcom = 0
        integer :: saltcom = 0
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
        character(len=8) :: fertnm = ' '
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
        character(len=4) :: urbnm
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
        integer :: opt = 0      !!none             |Septic system operation flag (1=active,2=failing,0=not operated)                                isep_opt
        real :: cap  = 0.       !!none             |Number of permanent residents in the house                                                          sep_cap
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
        real :: amt_mm = 0.      !! mm     |amount of water to be applied
        real :: salt = 0.        !! mg/kg  |concentration of salt in irrigation
        real :: no3 = 0.         !! mg/kg  |concentration of nitrate in irrigation
        real :: po4 = 0.         !! mg/kg  |concentration of phosphate in irrigation
        real :: eff = 0.         !!        |irrigation in-field efficiency
        real :: surq = 0.        !! frac   |surface runoff ratio
      end type irrigation_operation
      type (irrigation_operation), dimension(:), allocatable :: irrop_db
      type (irrigation_operation) :: irrop
      
      type autoirrigation_operation
        character (len=13) :: name
        integer :: src = 0       !!        |irrigation source code: 
!!                                         | 1 divert water from reach
!!                                         | 2 divert water from reservoir
!!                                         | 3 divert water from shallow aquifer
!!                                         | 4 divert water from deep aquifer
!!                                         | 5 divert water from source outside watershed
        integer :: src_num = 0   !!        |irrigation source location: 
!!                                         | if IRRSC=1, IRRNO is the number of the reach
!!                                         | if IRRSC=2, IRRNO is the number of the reservoir
!!                                         | if IRRSC=3, IRRNO is the number of the subbasin
!!                                         | if IRRSC=4, IRRNO is the number of the subbasin
!!                                         | if IRRSC=5, not used
        integer :: wstr_id = 0   !!        |water stress identifier
        real :: wstr_trig = 0.   !! mm     |water stress factor which triggers auto irrigation
        real :: amt_mm = 0.      !! mm     |amount of water to be applied
        real :: salt = 0.        !! mg/kg  |concentration of salt in irrigation
        real :: eff = 0.         !!        |mixing efficiency
        real :: surq = 0.        !! frac   |surface runoff ratio
      end type autoirrigation_operation
      type (autoirrigation_operation), dimension(:), allocatable :: airrop_db
      type (autoirrigation_operation) :: airrop
            
      type terrace_operation
        character (len=13) :: name
        real :: p = 0.            !       |usle p factor
        real :: cn2 = 0.          !       |condition II curve number
        real :: sl_len            !m      !slope length
      end type terrace_operation
      type (terrace_operation),dimension(:), allocatable :: terrace_db
      
      type contour_operation
        character (len=13) :: name
        real :: cont_cn = 0.            !       |initial SCS curve number II value
        real :: cont_p = 0.             !       |contouring USLE P factor
      end type contour_operation
      type (contour_operation),dimension(:), allocatable :: contour_db
      type (contour_operation) :: contour
      
      type filtstrip_operation
        character (len=13) :: name
        real :: vfsi = 0.               !       |initial SCS curve number II value
        real :: vfsratio = 0.           !       |contouring USLE P factor
        real :: vfscon                  !       |fraction of the total runoff from the entire field
        real :: vfsch                   !       |fraction of flow entering the most concentrated 10% of the VFS.
                                        !          which is fully channelized
      end type filtstrip_operation
      type (filtstrip_operation), dimension(:), allocatable :: filtstrip_db
      
      type stripcrop_operation
        character (len=13) :: name
        real :: strip_n = 0.               !       |initial SCS curve number II value
        real :: strip_cn = 0.              !       |contouring USLE P factor
        real :: strip_c = 0.               !       |fraction of the total runoff from the entire field
        real :: strip_p = 0.               !       |fraction of flow entering the most concentrated 10% of the VFS.
      end type stripcrop_operation
      type (stripcrop_operation), dimension(:), allocatable :: stripcrop_db
      
      type fire_operation
        character (len=13) :: name
        real :: fire_cn = 0.            !       |initial SCS curve number II value
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

      type plparmup_operation
        character (len=13) :: name
        integer :: plant_no            !       |plant number from plants.plt
        real :: hvsti = 0.             !       |harvest index crop yield/aboveground   
        real :: blai = 0.              !       |max (potential) leaf area index   
      end type plparmup_operation
      type (plparmup_operation),dimension(:), allocatable :: plparmup_db
      
      type rsdmgt_operation
        character (len=13) :: name
        real :: min_res = 0.      !kg/ha      |min residue allowed due to implementation of residue mgt                                
      end type rsdmgt_operation
      type (rsdmgt_operation),dimension(:), allocatable :: rsdmgt_db
      
      type bmpuser_operation  
!!! never switched from array to data type in....needs to be cleaned up !!!!
        character (len=13) :: name
        integer :: bmp_flag = 0
        real :: bmp_sed = 0.         !%             | Sediment removal by BMP       
        real :: bmp_pp = 0.          !%             | Particulate P removal by BMP
        real :: bmp_sp = 0.          !%             | Soluble P removal by BMP
        real :: bmp_pn = 0.          !%             | Particulate N removal by BMP 
        real :: bmp_sn = 0.          !%             | Soluble N removal by BMP  
        real :: bmp_bac = 0.         !%             | Bacteria removal by BMP                 
      end type bmpuser_operation
      type (bmpuser_operation),dimension(:), allocatable :: bmpuser_db
      
      type fertilizer_operation
        character (len=13) :: name
        character (len=13) :: fertnm = ' '
        real :: amt_kgh = 0.          !kg/ha        |amt of fert app to soil particles
        real :: surface = 0.          !             |frac of fert applied to the top 10mm of soil
      end type fertilizer_operation
      type (fertilizer_operation),dimension(:), allocatable :: fertop_db
      type (fertilizer_operation) :: fertop
      
      type autofertilizer_operation
        character (len=13) :: name
        character (len=13) :: fertnm = ' '
        integer :: option = 1
        real :: amt_kgh = 150.        !kg/ha          |amt of fert applied in auto-fert 
        real :: surface = 0.2         !               |frac of fert applied to the top 10mm of soil
        real :: str_trig = 0.95       !               |stress factor which triggers auto fert
        integer :: plant_trig = 1     !               |plant in community to trigger fert
        real :: ann_mx = 22.5         !kg NO3-N/ha    |maximum NO3-N content allowed to be
                                      !               |applied in one year by auto-fertilization
        real :: eff = 1.3             !               |fert application efficiency
      end type autofertilizer_operation
      type (autofertilizer_operation), dimension(:), allocatable :: autofertop_db
      type (autofertilizer_operation) :: autofertop
      
      type continuousfertilizer_operation
        character (len=13) :: name
        character (len=13) :: fertnm = ' '
        integer :: days = 0          !days        |number of days continuous fertilization
        integer :: freq = 0          !days        |number of days between applications in contfert
        real :: amt_kgh = 0.         !kg/ha       |amt of fert applied in auto-fert 
      end type continuousfertilizer_operation
      type (continuousfertilizer_operation),dimension(:), allocatable :: contfertop_db
      type (continuousfertilizer_operation) :: contfertop
      
      type pesticide_operation
        character (len=13) :: name
        character (len=13) :: pestnm
        real :: amt_kgh = 0.         !kg/ha        |amount of pesticide in layer
        real :: eff = 0.             !             |mixing efficiency
      end type pesticide_operation
      type (pesticide_operation), dimension(:), allocatable ::pestop_db
      type (pesticide_operation) :: pestop
      
      type continuous_pesticide_operation
        character (len=13) :: name
        character (len=13) :: pestnm = ' '
        integer :: days = 0
        integer :: freq = 0
        real :: amt_kgh = 0.         !kg/ha        |amount of pesticide in layer
      end type continuous_pesticide_operation
      type (continuous_pesticide_operation),dimension(:), allocatable :: contpestop_db
      type (continuous_pesticide_operation) :: contpestop
  
      type harvest_operation
        character (len=13) :: name
        character (len=13) :: typ   !none              |grain;biomass;residue;tree;tuber
        real :: hi_ovr = 0.         !(kg/ha)/(kg/ha)   |harvest index target specified at harvest
        real :: eff = 0.            !none              |harvest efficiency: fraction of harvested 
!!                                                       yield that is removed from HRU; the remainder 
!!                                                       becomes residue on the soil surface
        real :: bm_min = 0          !kg/ha             |minimum biomass to allow harvest
      end type harvest_operation
      type (harvest_operation), dimension(:), allocatable :: harvop_db
      type (harvest_operation) :: harvop
      type (harvest_operation) :: hkop
      
      type grazing_operation
        character (len=13) :: name
        character (len=13) :: fertnm = ' '
        integer :: days = 0
        real :: eat = 0.              !!(kg/ha)/day      |dry weight of biomass removed 
                                      !!                    by grazing daily
        real :: tramp = 0.            !!(kg/ha)/day      |dry weight of biomass removed
                                      !!                    by trampling daily
        real :: manure = 0.           !!(kg/ha)/day      |dry weight of manure deposited
        real :: biomin = 0.           !!kg/ha            |minimum plant biomass for grazing
      end type grazing_operation
      type (grazing_operation), dimension(:), allocatable :: grazeop_db
      type (grazing_operation) :: grazeop
      
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
        character(len=10) :: op_char
        character (len=10) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
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
        character(len=16) :: cond
        character(len=10) :: op_char
        character (len=10) :: op_plant
        integer :: op1 = 0
        integer :: op2 = 0              !! |none          |plant number in community for hu scheduling
        real :: op3 = 0                 !! |none          |application amount (mm or kg/ha)
      end type management_ops
      type (management_ops) :: mgt
      type (management_ops) :: mgt1
      type (management_ops), dimension(1) :: mgt2

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
        integer :: num_tot = 0          !! total number
        integer, dimension(:), allocatable :: num
        integer :: num_cond
        type (calibration_conditions), dimension(:), allocatable :: cond  
      end type update_parameters

      type (update_parameters), dimension (:), allocatable :: cal_upd   !dimensioned to db_mx%cal_parms
      type (update_parameters) :: chg

      type update_schedule
        character(len=16) :: typ        !! type of update schedule (parameter, structure, land_use_mgt)
        integer :: num = 0              !! number of updates
        character(len=16) :: name       !! name of update schedule
        integer :: day = 0
        integer :: year = 0
        character(len=16) :: cond       !! points to ruleset in conditional.ctl for scheduling the update
        type (update_parameters), dimension (:), allocatable :: upd_prm
      end type update_schedule
      type (update_schedule), dimension (:), allocatable :: upd_sched

      type structural_practices
        character(len=13) :: name = 'default'
        integer :: num_pr                                 
        character(len=13), dimension(:), allocatable :: prac       !! terrace, tile, contour, filter, stripcrop
                                                           !! fire, grassww, plantup, resman, user_def, septic       
        character(len=13), dimension(:), allocatable :: prac_typ   !! points to appropriate data file 
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
      
      type management_schedule
        character(len=16) :: name
        integer :: num_ops = 0
        integer :: num_autos = 0
        type (management_ops), dimension (:), allocatable :: mgt_ops
        character(len=16), dimension (:), allocatable :: auto_typ
        character(len=16), dimension (:), allocatable :: auto_name
        integer, dimension (:), allocatable :: num_db
        integer :: irr
      end type management_schedule
      type (management_schedule), dimension (:), allocatable :: sched
      
      type soiltest_db
        character(len=13) :: name = 'default'
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
        character(len=13) :: name = 'default'
        real :: elev = 100.       !!               |m             |elevation of HRU
        real :: slope = .02       !!	hru_slp(:)   |m/m           |average slope steepness in HRU
        real :: slope_len = 50.   !! slsubbsn(:)   |m             |average slope length for erosion
        real :: lat_len = 50.     !! slsoil(:)     |m             |slope length for lateral subsurface flow
        real :: dis_stream = 100. !! dis_stream(:) |m             |average distance to stream
        real :: dep_co = 1.       !!               |              |deposition coefficient
      end type topography_db
      type (topography_db), dimension (:), allocatable :: topo_db
      
      type topography_sub_db
        character(len=13) :: name = 'default'
        real :: elev = 100.       !!               |m             |elevation of HRU
        real :: slope = .02       !!	hru_slp(:) |m/m           |average slope steepness in HRU
        real :: slope_len = 50.   !! slsubbsn(:)   |m             |average slope length for erosion
        real :: lat_len = 50.     !! slsoil(:)     |m             |slope length for lateral subsurface flow
        real :: dis_stream = 100. !! dis_stream(:) |m             |average distance to stream
        real :: dep_co = 1.       !!               |              |deposition coefficient
      end type topography_sub_db
      type (topography_sub_db), dimension (:), allocatable :: toposub_db
              
      type fields_db
           character(len=13) :: name = 'default'
           real :: length = 500. !!               |m             |field length for wind erosion
           real :: wid = 100.    !!               |m             |field width for wind erosion
           real :: ang = 30.     !!               |deg           |field angle for wind erosion
      end type fields_db
      type (fields_db), dimension (:), allocatable :: field_db
      
      type hydrology_db
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
         real :: lat_orgn = 0.   !!               |ppm           |organic N concentration in lateral flow
         real :: lat_orgp = 0.   !!               |ppm           |organic P concentration in lateral flow
         real :: harg_pet  = .0023  !!           |              |coefficient related to radiation used in 
                                     !!                            Hargreaves equation
         real :: cncoef = 0.3        !!           |              |plant ET curve number coefficient
       end type hydrology_db
        type (hydrology_db), dimension (:), allocatable :: hyd_db
        
      type landuse_db
          character(len=15) :: name = " "
          integer :: cn_lu = 1 
          real :: usle_p = 1.           !! none           USLE equation support practice (P) factor daily
          integer :: iurban = 0         !! none           urban simulation code:
                                        !!                 |0  no urban sections in HRU
                                        !!                 |1  urban sections in HRU, simulate using USGS regression eqs
                                        !!                 |2  urban sections in HRU, simulate using build up/wash off alg
          integer ::  urb_lu = 0        !! none           urban land type identification number
          real :: ovn = 0.1             !! none           Manning's "n" value for overland flow
      end type landuse_db
      type (landuse_db), dimension (:), allocatable :: luse_db
      
      type land_use_management
        character (len=16) :: name = " "
        character (len=16) :: plant_cov = ""
        character (len=16) :: mgt_ops = ""
        integer :: cn_lu = 1
        integer :: usle_p
        integer :: urb_lu = 0           !! none     urban land type identification number
        integer :: iurban = 0           !! none     urban simulation code 
                                        !!                 |0  no urban sections in HRU
                                        !!                 |1  urban sections in HRU, simulate using USGS regression eqs
                                        !!                 |2  urban sections in HRU, simulate using build up/wash off alg
        real :: ovn = 0.1                !! none    Manning's "n" value for overland flow
        character (len=25) :: tiledrain
        character (len=25) :: septic
        character (len=25) :: fstrip
        character (len=25) :: grassww
        character (len=25) :: terrace
        character (len=25) :: contour
        character (len=25) :: stcrop
        character (len=25) :: bmpuser
      end type land_use_management
      type (land_use_management), dimension (:), allocatable :: lum
      
      type land_use_structures
        integer :: plant_cov = 0
        integer :: mgt_ops = 0
        integer :: tiledrain = 0
        integer :: septic = 0
        integer :: fstrip = 0
        integer :: grassww = 0
        integer :: terrace = 0
        integer :: contour = 0
        integer :: stcrop = 0
        integer :: bmpuser = 0
      end type land_use_structures
      type (land_use_structures), dimension (:), allocatable :: lum_str
           
      type curvenumber_table
          character(len=80) :: lu = " "             !landuse
          character(len=40) :: treat = " "          !treatment
          character(len=13) :: hycon = " "          !condition of cover
          real, dimension(4) :: cn = (/30.,55.,70.,77./) !curve number
      end type curvenumber_table
      type (curvenumber_table), dimension (:), allocatable :: cn
      
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
         character (len=13) :: name
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
        character(len=1) :: hydgrp = "A"      !! NA            |hydrologic soil group
        real :: zmx = 1500.                   !! mm            |maximum rooting depth
        real :: anion_excl = 0.5              !! none          |fraction of porosity from which anions are excluded
        real :: crk = 0.01                    !! none          |crack volume potential of soil
        character(len=20) :: texture = " "    !!               |texture of soil
      end type soil_profile_db
      
      type soil_database
         type (soil_profile_db) :: s
         type (soilayer_db), dimension(:), allocatable :: ly
      end type soil_database
      type (soil_database), dimension(:), allocatable :: soildb
      
      type plant_db
        character(len=4) :: plantnm = 'FRSD ' !N/A    |4 letter char code represents crop name
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
        real :: bmx_trees = 1000.        !metric tons/ha    |max biomass for forest (trees only)
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
        real :: frsw_gro = 0.            !frac              |frac of field capacity to initiate growth of tropical 
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
      
      type pcom_crosswalk
        character (len=20) name    !! name of the community
        character (len=4), dimension(:), allocatable :: pl_name  !! name of plants in the community
      end type pcom_crosswalk
      type (pcom_crosswalk), dimension(:), allocatable :: pcom_xw
      
      character (len=20), dimension(:), allocatable :: plnt_xw 
      character (len=20), dimension(:), allocatable :: harv_xw        
      character (len=20), dimension(:), allocatable :: till_xw
      character (len=20), dimension(:), allocatable :: irrm_xw      
      character (len=20), dimension(:), allocatable :: fert_xw
      character (len=20), dimension(:), allocatable :: fertops_xw
      character (len=20), dimension(:), allocatable :: frta_xw  
      character (len=20), dimension(:), allocatable :: frtc_xw    
      character (len=20), dimension(:), allocatable :: pest_xw
      character (len=20), dimension(:), allocatable :: pestop_xw
      character (len=20), dimension(:), allocatable :: pstc_xw
      character (len=20), dimension(:), allocatable :: graz_xw   
      character (len=20), dimension(:), allocatable :: burn_xw   
      character (len=20), dimension(:), allocatable :: swep_xw
      
 !!  structural ops     
      character (len=20), dimension(:), allocatable :: sept_str_xw
      character (len=20), dimension(:), allocatable :: bmp_str_xw
      character (len=20), dimension(:), allocatable :: cont_str_xw
      character (len=20), dimension(:), allocatable :: fstrip_str_xw
      character (len=20), dimension(:), allocatable :: fire_str_xw
      character (len=20), dimension(:), allocatable :: grassww_str_xw
      character (len=20), dimension(:), allocatable :: plparms_str_xw
      character (len=20), dimension(:), allocatable :: rsdmgt_str_xw
      character (len=20), dimension(:), allocatable :: stripcr_str_xw
      character (len=20), dimension(:), allocatable :: terr_str_xw
      character (len=20), dimension(:), allocatable :: tdrain_str_xw
      character (len=20), dimension(:), allocatable :: init_str_xw
      
      character (len=20), dimension(:), allocatable :: mgt_ops_xw
      character (len=20), dimension(:), allocatable :: soil_nut_xw
      character (len=20), dimension(:), allocatable :: topo_xw
      character (len=20), dimension(:), allocatable :: toposub_xw
      character (len=20), dimension(:), allocatable :: hyd_xw
      character (len=20), dimension(:), allocatable :: soil_xw
      character (len=20), dimension(:), allocatable :: landuse_xw
      character (len=20), dimension(:), allocatable :: snow_xw
      character (len=20), dimension(:), allocatable :: field_xw
      
!!    constituents ops
      character (len=20), dimension(:), allocatable :: constit_xw
      character (len=20), dimension(:), allocatable :: pestcom_xw
      character (len=20), dimension(:), allocatable :: pathcom_xw
      character (len=20), dimension(:), allocatable :: hmetcom_xw
      character (len=20), dimension(:), allocatable :: saltcom_xw
    
      type routing_nut_data         ! used for 2-stage ditch in chandeg and overland flow
        character(len=16) :: name = 'Drainage_Ditch'
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
      end type routing_nut_data
      type (routing_nut_data), dimension(:), allocatable :: rte_nut
      
      type channel_initial
        character(len=13) :: name
        real :: vol               !m**3          |res vol (read in as frac principle and converted to m^3)
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
        real :: psol          !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: psor         !kg/L          |amt of pest in res (read in as mg/L and converted to kg/L)
        real :: bactlp            !# cfu/100ml   |less persistent bacteria stored in res
        real :: bactp             !# cfu/100ml   |persistent bacteria stored in res
      end type channel_initial
      type (channel_initial), dimension(:),allocatable :: ch_init

      type channel_data
        character(len=13) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
        integer :: pst = 0                    !pesticide inputs-points to pesticide.res
        integer :: ls_lnk = 0                 !landscape linkage-points to ch_ls_link?
        integer :: aqu_lnk = 0                !aquifer likage-points to ch_aqu_link
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
        character(len=13) :: name
        real :: vol = 0.          !m**3          |res vol (read in as frac principle and converted to m^3)
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

      type reservoir_data
        character(len=13) :: name = "default"
        integer :: init = 0                   !initial data-points to initial.res
        integer :: hyd = 0                    !points to hydrology.res for hydrology inputs
        integer :: release = 0                !0=simulated; 1=measured outflow
        integer :: sed = 0                    !sediment inputs-points to sediment.res
        integer :: nut = 0                    !nutrient inputs-points to nutrient.res
        integer :: pst = 0                    !pesticide inputs-points to pesticide.res
      end type reservoir_data
      type (reservoir_data), dimension(:), allocatable :: res_dat
      type (reservoir_data) :: res_datz
      
      type reservoir_hyd_data
        !variables are conditional on res_dat()%hyd = 0 for reservoirs and 1 for hru impounding
        !surface areas are ha for 0 and frac of hru for 1; volumes are ha-m for 0 and mm for 1
        !br1 and br2 are used for 0 and acoef for 0 -- for surface area - volume relationship
        character(len=13) :: name = "default"
        integer :: iyres = 0      !none          |year of the sim that the res becomes operational
        integer :: mores = 0      !none          |month the res becomes operational
        real :: psa = 0.          !ha or frac    |res surface area when res is filled to princ spillway
        real :: pvol = 0.         !ha-m or mm    |vol of water needed to fill the res to the princ spillway (read in as 10^4 m^3
                                  !                and converted to m^3)
        real :: esa = 0.          !ha or frac    |res surface area when res is filled to emerg spillway 
        real :: evol = 0.         !ha-m or mm    |vol of water needed to fill the res to the emerg spillway (read in as 10^4 m^3
                                  !                and converted to m^3)
        real :: k = .01           !mm/hr         |hydraulic conductivity of the res bottom
        real :: evrsv = .7        !none          |lake evap coeff
        real :: br1 = 0.          !none          |shape coefficient for reservoirs (model estimates if zero)
        real :: br2 = 0.          !none          |shape coefficient for reservoirs (model estimates if zero)
        real :: acoef = 1         !none          |vol-surface area coefficient for hru impoundment
        real :: frac = .5         !none          |fraction of hru that drains into impoundment
      end type reservoir_hyd_data
      type (reservoir_hyd_data), dimension(:), allocatable :: res_hyd
      type (reservoir_hyd_data) :: res_hydz
      
      type reservoir_sed_data
        character(len=13) :: name
        real :: nsed              !kg/L          |normal amt of sed in res (read in as mg/L and convert to kg/L)
        real :: d50               ! 
        real :: sed_stlr          !none          |sed settling rate
        real :: velsetlr
      end type reservoir_sed_data
      type (reservoir_sed_data), dimension(:), allocatable :: res_sed
            
      type reservoir_nut_data
        character(len=13) :: name
        integer :: ires1          !none          |beg of mid-year nutrient settling "season"
        integer :: ires2          !none          |end of mid-year nutrient settling "season"
        real :: nsetlr1               !m/day         |nit settling rate for mid-year period (read in as m/year and converted to m/day)
        real :: nsetlr2               !m/day         |nit settling rate for remainder of year (read in as m/year and converted to m/day)
        real :: psetlr1               !m/day         |phos settling rate for mid-year period (read in as m/year and converted to m/day)
        real :: psetlr2               !m/day         |phos settling rate for remainder of year (read in as m/year and converted to m/day)
        real :: chlar = 1.            !none          |chlorophyll-a production coeff for res
        real :: seccir = 1.0          !none          |water clarity coeff for res
      end type reservoir_nut_data
      type (reservoir_nut_data), dimension(:), allocatable :: res_nut
          
      type reservoir_pst_data
        character(len=13) :: name
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
        character(len=13) :: name
        real :: num_steps = 24        !              |number of time steps in day for weir routing
        real :: c = 1.                !              |weir discharge coefficient 
        real :: k = 150000.           !m^1/2 d^-1    |energy coefficient (broad_crested=147,000' sharp crested=153,000)
        real :: wid = 2.              !(m)           |width
        real :: bcoef = 1.75          !              |velocity exponent coefficient for bedding material
        real :: ccoef = 1.            !              |depth exponent coefficient for bedding material
      end type reservoir_weir_outflow
      type (reservoir_weir_outflow),dimension(:),allocatable :: res_weir

    contains

!      include 'data_files_read.f'
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
      include 'mgt_fertops_read.f90'
      include 'mgt_autofertops_read.f90'
      include 'mgt_contfertops_read.f90'
      include 'mgt_pestops_read.f90'
      include 'mgt_contpestops_read.f90'
      include 'mgt_harvops_read.f90'
      include 'mgt_grazeops_read.f90'
      include 'mgt_sweepops_read.f90'
      include 'str_init_read.f90'
      include 'scen_terrace_read.f90'
      include 'scen_contour_read.f90'
      include 'scen_filtstrip_read.f90'
      include 'scen_stripcrop_read.f90'
      include 'scen_fire_read.f90'
      include 'scen_grwway_read.f90'
      include 'scen_plparmup_read.f90'
      include 'scen_rsdmgt_read.f90'
      include 'scen_bmpuser_read.f90'
      include 'sep_read.f90'
      include 'solt_db_read.f90'
      include 'topo_read.f90'
      include 'toposub_read.f90'
      include 'field_read.f90'
      include 'hydrol_read.f90'
      include 'landuse_read.f90'
      include 'cntbl_read.f90'
      include 'sdr_read.f90'
!      include 'potdb_read.f'
      include 'snowdb_read.f90'
      include 'soil_db_read.f90'
      include 'atmoparm_read.f90'
      include 'res_hyd_read.f90'
      include 'res_sed_read.f90'
      include 'res_nut_read.f90'
      include 'res_pst_read.f90'
      include 'res_weir_read.f90'
      include 'ch_read_hyd.f90'
      include 'ch_read_sed.f90'
      include 'ch_read_nut.f90'
      include 'ch_read_pst.f90'
      include 'rte_read_nut.f90'
      
      end module jrw_datalib_module 