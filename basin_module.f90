      module basin_module
      
      use output_landscape_module
      use time_module
      
      integer :: mchg_par, ichg_par, ispu
      real :: ubw, uobw, uobn, uobp

      type basin_inputs
        character(len=13) :: name
        real :: area_ha = 0.
        real :: km2 = 0. 
      end type basin_inputs
      type (basin_inputs) :: bsn
      
      type basin_control_codes
        !character(len=16) :: update     !! pointer to basin updates in schedule.upd
        character(len=16) :: petfile     !! potential et filename
        character(len=16) :: wwqfile     !! watershed stream water quality filename
        integer :: pet = 0       !! potential ET method code
                                 !!   0 = Priestley-Taylor 
                                 !!   1 = Penman-Monteith
                                 !!   2 = Hargreaves method
                                 !!   3 = read in daily pot ET values
        integer :: event = 0     !! event code
        integer :: crk = 0       !! crack flow code 
                                 !!   1 = compute flow in cracks
        integer :: subwq = 0     !! subbasin water quality code
                                 !!   0 = do not calc algae/CBOD
                                 !!   1 = calc algae/CBOD
        integer :: sed_det = 0   !! max half-hour rainfall frac calc
                                 !!   0 = gen from triangular dist
                                 !!   1 = use monthly mean frac
        integer :: rte = 0       !! water routing method
                                 !!   0 variable storage method
                                 !!   1 Muskingum method
        integer :: deg = 0       !! channel degradation code
                                 !!   0 = do not compute
                                 !!   1 = compute (downcutting and widening)
        integer :: wq = 0        !! stream water quality code
                                 !!   0 do not model
                                 !!   1 model (QUAL2E)
        integer :: rtpest = 0    !! redeinfed to the sequence number  -- changed to no nutrient stress
                                 !!   of pest in NPNO(:) to be routed
                                 !!   through the watershed
        integer :: cn = 0        !! CN method flag
                                 !!   0 = use traditional SWAT method bases CN 
                                 !!   CN on soil moisture
                                 !!   1 = use alternative bases CN on plant ET
                                 !!   2 = use traditional SWAT mathod bases CN on 
                                 !!   soil moisture but retention is adjusted for 
                                 !!   mildly-sloped tiled-drained watersheds
        integer :: cfac = 0      !!  0 = C-factor calc using CMIN
                                 !!  1 = for new C-factor from RUSLE (no min needed)      
        integer :: cswat = 0     !! carbon code
                                 !!  = 0 Static soil carbon (old mineralization routines)
                                 !!  = 1 C-FARM one carbon pool model 
                                 !!  = 2 Century model
        integer :: bf_flg = 0    !! baseflow distribution factor during the day for subdaily runs
                                 !!   0 = baseflow evenly distributed to each time step during the day
                                 !!   0.5 = even weights between even distribution and rainfall pattern
                                 !!   1 = profile of baseflow in a day follows rainfall pattern
        integer :: uhyd = 1      !! Unit hydrograph method: 
                                 !!   1 = triangular UH
                                 !!   2 = gamma function UH
        integer :: sed_ch = 0    !! Instream sediment model
                                 !!   0 = Bagnold model
                                 !!   1 = Brownlie model
                                 !!   2 = Yang model
        integer :: tdrn = 0      !! tile drainage eq code
                                 !!   1 = sim tile flow using sub drains (wt_shall)
                                 !!   0 = sim tile flow using sub origtile (wt_shall,d)
        integer :: wtdn = 0      !! water table depth algorithms code
                                 !!   1 = sim wt_shall using sub new water table depth routine
                                 !!   0 = sim wt_shall using sub orig water table depth routine
        integer :: sol_p_model=0 !! 1 = new soil P model
        integer :: abstr = 0     !! Initial abstraction on impervious cover (mm) 
        integer :: atmo = 0      !! atmospheric deposition code
                                 !!   1 = average annual 
                                 !!   2 = monthly
        integer :: smax = 0      !! max depressional storage selection code
                                 !!   1 = dynamic stmaxd computed as a cunfction of random
                                 !!          roughness and rain intensity
                                 !!   0 = static stmaxd read from .bsn for the global value or .sdr
                                 !! for specific hrus 
        integer :: i_subhw = 0   !! headwater code (0=do not route; 1=route) 
      end type basin_control_codes
      type (basin_control_codes) :: bsn_cc

      type basin_parms
        real :: evlai = 3.0         !! none          |leaf area index at which no evap occurs
        real :: ffcb = 0.           !! none          |initial soil water cont expressed as a fraction of fc 
        real :: surlag = 4.0        !! days          |surface runoff lag time (days)
        real :: adj_pkr = 1.0       !! none          |peak rate adjustment factor in the subbasin
        real :: prf = 1.0           !! peak rate adjustment factor for sediment routing in the channel
        real :: spcon = 0.0001      !! linear parm for calc sed reentrained in channel sed routing
        real :: spexp = 1.0         !! exponent parameter for calc sed reentrained in channel sed routing
        real :: cmn = 0.0003        !! rate factor for mineralization on active org N
        real :: n_updis = 20.0      !! nitrogen uptake dist parm
        real :: p_updis = 20.0      !! phosphorus uptake dist parm
        real :: nperco = 20.0       !! nitrate perc coeff (0-1)
                                    !!   0 = conc of nitrate in surface runoff is zero
                                    !!   1 = perc has same conc of nitrate as surf runoff
        real :: pperco = 10.0       !! phos perc coeff (0-1)
                                    !!  0 = conc of sol P in surf runoff is zero
                                    !!  1 = percolate has some conc of sol P as surf runoff      
        real :: phoskd = 175.0      !! phos soil partitioning coef
        real :: psp = 0.40          !! phos availability index
        real :: rsdco = 0.05        !! residue decomposition coeff
        real :: percop = 0.5        !! pestcide perc coeff (0-1)
        real :: msk_co1 = 0.75      !! calibration coeff to control impact of the storage
                                    !!  time constant for the reach at bankfull depth
        real :: msk_co2 = 0.25      !! calibration coefficient used to control impact of the 
                                    !!   storage time constant for low flow (where low flow is when
                                    !!   river is at 0.1 bankfull depth) upon the Km value calculated
                                    !!   for the reach
        real :: msk_x = 0.20        !! weighting factor control relative importance of inflow rate 
                                    !!  and outflow rate in determining storage on reach
        real :: trnsrch             !! fraction of transmission losses from main channel that enter
                                    !!  deep aquifer
        real :: evrch = 0.60        !! reach evaporation adjustment factor
        real :: cncoef = 1.0        !! plant ET curve number coefficient
        real :: cdn = 1.40          !! denitrification expoential rate coefficient        
        real :: sdnco = 1.30        !! denitrification threshold frac of field cap
        real :: bact_swf = 0.15     !! frac of manure containing active colony forming units
        real :: tb_adj = 0.         !! adjustment factor for subdaily unit hydrograph basetime
        real :: cn_froz = 0.000862  !! parameter for frozen soil adjustment on infiltraion/runoff
        real :: dorm_hr = -1.       !! time threshold used to define dormant (hrs)
        real :: smxco = 1.0         !! adjustment factor for maximum curve number S factor.  
                                    !!  coeffcient curve number method that uses antecedent climate
        real :: fixco = 0.50        !! nitrogen fixation coeff
        real :: nfixmx = 20.0       !! max daily n-fixation (kg/ha)
        real :: decr_min = 0.01     !! minimum daily residue decay
        real :: rsd_covco = 0.30    !! residue cover factor for computing frac of cover         
        real :: vcrit = 0.          !! critical velocity
        real :: res_stlr_co = 0.184 !! reservoir sediment settling coeff
        real :: uhalpha = 1.0       !! alpha coeff for est unit hydrograph using gamma func
        real :: eros_spl = 0.       !! coeff of splash erosion varing 0.9-3.1 
        real :: rill_mult = 0.      !! rill erosion coefficient
        real :: eros_expo = 0.      !! exponential coeffcient for overland flow
        real :: c_factor = 0.       !! scaling parameter for cover and management factor for 
                                    !!  overland flow erosion
        real :: ch_d50 = 0.         !! median particle diameter of main channel (mm)
        real :: sig_g = 0.          !! geometric std dev of part sizes for the main channel
        real :: r2adj = 0.          !! curve number retention parameter adjustment for low gradient
                                    !!  non-draining soils
        integer :: igen = 0         !!  random generator code: 
                                    !!   0 = use default numbers
                                    !!   1 = generate new numbers in every simulation 
        integer :: rdist = 0        !!  rainfall ditribution code 0=skewed 1=exponential
        integer :: rexp = 0         !!  value of exponent for mixed exponential rainfall dist
      end type basin_parms
      type (basin_parms) :: bsn_prm

      type basin_print_codes
          !!  0 = average annual (always print)
          !!  1 = yearly
          !!  2 = monthly
          !!  3 = daily                            
        integer :: nyskip = 0         !!  number of years to skip output summarization
        integer :: jd_start = 0       !!  julian day to start printing output
        integer :: jd_end = 0         !!  julian day to end printing output
        integer :: yr_start = 0       !!  calendar year to start printing output
        integer :: yr_end = 0         !!  calendar year to end printing output
        integer :: interval = 1       !!  interval between daily printing within period
        ! HRU
        integer :: wb_hru = 0         !!  water balance HRU output
        integer :: nb_hru = 0         !!  nutrient balance HRU output
        integer :: ls_hru = 0         !!  losses HRU output
        integer :: pw_hru = 0         !!  plant weather HRU output
        ! SWAT-DEG
        integer :: wb_sd = 0          !!  water balance SWAT-DEG output 
        integer :: nb_sd = 0          !!  nutrient balance SWAT-DEG output
        integer :: ls_sd = 0          !!  losses SWAT-DEG output
        integer :: pw_sd = 0          !!  plant weather SWAT-DEG output
        ! SUBBASIN
        integer :: wb_sub = 0         !!  water balance SUBBASIN output
        integer :: nb_sub = 0         !!  nutrient balance SUBBASIN output
        integer :: ls_sub = 0         !!  losses SUBBASIN output
        integer :: pw_sub = 0         !!  plant weather SUBBASIN output
        ! BASIN
        integer :: wb_bsn = 0         !!  water balance BASIN output
        integer :: nb_bsn = 0         !!  nutrient balance BASIN output
        integer :: ls_bsn = 0         !!  losses BASIN output
        integer :: pw_bsn = 0         !!  plant weather BASIN output
        ! CHANNEL
        integer :: chan = 0           !!  channel output 
        ! AQUIFER
        integer :: aqu = 0            !!  aqufier output
        ! RESERVOIR
        integer :: res = 0            !!  reservoir output
        ! HYDIN AND HYDOUT
        integer :: hyd = 0            !!  hydin_output and hydout_output
        ! HYD CONNECT OUTPUT
        integer :: hydcon = 0         !!  hydrograph connect output file (hydcon.out)
        integer :: solout = 0         !!  soils output file (soils.out)
        integer :: mgtout = 0         !!  management output file (mgt.out)
        integer :: csvout = 0         !!  code to print .csv files 0=no print; 1=print;
        integer :: fdcout = 0         !!  flow duration curve output 0=no print; 1=print;
      end type basin_print_codes
      type (basin_print_codes) :: pco
           
      contains
      include 'basin_cc_read.f90'
      include 'basin_prm_read.f90'
      include 'basin_prm_default.f90'
      include 'basin_print_codes_read.f90'
      include 'basin_output.f90'

      end module basin_module