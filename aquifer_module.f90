      module aquifer_module

      use hydrograph_module
      use climate_parms

      integer :: iaq, iaqdb
      
      type aquifer_database
        character(len=16) :: aqunm = ""
        real :: flo = 0.05        !! mm H2O        |groundwater flow
        real :: stor = 0.         !! mm H2O        |depth of water in shallow aq
        real :: hgt = 0.          !! m             |groundwater height 
        real :: no3 = 0.          !! ppm NO3-N     |nitrate conc in shallow aq converted to kg/ha 
        real :: minp = 0.         !! mg P/L        |mineral P concentration
        real :: orgn = 0.         !! mg/L          |organic n in the base flow
        real :: orgp = 0.         !! mg/L          |organic p in the base flow
        real :: delay = 0.        !! days          |gw delay: time req for water leaving the bottom of the root zone to reach the shallow aq
        real :: alpha = 0.        !! 1/days        |alpha factor for gw recession curve   
        real :: revap = 0.        !! 0-1 frac      |fraction of pet to calculate revap
        real :: seep = 0.         !! none          |recharge to deep aq: the fraction of root zone perc that reaches the deep aq
        real :: spyld = 0.        !! m**3/m**3     |specific yield for shallow aq
        real :: hlife_n = 0.      !! days          |half-life of nitrate in the shallow aquifer
        real :: flo_min = 0.   
        real :: revap_min = 0.    !! mm H2O        |threshold depth of water in shallow aquifer required to allow revap to occur
      end type aquifer_database
      type (aquifer_database), dimension(:), allocatable :: aqudb
 
      type aquifer_data_parameters
        real :: delay_e = 0.   !days       |groundwater delay (time required for water leaving the
                                           !bottom of the root zone to reach shallow aquifer
        real :: alpha_e = 0.   !days       |Exp(-alpha_bf(:))
        real :: nloss = 0.     !frac       |nloss based on half life
      end type aquifer_data_parameters
      type (aquifer_data_parameters), dimension(:), allocatable :: aqu_prm 
                 
      type aquifer_state_parameters
        real :: rchrg_prev = 0.   !m^3        |previous days recharge
        real :: rchrgn_prev = 0.  !m^3        |previous days recharge
      end type aquifer_state_parameters
      type (aquifer_state_parameters), dimension(:), allocatable :: aqu_st 
             
      type aquifer_dynamic
        real :: flo = 0.       !m^3       |flow from aquifer in current time step       
        real :: stor = 0.      !          |water storage in aquifer 
        real :: hgt = 0.       !m         |groundwater height
        real :: no3 = 0.       !ppm NO3-N |nitrate-N concentration in aquifer
        real :: minp = 0.      !kg        |mineral phosphorus from aquifer on current timestep    
        real :: orgn = 0.  
        real :: orgp = 0.   
        real :: rchrg = 0.     !m^3       |recharge 
        real :: rchrg_n = 0.   !          |amount of nitrate getting to the shallow aquifer  
        real :: nloss = 0. 
        real :: no3gw          !kg N/ha   |nitrate loading to reach in groundwater
        real :: seep = 0.      !kg N/ha   |seepage to next object
        real :: revap = 0.     !m^3       |revap
        real :: seepno3 = 0.   !kg        |seepage of no3 to next object
        real :: flo_min        !mm        |minimum aquifer storage to allow return flow
        real :: revap_co       !0-1 frac  |fraction of pet to calculate revap
        real :: revap_min = 0. !mm H2O    |threshold depth of water in shallow aquifer required to allow revap to occur
      end type aquifer_dynamic
      type (aquifer_dynamic), dimension(:), allocatable :: aqu
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_m
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_y
      type (aquifer_dynamic), dimension(:), allocatable, save :: aqu_a
      type (aquifer_dynamic) :: aquz
      
      type aqu_header
          character (len=6) :: yrs =       ' time '
          character (len=6) :: yrc =       ' year '
          character (len=8) :: isd =       '   unit '
          character(len=15) :: flo =       '         flo_mm'          ! (mm)
          character(len=15) :: stor =      '        stor_mm'          ! (mm)
          character(len=15) :: hgt =       '       height_m'          ! (m)
          character(len=15) :: no3 =       '     no3_kgN/ha'          ! (kg/ha N)
          character(len=15) :: sed_in =    '    minp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: sed_out=    '    orgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: sed_conc =  '    orgp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: orgn_in =   '       rchrg_mm'          ! (mm)
          character(len=15) :: orgn_out =  '  rchrgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: orgp_in =   '        perc_mm'          ! (mm)
          character(len=15) :: orgp_out =  '   nloss_kgN/ha'          ! (kg/ha N)
          character(len=15) :: seep =      '     seepage mm'          ! (mm)
          character(len=15) :: revap =     '       revap_mm'          ! (mm)
          character(len=15) :: seep_no3 =  '   nseep_kgN/ha'          ! (kg/ha N)
      end type aqu_header
      type (aqu_header) :: aqu_hdr
      interface operator (+)
        module procedure aqu_add
      end interface
      
      interface operator (/)
        module procedure aqu_div
      end interface
        
      contains
        
      !! routines for shallow aquifer module
      include 'aqu_initial.f90'
      include 'aqu_1d_control.f90'
      include 'aquifer_output.f90'
      include 'aqu_read.f90'

      
      function aqu_add(aqo1,aqo2) result (aqo3)
      type (aquifer_dynamic),  intent (in) :: aqo1
      type (aquifer_dynamic),  intent (in) :: aqo2
      type (aquifer_dynamic) :: aqo3
       aqo3%flo = aqo1%flo + aqo2%flo
       aqo3%hgt = aqo1%hgt + aqo2%hgt
       aqo3%no3 = aqo1%no3 + aqo2%no3   
       aqo3%minp = aqo1%minp + aqo2%minp  
       aqo3%orgn = aqo1%orgn + aqo2%orgn
       aqo3%orgp = aqo1%orgp + aqo2%orgp
       aqo3%rchrg = aqo1%rchrg + aqo2%rchrg     
       aqo3%rchrg_n = aqo1%rchrg_n + aqo2%rchrg_n         
       aqo3%nloss = aqo1%nloss + aqo2%nloss
       aqo3%seep = aqo1%seep + aqo2%seep
       aqo3%revap = aqo1%revap + aqo2%revap           
       aqo3%seepno3 = aqo1%seepno3 + aqo2%seepno3
      end function aqu_add
      
      function aqu_div (aq1,const) result (aq2)
        type (aquifer_dynamic), intent (in) :: aq1
        real, intent (in) :: const
        type (aquifer_dynamic) :: aq2
        consta = time%nbyr
        aq2%flo = aq1%flo / consta
        aq2%hgt = aq1%hgt / consta
        aq2%no3 = aq1%no3 / consta  
        aq2%minp = aq1%minp / consta  
        aq2%orgn = aq1%orgn / consta 
        aq2%orgp = aq1%orgp / consta           
        aq2%rchrg = aq1%rchrg / consta            
        aq2%rchrg_n = aq1%rchrg_n / consta               
        aq2%nloss = aq1%nloss / consta
        aq2%seep = aq1%seep / consta  
        aq2%revap = aq1%revap / consta              
        aq2%seepno3 = aq1%seepno3 / consta
      end function aqu_div
        
      end module aquifer_module