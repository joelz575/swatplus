!------------------------------------------------------------------------------
!> aquifer components module
!!------------------------------------------------------------------------------
!!
!! @author Jeff Arnold
!! @version 0.0.0
!! @date 06 28 2016
!!
!! @brief
!! This module includes the aquifer database parameters
!!
!! @details
!! REVISION HISTORY:
!! 2016.24 test
!!
      module aquifer_module

      use hydrograph_module
      use climate_parms

      integer :: iaq, iaqdb
       
      !>@brief type for reading aquifer input files rows
      type aquifer_database
        character(len=16) :: aqunm = ""
        !>@brief [FLO]      groundwater flow [mm H2O]
        real :: flo = 0.05
        !>@brief [STOR]     depth of water in shallow aq [mm H2O]
        real :: stor = 0.
        !>@brief [HGT]      groundwater height [m]
        real :: hgt
        !>@brief [NO3]      nitrate conc in shallow aq converted to kg/ha [ppm NO3-N]
        real :: no3 = 0.           
        !>@brief [MINP]     mineral P concentration [mg P/L]
        real :: minp = 0.   
        !>@brief [ORGN]     organic N in the base flow [mg/L]
        real :: orgn = 0.         
        !>@brief [ORGP]     organic P in the base flow [mg/L]
        real :: orgp = 0.         
        !>@brief [DELAY]    gw delay: time req for water leaving the bottom of the root zone to reach the shallow aq [days]
        real :: delay = 0.
        !>@brief [ALPHA]    alpha factor for gw recession curve [1/days]
        real :: alpha = 0.        
        !>@brief [HGT]      fraction of pet to calculate revap [0-1 frac]
        real :: revap = 0.      
        !>@brief [SEEP]     recharge to deep aq: the fraction of root zone perc that reaches the deep aq [none]
        real :: seep = 0.
        !>@brief [SPYLD]    specific yield for shallow aq [m**3/m**3]
        real :: spyld = 0. 
        !>@brief [HLIFE_N]  half-life of nitrate in the shallow aquifer [days]
        real :: hlife_n = 0.
        !>@brief [FLO_MIN]  minimum aquifer storage to allow return flow [m]  
        real :: flo_min = 0. 
        !>@brief [REVAP_MIN]    threshold depth of water in shallow aquifer required to allow revap to occur [mm H2O]
        real :: revap_min = 0.      
      end type aquifer_database
      !> @brief read from the aquifer database file named aquifer.aqu
      !! @see aqu_read
      type (aquifer_database), dimension(:), allocatable :: aqudb 
      
      type aquifer_data_parameters
        real :: delay_e = 0.   !delay_e     days       |groundwater delay (time required for water leaving the
                                           !bottom of the root zone to reach shallow aquifer
        real :: alpha_e = 0.   !days       |Exp(-alpha_bf(:))
        real :: nloss = 0.     !frac       |nloss based on half life
      end type aquifer_data_parameters
      
      type (aquifer_data_parameters), dimension(:), allocatable :: aqu_prm 
                 
      type aquifer_state_parameters
        character(len=16) :: name
        integer :: props
        integer :: obj_no
        real :: flo_min        !mm        |minimum aquifer storage to allow return flow
        real :: revap_co       !0-1 frac  |fraction of pet to calculate revap
        real :: revap_min = 0. !mm H2O    |threshold depth of water in shallow aquifer required to allow revap to occur
        real :: rchrg_prev = 0.   !m^3        |previous days recharge
        real :: rchrgn_prev = 0.  !m^3        |previous days n recharge
      end type aquifer_state_parameters
      type (aquifer_state_parameters), dimension(:), allocatable :: aqu_st 
             
      type aquifer_dynamic
        real :: flo = 0.       !m^3       |flow from aquifer in current time step       
        real :: stor = 0.      !          |water storage in aquifer 
        real :: rchrg = 0.     !m^3       |recharge
        real :: seep = 0.      !kg N/ha   |seepage to next object
        real :: revap = 0.     !m^3       |revap
        real :: hgt = 0.       !m         |groundwater height
        real :: no3 = 0.       !ppm NO3-N |nitrate-N concentration in aquifer
        real :: minp = 0.      !kg        |mineral phosphorus from aquifer on current timestep    
        real :: orgn = 0.  
        real :: orgp = 0.   
        real :: rchrg_n = 0.   !          |amount of nitrate getting to the shallow aquifer  
        real :: nloss = 0. 
        real :: no3gw          !kg N/ha   |nitrate loading to reach in groundwater
        real :: seepno3 = 0.   !kg        |seepage of no3 to next object
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
          character(len=15) :: flo =       '        flo_m^3'          ! (^m3)
          character(len=15) :: stor =      '        stor_mm'          ! (mm)
          character(len=15) :: rchrg =     '      rchrg_m^3'          ! (m^3)
          character(len=15) :: seep =      '           seep'          ! (mm)
          character(len=15) :: revap =     '      revap_m^3'          ! (m^3)
          character(len=15) :: hgt =       '        hgt_m  '          ! (m)
          character(len=15) :: no3_st =    'no3_stor_kgN/ha'          ! (kg/ha N)
          character(len=15) :: minp =      '        minp_kg'          ! (kg)
          character(len=15) :: orgn =      '    orgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: orgp =      '    orgp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: rchrgn =    '  rchrgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: nloss =     '   nloss_kgN/ha'          ! (kg/ha N)
          character(len=15) :: no3gw =     '   no3gw_kgN/ha'          ! (kg N/ha)
          character(len=15) :: seep_no3 =  '     seepno3_kg'          ! (kg)
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
       aqo3%stor = aqo1%stor + aqo2%stor
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
       aqo3%no3gw = aqo1%no3gw + aqo2%no3gw
       aqo3%seepno3 = aqo1%seepno3 + aqo2%seepno3
      end function aqu_add
      
      function aqu_div (aq1,const) result (aq2)
        type (aquifer_dynamic), intent (in) :: aq1
        real, intent (in) :: const
        type (aquifer_dynamic) :: aq2
        consta = time%nbyr
        aq2%flo = aq1%flo / consta
        aq2%stor = aq1%stor / consta
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
        aq2%no3gw = aq1%no3gw / consta
        aq2%seepno3 = aq1%seepno3 / consta
      end function aqu_div
        
      end module aquifer_module