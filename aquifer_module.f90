      module aquifer_module
    
      implicit none

      real :: sumrchrg, sumflo, sumseep, sumrevap
       
      type aquifer_database
        character(len=16) :: aqunm = ""       !aquifer name
        real :: flo = 0.05         !mm        |flow from aquifer in current time step 
        real :: stor = 0.          !mm        |water storage in aquifer 
        real :: hgt                !m         |groundwater height
        real :: no3 = 0.           !ppm NO3-N |nitrate-N concentration in aquifer 
        real :: minp = 0.          !kg        |mineral phosphorus from aquifer on current timestep 
        real :: orgn = 0.          !(kg/ha N) |organic nitrogen 
        real :: orgp = 0.          !(kg/ha P) |organic phosphorus
        real :: delay = 0.         !          |
        real :: alpha = 0.         !          |
        real :: revap = 0.         !mm        |revap
        real :: seep = 0.          !kg N/ha   |seepage to next object
        real :: spyld = 0.         !          |
        real :: hlife_n = 0.       !          |
        real :: flo_min = 0.       !mm        |minimum aquifer storage to allow return flow
        real :: revap_min = 0.     !mm H2O    |threshold depth of water in shallow aquifer required to allow revap to occur 
      end type aquifer_database
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
        real :: flo = 0.       !mm        |flow from aquifer in current time step       
        real :: stor = 0.      !mm        |water storage in aquifer 
        real :: rchrg = 0.     !mm        |recharge
        real :: seep = 0.      !kg N/ha   |seepage to next object
        real :: revap = 0.     !mm        |revap
        real :: hgt = 0.       !m         |groundwater height
        real :: no3 = 0.       !ppm NO3-N |nitrate-N concentration in aquifer
        real :: minp = 0.      !kg        |mineral phosphorus from aquifer on current timestep    
        real :: orgn = 0.  
        real :: orgp = 0.   
        real :: rchrg_n = 0.   !          |amount of nitrate getting to the shallow aquifer  
        real :: nloss = 0. 
        real :: no3gw          !kg N/ha   |nitrate loading to reach in groundwater
        real :: seepno3 = 0.   !kg        |seepage of no3 to next object
        real :: flo_cha = 0.   !mm H2O    |surface runoff flowing into channels
        real :: flo_res = 0.   !mm H2O    |surface runoff flowing into reservoirs
        real :: flo_ls = 0.    !mm H2O    |surface runoff flowing into a landscape element
      end type aquifer_dynamic
      type (aquifer_dynamic), dimension(:), allocatable :: aqu
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_m
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_y
      type (aquifer_dynamic), dimension(:), allocatable :: aqu_a
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_d
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_m
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_y
      type (aquifer_dynamic), dimension(:), allocatable :: saqu_a
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_d
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_m
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_y
      !type (aquifer_dynamic), dimension(:), allocatable :: raqu_a
      type (aquifer_dynamic) :: baqu_d
      type (aquifer_dynamic) :: baqu_m
      type (aquifer_dynamic) :: baqu_y
      type (aquifer_dynamic) :: baqu_a
      type (aquifer_dynamic) :: aquz
      
      type aqu_header
          character (len=6) :: yrs =       ' time '
          character (len=6) :: yrc =       ' year '
          character (len=8) :: isd =       '   unit '
          character (len=8) :: id =         '     id '           
          character (len=16) :: name =     ' name              '          
          character(len=15) :: flo =       '         flo_mm'          ! (mm)
          character(len=15) :: stor =      '        stor_mm'          ! (mm)
          character(len=15) :: rchrg =     '       rchrg_mm'          ! (mm)
          character(len=15) :: seep =      '        seep_mm'          ! (mm)
          character(len=15) :: revap =     '       revap_mm'          ! (mm)
          character(len=15) :: hgt =       '         hgt_m '          ! (m)
          character(len=15) :: no3_st =    'no3_stor_kgN/ha'          ! (kg/ha N)
          character(len=15) :: minp =      '        minp_kg'          ! (kg)
          character(len=15) :: orgn =      '    orgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: orgp =      '    orgp_kgP/ha'          ! (kg/ha P)
          character(len=15) :: rchrgn =    '  rchrgn_kgN/ha'          ! (kg/ha N)
          character(len=15) :: nloss =     '   nloss_kgN/ha'          ! (kg/ha N)
          character(len=15) :: no3gw =     '   no3gw_kgN/ha'          ! (kg N/ha)
          character(len=15) :: seep_no3 =  '     seepno3_kg'          ! (kg)
          character(len=15) :: flo_cha =   '    flo_cha_m^3'          ! (m^3)
          character(len=15) :: flo_res =   '    flo_res_m^3'          ! (m^3)
          character(len=15) :: flo_ls =    '     flo_ls_m^3'          ! (m^3)
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
      !include 'aqu_initial.f90'
      !include 'aqu_1d_control.f90'
      !include 'aquifer_output.f90'
      !include 'aqu_read.f90'

     
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
       aqo3%flo_cha = aqo1%flo_cha + aqo2%flo_cha
       aqo3%flo_res = aqo1%flo_res + aqo2%flo_res
       aqo3%flo_ls = aqo1%flo_ls + aqo2%flo_ls
      end function aqu_add
      
      function aqu_div (aq1,const) result (aq2)
        type (aquifer_dynamic), intent (in) :: aq1
        real, intent (in) :: const
        type (aquifer_dynamic) :: aq2
        aq2%flo = aq1%flo / const
        aq2%stor = aq1%stor / const
        aq2%hgt = aq1%hgt / const
        aq2%no3 = aq1%no3 / const
        aq2%minp = aq1%minp / const
        aq2%orgn = aq1%orgn / const
        aq2%orgp = aq1%orgp / const
        aq2%rchrg = aq1%rchrg / const
        aq2%rchrg_n = aq1%rchrg_n / const
        aq2%nloss = aq1%nloss / const
        aq2%seep = aq1%seep / const
        aq2%revap = aq1%revap / const
        aq2%no3gw = aq1%no3gw / const
        aq2%seepno3 = aq1%seepno3 / const
        aq2%flo_cha = aq1%flo_cha / const
        aq2%flo_res = aq1%flo_res / const
        aq2%flo_ls = aq1%flo_ls / const
      end function aqu_div
        
      end module aquifer_module