      module change_module
      
      use basin_module
      use jrw_datalib_module
      use parm
      use channel_module
      use reservoir_module
      use aquifer_module

      type change_parms
        character(len=13) :: name = 'default'
        integer :: day = 0
        integer :: year = 0
        character(len=15) :: parm_name
        real :: val               !!value of change
        character(len=10) :: typ  !!type of change (absvsl,abschg,pctchg)
        real :: absmin            !! minimum range for variable
        real :: absmax            !! maximum range for variable
        integer :: num_tot = 0    !! total number
        integer, dimension(:), allocatable :: num
      end type change_parms
      type (change_parms), dimension(:), allocatable :: chg_prm
      type (change_parms), dimension(:), allocatable :: chg_scen
      type (change_parms) :: chg
           
      contains
      include 'current_par_value.f90'
      include 'change_par_read.f90'
      include 'change_scen_read.f90'
      include 'change_scenario.f90'
      include 'chg_par.f90'

      end module change_module