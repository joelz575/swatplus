      module conditional_module

      type conditions
        character(len=16) :: var            ! condition variable (ie volume, flow, sw, time, etc)
        character(len=16) :: ob             ! object variable (ie res, hru, canal, etc)
        integer :: ob_num                   ! object number
        character(len=16) :: lim_var        ! limit variable (ie evol, pvol, fc, ul, etc)
        character(len=2) :: lim_op          ! limit operator (*,+,-)
        real :: lim_const                   ! limit constant
      end type conditions
              
      type actions
        character(len=16) :: name           ! name of action
        character(len=16) :: typ            ! type of action (ie reservoir release, irrigate, fertilize, etc)
        character(len=4) :: option          ! action option - specific to type of action (ie for reservoir, option to
                                            ! input rate, days of drawdown, weir equation pointer, etc
        real :: const                       ! constant used for rate, days, etc
        character(len=16) :: file_pointer   ! pointer for option (ie weir equation pointer)
      end type actions
       
      type decision_table
        character (len=16) :: name                                      ! name of the decision table
        integer :: conds                                                ! number of conditions
        integer :: alts                                                 ! number of alternatives
        integer :: acts                                                 ! number of actions
        type (conditions), dimension(:), allocatable :: cond            ! conditions
        character(len=16), dimension(:,:), allocatable :: alt           ! condition alternatives
        type (actions), dimension(:), allocatable :: act                ! actions
        character(len=1), dimension(:,:), allocatable :: act_outcomes   ! action outcomes ('y' to perform action; 'n' to not perform)
        character(len=1), dimension(:), allocatable :: act_hit          ! 'y' if all condition alternatives (rules) are met; 'n' if not
        integer, dimension(:), allocatable :: act_ptr                   ! pointer to file character name (ie irr.ops)
      end type decision_table
      type (decision_table), dimension(:), allocatable :: d_tbl

      contains
      include 'condition_read.f90'
      
      end module conditional_module   