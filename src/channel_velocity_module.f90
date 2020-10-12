      module channel_velocity_module
    
      implicit none

      type channel_velocity_parameters
          real :: area = 0.             !m^2        |cross sectional area of flow at bankfull depth
          real :: vel_bf = 0.           !m^3/s      |flow rate when reach is at bankful depth
          real :: wid_btm = 0.          !m          |bottom width of main channel
          real :: dep_bf = 0.           !m          |depth of water when reach is at bankfull depth
          real :: velav_bf = 0.         !m/s        |average velocity when reach is at bankfull depth
          real :: celerity_bf = 0.      !m/s        |wave celerity when reach is at bankfull depth
          real :: st_dis = 0.           !hr         |storage time constant for reach at bankfull depth
          real :: vel_1bf = 0.          !m/s        |average velocity when reach is at 0.1 bankfull depth (low flow)
          real :: celerity_1bf = 0.     !m/s        |wave celerity when reach is at 0.1 bankfull depth (low flow)
          real :: stor_dis_1bf = 0.     !hr         |storage time constant for reach at 0.1 bankfull depth (low flow)
      end type channel_velocity_parameters
      type (channel_velocity_parameters), dimension(:), allocatable :: ch_vel
      type (channel_velocity_parameters), dimension(:), allocatable :: sd_ch_vel
      type (channel_velocity_parameters), dimension(:), allocatable :: grwway_vel
      
      type channel_rating_curve_parameters
        real :: area = 0.               !m^2        |cross sectional area of flow
        real :: flo_rate = 0.           !m^3/s      |flow rate
        real :: dep = 0.                !m          |depth of water
        real :: vol = 0.                !m^3        |volume of water in reach
        real :: perim_wet = 0.          !m          |wetted perimeter
        real :: celerity = 0.           !m/s        |wave celerity 
        real :: stor_dis = 0.           !hr         |storage time constant
        real :: ttime = 0.              !hr         |travel time
      end type channel_rating_curve_parameters
      type (channel_rating_curve_parameters) :: rcurv
      
      type channel_rating_curve
        real :: wid_btm = 0.          !m          |bottom width of main channel
        !! elev - 1=.1 bf dep; 2=bf dep; 3=2*bf dep
        type (channel_rating_curve_parameters), dimension(3) :: elev
      end type channel_rating_curve
      type (channel_rating_curve), dimension(:), allocatable :: ch_rcurv
              
      interface operator (*)
        module procedure chrc_mult
      end interface 
             
      contains
            
      !! this function multiplies the rating curve by a ratio
      !! used when interpolating flow rates in flood routing
      function chrc_mult (rc1, const) result (rc2)
        type (channel_rating_curve_parameters), intent (in) :: rc1
        real, intent (in) :: const
        type (channel_rating_curve_parameters) :: rc2
        rc2%area = rc1%area * const
        rc2%flo_rate = rc1%flo_rate * const
        rc2%dep = rc1%dep * const
        rc2%vol = rc1%vol * const
        rc2%perim_wet = rc1%perim_wet * const
        rc2%celerity = rc1%celerity * const
        rc2%stor_dis = rc1%stor_dis * const
        rc2%ttime = rc1%ttime * const
      end function chrc_mult
      
      subroutine chrc_interp (rc1, rc2, ielev, const, rci)
        type (channel_rating_curve_parameters), intent (in) :: rc1
        type (channel_rating_curve_parameters), intent (in) :: rc2
        type (channel_rating_curve_parameters), intent (out) :: rci
        integer, intent (in) :: ielev
        real, intent (in) :: const
        rci%area = rc1%area + const * (rc2%area - rc1%area)
        rci%flo_rate = rc1%flo_rate + const * (rc2%flo_rate - rc1%flo_rate)
        rci%dep = rc1%dep + const * (rc2%dep - rc1%dep)
        rci%vol = rc1%vol + const * (rc2%vol - rc1%vol)
        rci%perim_wet = rc1%perim_wet + const * (rc2%perim_wet - rc1%perim_wet)
        rci%celerity = rc1%celerity + const * (rc2%celerity - rc1%celerity)
        rci%stor_dis = rc1%stor_dis + const * (rc2%stor_dis - rc1%stor_dis)
        rci%ttime = rc1%ttime + const * (rc2%ttime - rc1%ttime)
     end subroutine chrc_interp
    
      end module channel_velocity_module
