      subroutine zeroini

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use hru_module, only : eo_30d,hru,nd_30, par,snocov1, volcrmin    
      use soil_module
      use time_module
      
      implicit none

      real :: snocov2             !none          |2nd shape parameter for snow cover equation
                                  !              |This parameter is determined by solving the
                                  !              |equation for 95% snow cover
      eo_30d = 0.        !(CB 8/24/09
      nd_30 = 0
      snocov1 = 0.
      snocov2 = 0.
      volcrmin = 0.
      return
      end subroutine zeroini