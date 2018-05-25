      subroutine zeroini

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use hru_module, only : bactrolp,bactrop,bactsedlp,bactsedp,da_ha,eo_30d,hru,nd_30,   &
        nhru,npmx,nrch,par,sbactrolp,sbactrop,sbactsedlp,sbactsedp,sno3up,snocov1,  &
        volcrmin    
      use soil_module
      use time_module
      
      implicit none
      
      real :: p_n                 !none          |algal preference factor for ammonia
      real :: snocov2             !none          |2nd shape parameter for snow cover equation
                                  !              |This parameter is determined by solving the
                                  !              |equation for 95% snow cover
      
      bactrolp = 0.
      bactsedlp = 0.
      da_ha = 0.
      eo_30d = 0.        !(CB 8/24/09
      nd_30 = 0
      nhru = 0
      npmx = 1
      nrch = 0
      p_n = 0.
      sbactrolp = 0.
      sbactrop = 0.
      sbactsedlp = 0.
      sbactsedp = 0.
      sno3up = 0.
      snocov1 = 0.
      snocov2 = 0.
      volcrmin = 0.
      return
      end subroutine zeroini