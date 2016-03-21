      subroutine zeroini

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine zeros values for single array variables

      use parm
      use time_module
      
      bactrolp = 0.
      bactsedlp = 0.
      chla_subco = 0.       !CB 12/2/09
      da_ha = 0.
      eo_30d = 0.        !(CB 8/24/09
      nd_30 = 0
      nhru = 0
      npmx = 1
      nrch = 0
      p_n = 0.
      pest_sol = 0.
      rcor = 0.
      sbactrolp = 0.
      sbactrop = 0.
      sbactsedlp = 0.
      sbactsedp = 0.
      sno3up = 0.
      snocov1 = 0.
      snocov2 = 0.
      thbact = 0.
      volcrmin = 0.
      return
      end subroutine zeroini