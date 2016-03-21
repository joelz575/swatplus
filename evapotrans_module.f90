      module evapotrans_module

      use parm
      use jrw_datalib_module
    
      contains
!! routines for evapotranspiration module
      include 'et_act.f90'
      include 'et_pot.f90'

      end module evapotrans_module