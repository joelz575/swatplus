      module surfrunoff_module

      use parm
      use jrw_datalib_module
      
      use climate_parms
    
      contains
!! routines for surface runoff module
      include 'sq_canopyint.f90'
      include 'sq_crackflow.f90'
      include 'sq_crackvol.f90'
      include 'sq_dailycn.f90'
      include 'sq_daycn.f90'
      include 'sq_greenampt.f90'
      include 'sq_snom.f90'
      include 'sq_surfst.f90'
      include 'sq_volq.f90' 

      end module surfrunoff_module