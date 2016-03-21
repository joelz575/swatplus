      module pesticide_module

      use parm
      use constituent_mass_module
    
      contains
!! routines for pesticide module
      include 'pst_decay.f90'
      include 'pst_enrsb.f90'
      include 'pst_lch.f90'
      include 'pst_pesty.f90'
      include 'pst_washp.f90'
 !!     include 'pst_pestw.f90'

      end module pesticide_module