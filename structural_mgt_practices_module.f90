      module structural_mgt_practices_module

      use parm
      use time_module
      use constituent_mass_module
    
      contains
!! routines for structural management practices module
      include 'smp_bmpfixed.f90'
      include 'smp_buffer.f90'
      include 'smp_filter.f90'
      include 'smp_filtw.f90'
      include 'smp_grass_wway.f90'


      end module structural_mgt_practices_module