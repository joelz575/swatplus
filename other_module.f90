      module other_module

      use parm
      use time_module
    
      contains
!! routines for other (leftover) module
      include 'sub_subbasin.f90'
      include 'varinit.f90'
      include 'water_hru.f90'
 !     include 'curno.f90'
 !     include 'ttcoef.f'
      include 'albedo.f90'
      include 'surface.f90'
      include 'wattable.f90'
          
      end module other_module