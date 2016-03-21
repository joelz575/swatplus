      module erosion_module

      use parm
    
      contains
!! routines for erosion module
      include 'ero_alph.f90'
      include 'ero_cfactor.f90'
      include 'ero_eiusle.f90'
      include 'ero_ovrsed.f90'
      include 'ero_pkq.f90'
      include 'ero_ysed.f90'

      end module erosion_module