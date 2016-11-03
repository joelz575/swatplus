      module nutrient_module

      use parm
      use jrw_datalib_module
      use time_module
    
      contains
!! routines for nutrient cycling module
      include 'nut_psed.f90'
!      include 'nut_psed1.f90'
      include 'nut_nrain.f90'
!      include 'nut_nrain1.f90'
      include 'nut_nlch.f90'
!      include 'nut_nlch1.f90'
      include 'nut_solp.f90'
!      include 'nut_solp1.f90'
      include 'nut_nminrl.f90'
!      include 'nut_nminrl1.f90'
      include 'nut_nitvol.f90'
!      include 'nut_nitvol1.f90'
      include 'nut_pminrl.f90'
      include 'nut_pminrl2.f90'
      include 'nut_denit.f90'
!      include 'nut_denit1.f90'
      include 'nut_orgn.f90'
!      include 'nut_orgn1.f90'
      include 'nut_orgnc.f90'
   
      end module nutrient_module