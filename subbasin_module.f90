      module subbasin_module

      use parm
      use sd_hru_module
      use hydrograph_module
      use output_landscape_module
      use jrw_datalib_module
      use channel_module

      integer :: isub, msub_db
      real, dimension (:), allocatable :: sub_tc, sub_n, hyd_flo
      integer, dimension (:), allocatable :: itsb
      real, dimension (:,:), allocatable :: uhs
      
      type subbasin_databases
        integer :: elem_def = 1
        integer :: elem_dr = 1
        integer :: toposub_db = 1
        integer :: field_db = 1
      end type subbasin_databases
    
      type subbasin_parameters
        character(len=16) :: name = ""
        real :: da_km2 = 0.      !! km2      drainage area
        type (subbasin_databases) :: dbs
      end type subbasin_parameters
      type (subbasin_parameters), dimension(:), allocatable :: sub

      contains
!! routines for routing unit module
      include 'subbasin_control.f90'
      include 'sub_read.f90'
      include 'sub_allo.f90'
      include 'subbasin_output.f90'

      end module subbasin_module