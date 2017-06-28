      module mgtops_module

      use parm 
      use plant_module
      use time_module
      use jrw_datalib_module
      use constituent_mass_module
      
      contains
!! routines for management operations module
      include 'mgt_plantop.f90' 
      include 'mgt_dormant.f90' 
      include 'mgt_harvestop.f90' 
      include 'mgt_harvgrainop.f90' 
      include 'mgt_killop.f90' 
      include 'mgt_newtillmix.f90'
      include 'mgt_sched.f90'
      include 'mgt_operatn.f90'
      include 'mgt_tillfactor.f90'
      include 'mgt_tillmix.f90'
      include 'mgt_trop_gro.f90'

      end module mgtops_module