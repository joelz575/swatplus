      module climate_module

      use parm
      use climate_parms
      use time_module
      use hydrograph_module
      use jrw_datalib_module
       
      contains
!! routines for climate module
      include 'cli_pmeas.f90'
      include 'cli_tmeas.f90'
      include 'cli_smeas.f90'
      include 'cli_hmeas.f90'
      include 'cli_pgen.f90'
      include 'cli_weatgn.f90'
      include 'cli_tgen.f90'
      include 'cli_clgen.f90'
      include 'cli_slrgen.f90'
      include 'cli_wndgen.f90'
      include 'cli_pgenhr.f90'
      include 'cli_dstn1.f90'
      include 'cli_wmeas.f90'
      include 'cli_tair.f90'
      include 'cli_rhgen.f90'
      include 'climate_control.f90'
      include 'cli_initwgn.f90'
      include 'cli_staread.f90'
      include 'cli_wgnread.f90'
 !     include 'gcycl.f'
 !     include 'cli_forecast_read.f'
              
      end module climate_module