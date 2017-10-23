      module hru_module

        use parm
        use jrw_datalib_module
        use bacteria_module
        use jrw_process_module
        use climate_parms
        use time_module
        use hydrograph_module
        use constituent_mass_module
        use reservoir_module

      contains
        !include 'hru_control.f90'
        !include 'bac_apply_hrucon.f90'
        !include 'erfc.f90'
        !include 'hru_soiltest_update.f90'
        !include 'hru_soil_chem.f90'
        !include 'hru_soil_phys.f90'
        !include 'hru_sweep.f90'
        !include 'hru_urbanhr.f90'    !!! put in urban module
        !include 'hru_urban.f90'      !!!  "   " 
        !include 'hru_urb_bmp.f90'    !!!  "   " 
        !include 'hru_output.f90'
        include 'hru_read.f90'
        !include 'bac_hrucontrol.f90'
        !include 'bac_lsinit_read.f90'
        !include 'pst_lsinit_read.f90'
        !include 'hru_soil_assign.f90'
        !include 'rls_routesurf.f90'
        
        !! removing modules within hru module
        !include 'rls_routesoil.f90'
        !include 'swr_depstor.f90'
        !include 'swr_drains.f90'
        !include 'swr_percmacro.f90'
        !include 'swr_percmain.f90'
        !include 'swr_percmicro.f90'
        !include 'swr_satexcess.f90'
        !include 'swr_substor.f90'
        !include 'swr_latsed.f90'
        !include 'swr_subwq.f90'
        !include 'swr_origtile.f90'

      end module hru_module