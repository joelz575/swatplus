      subroutine proc_hru
    
      use hydrograph_module
      use maximum_data_module
      use hru_module
      use soil_module
    
      implicit none

       !! set the object number for each hru-to point to weather station
      if (sp_ob%hru > 0) then
        call hru_read    
        call hrudb_init
        call topohyd_init
        call soils_init
        call soiltest_all_init
        call hru_output_allo
        call bacteria_init
        call pesticide_init
        call plant_all_init
        call hydro_init
        if (db_mx%wet_dat > 0) call wet_initial
      end if
            
      !if tile drains, set dep_imp to zero
      do ihru = 1, sp_ob%hru
        if (hru(ihru)%tiledrain > 0) then
          hru(ihru)%hyd%dep_imp = soil(ihru)%phys(soil(ihru)%nly)%d
        end if
      end do
        
      call hru_lte_read
      call sd_channel_read
      
      call ls_link
        
      call rte_read_nut
       
	  return
      
      end subroutine proc_hru