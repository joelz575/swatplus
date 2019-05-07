      subroutine calsoft_init

      use sd_channel_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hru_module, only : hru, hru_init, ihru, sno_hru, sno_init, bss
      use soil_module
      use plant_module
      use hydrograph_module, only : sp_ob, res, res_om_init, ch_stor, ch_om_water_init, wet, wet_om_init
      use calibration_data_module
      use reservoir_data_module
      use aquifer_module
      
      implicit none

      !! initialize all hru parameters
      if (sp_ob%hru > 0) then
        hru_init = hru
        soil_init = soil
        soil1_init = soil1
        rsd1_init = rsd1
        pcom_init = pcom
        wet = wet_om_init
        sno_init = sno_hru
        bss = 0.
      end if
      
      !! initialize hru_lte parameters
      if (sp_ob%hru_lte > 0) then
        hlt_init = hlt
      end if
      
      !! initialize channel lte storage and dimensions
      if (sp_ob%chandeg > 0) then
        sdch_init = sd_ch
        ch_stor = ch_om_water_init
      end if
      
      !! initialize reservoir storage
      if (sp_ob%res > 0) then
        res = res_om_init
      end if
      
      !! initialize aquifer storage
      if (sp_ob%aqu > 0) then
        aqu_d = aqu_om_init
      end if

	  return
      end subroutine calsoft_init