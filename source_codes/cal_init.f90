      subroutine cal_init

      use sd_channel_module
      use hru_lte_module
      use organic_mineral_mass_module
      use hru_module, only : hru, hru_init, ihru, sno_hru, sno_init
      use soil_module
      use plant_module
      use hydrograph_module, only : sp_ob
      use calibration_data_module
      
      implicit none
      
      integer :: ical         !          |
      integer :: isdc         !none      |counter
      
      !save initial conditions if calibrating
      ical = 0
      if (cal_codes%hyd_hru == 'y' .or. cal_codes%hyd_hrul == 'y'.or.       &
             cal_codes%plt == 'y' .or. cal_codes%sed == 'y' .or.            &
             cal_codes%nut == 'y' .or. cal_codes%chsed == 'y' .or.          &
             cal_codes%chnut == 'y' .or. cal_codes%res == 'y') ical = 1
             
      if (ical == 1) then
        do ihru = 1, sp_ob%hru
          hru_init(ihru) = hru(ihru)
          soil_init(ihru) = soil(ihru)
          soil1_init(ihru) = soil1(ihru)
          rsd1_init(ihru) = rsd1(ihru)
          pcom_init(ihru) = pcom(ihru)
          sno_init(ihru) = sno_hru(ihru)
        end do
      end if
      
      !save hru_lte initial conditions if calibrating
      if (cal_codes%hyd_hrul == 'y') then
        do ihru = 1, sp_ob%hru_lte
          hlt_init(ihru) = hlt(ihru)
        end do
      end if
      
      !save sdc initial conditions if calibrating
      if (cal_codes%chsed == 'y') then
        do isdc = 1, sp_ob%chandeg
          sdch_init(isdc) = sd_ch(isdc)
        end do
      end if
      
	  return
      end subroutine cal_init