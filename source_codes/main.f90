      program main

      use hru_module, only : hru, ihru
      use mgt_operations_module
      use time_module
      use hydrograph_module
      use maximum_data_module
      use conditional_module
      use climate_module
      use calibration_data_module

      implicit none

      prog = " SWAT+ Nov 6 2018    MODULAR Rev 2018.56"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "               Revision 56           ",/,              &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)
      
      call proc_bsn   
      call proc_date_time
      call proc_db
      call proc_read

      call hyd_connect
           
      call object_read_output

      !! read decision table data for conditional management
      !call condition_read
      call dtbl_lum_read
      call dtbl_res_read
      call dtbl_scen_read
      
      call om_water_init
      call pest_water_init
      call path_water_init
      call proc_res
      call proc_hru
      call proc_cha
      call proc_allo
      call proc_cal
      call proc_open
      call proc_cond
      
      ! compute unit hydrograph parameters for subdaily runoff
      if (time%step > 0) call unit_hyd
      
      call dr_ru
        
      call hyd_connect_out
      
      ! save initial time settings for soft calibration runs
      time_init = time

      !! simulate watershed processes
      if (time%step < 0) then
        !! export coefficient - average annual
        time%end_sim = 1
        call command
      else
        call time_control
      end if

      if (cal_soft == "y") call calsoft_control
           
      write (*,1001)
 1001 format (/," Execution successfully completed ")

	  stop
      
      end