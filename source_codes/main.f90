      program main


      use tinamit_module
      use hru_module, only : hru, ihru
      use mgt_operations_module
      use time_module
      use hydrograph_module
      use maximum_data_module
      use conditional_module
      use climate_module
      use calibration_data_module

      implicit none
      character(len = 32) :: arg1, arg2
      prog = " SWAT+ Oct 10 2019    MODULAR Rev 2019.60.1"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "               Revision 60.1          ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

      call get_command_argument(1, arg1)
      call get_command_argument(2, arg2)
      call abre(arg1, arg2, cliente_obj)
      call proc_bsn
      print *, "proc_bsn"
      call proc_date_time
      print *, "proc_date_time"
      call proc_db
      print *, "proc_db"
      call proc_read
      print *, "proc_read"
      call exco_db_read
      print *, "exco_db_read"
      call dr_db_read
      print *, "dr_db_read"
      call hyd_connect
      print *, "hyd_connect"
      call object_read_output
      print *, "object_read_output"

      !! read decision table data for conditional management
      call dtbl_lum_read
      print *, "dtbl_lum_read"
      call dtbl_res_read
      print *, "dtbl_res_read"
      call dtbl_scen_read
      print *, "dtbl_scen_read"
      call dtbl_flocon_read
      print *, "dtbl_flocon_read"

      call om_water_init
      print *, "om_water_init"
      call pest_cha_res_read
      print *, "pest_cha_res_read"
      call path_cha_res_read
      print *, "path_cha_res_read"
      call salt_cha_res_read
      print *, "salt_cha_res_read"
      call proc_res
      print *, "proc_res"
      call proc_hru
      print *, "proc_hru"
      call proc_cha
      print *, "proc_cha"
      call proc_allo
      print *, "proc_allo"
      call proc_cond
      print *, "proc_cond"
      call hru_dtbl_actions_init
      print *, "hru_dtbl_actions_init"
      call proc_cal
      print *, "proc_cal"
      call proc_open
      print *, "proc_open"

      ! compute unit hydrograph parameters for subdaily runoff
      if (time%step > 0) call unit_hyd
      
      call dr_ru
      print *, "dr_ru"

      call hyd_connect_out
      print *, "hyd_connect_out"
      ! save initial time settings for soft calibration runs
      time_init = time

      !! simulate watershed processes
      if (time%step < 0) then
        !! export coefficient - average annual
        time%end_sim = 1
        call command
        print *, "command"
      else
        call time_control
          print *, "time_control"
      end if
      
      if (cal_soft == "y") call calsoft_control
      print *, "calsoft_cotrol"
      if (cal_hard == "y") then
        deallocate (cal_upd)
        call cal_parmchg_read
        print *, "cal_parmchg_read"
        call calhard_control
        print *, "calhard_control"

      end if
           
      write (*,1001)
 1001 format (/," Execution successfully completed ")

	  stop
      
      end