      program main

      use time_module
      use hydrograph_module
      use maximum_data_module
      use calibration_data_module
      use tinamit_module, ONLY : dynamic, cliente_obj, abre, recibe

      implicit none
      character(len = 32) :: arg1, arg2
      prog = " SWAT+ Feb 26 2020    MODULAR Rev 2020.60.2"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "               Revision 60.4          ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

      call get_command_argument(1, arg1)
      call get_command_argument(2, arg2)
      dynamic = ((TRIM(arg1)/='').AND.(TRIM(arg2)/=''))
      if (dynamic) then
        call abre(arg1, arg2, cliente_obj)
      end if

      call proc_bsn
      call proc_date_time
      call proc_db
      call proc_read

      call exco_db_read
      call dr_db_read
      call hyd_connect
      call object_read_output
      call water_rights_read

      call om_water_init
      call pest_cha_res_read
      call path_cha_res_read
      call salt_cha_res_read

      call proc_hru
      call proc_cha
      call proc_aqu

      !! read decision table data for conditional management
      call dtbl_lum_read

      call proc_cond
      call dtbl_res_read
      call dtbl_scen_read
      call dtbl_flocon_read
      call hru_dtbl_actions_init

      ! read reservoir and wetland data
      call proc_res
      call wet_read_hyd
      call wet_read
      if (db_mx%wet_dat > 0) call wet_initial

      call proc_cal
      call proc_open

      ! compute unit hydrograph parameters for subdaily runoff
      call unit_hyd_ru_hru

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

      if (cal_hard == "y") then
        deallocate (cal_upd)
        call cal_parmchg_read
        call calhard_control
      end if

      if(dynamic)then !<------------------------close sockets if tinamit is still connected
            call recibe(cliente_obj)
        end if

      write (*,1001)
      open (107,file="success.fin")
      write (107,1001)

 1001 format (/," Execution successfully completed ")

	  stop

      end