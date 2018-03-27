      program main
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    comment changes to test merging with trunk and c:\branch_test code
!!    two lines added to c:\branch_test code

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

      use hru_module, only : hru, ihru, prog, soil
      use hydrograph_module
      use ru_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use mgt_operations_module
      use conditional_module
      use reservoir_module
      use input_file_module
      use organic_mineral_mass_module
      use time_module
      use climate_module
      
      implicit none
      integer :: date_time(8)
      character*10 b(3)
      integer :: ob1, ob2, ires, imp, mrch, irch, isdc, imax, iup
      integer :: ibac, mbac, mbac_db, ii, iob, idfn, isb, ielem, ifld
      integer :: istr_db, mstr_prac, istr, j, ichan, idat
      integer :: isched, iauto, ictl
      integer :: isdh, idb, ihru_s, ical, icvmax
      real :: rto, sumn, t_ch, ch_slope, ch_n, ch_l, tov
      character(len=16):: chg_typ
      real :: chg_val, absmin, absmax, diff, meas
      integer :: num_db, mx_elem, ireg, ilum, iihru, iter, icn, iesco, iord
      integer :: i

      prog = " SWAT+ Mar 27 2018    MODULAR Rev 2018.45"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "              Revision 45             ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

!! process input
      		
      call basin_read_objs
      call readtime_read
      if (time%step > 0) then
        time%dtm = 1440. / time%step
      end if
      call readcio_read
!!!  open file to print all output files that are written
      open (9000,file='files_out.out')
      write (9000,*) 'FILES_OUT - OUTPUT FILES WRITTEN'
      
!!!  open diagnostics.out file to print problems with various files
     open (9001,file='diagnostics.out')
     write (9001,*) 'DIAGNOSTICS.OUT FILE' 
          
      call basin_read_cc
      call basin_read_prm
      call basin_prm_default
      call basin_print_codes_read
   
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      
      write (*,111) 'reading from precipitation file    ', date_time(5), date_time(6), date_time(7)
111   format (1x,a, 5x,'Time',2x,i2,':',i2,':',i2)
      call cli_pmeas
      write (*,111) 'reading from temperature file      ', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_tmeas
      write (*,111) 'reading from solar radiation file  ', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_smeas
      write (*,111) 'reading from relative humidity file', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_hmeas
      write (*,111) 'reading from wind file             ', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_wmeas
      write (*,111) 'reading from wgn file              ', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_wgnread
      write (*,111) 'reading from wx station file       ', date_time(5), date_time(6), date_time(7)
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      call cli_read_atmodep
      call cli_staread
   
      call sep_read
      call solt_db_read
      call topo_read
      call field_read
      call hydrol_read
      
      call sdr_read
      call snowdb_read
      call soil_db_read
      
     !! databases used by all spatial modules
      call plantparm_read                             !! read the plant paramter database
      call plantparm_init                             !! initialize plant parameters
      call tillparm_read                              !! read the tillage database
      call pestparm_read                              !! read the pesticide database
      call fertparm_read                              !! read the fertilizer/nutrient database
      call urbanparm_read                             !! read the urban land types database
      call bac_read_lsparms                           !! read the bacteria data parameters
      call septicparm_read 
      
      !! read management scheduling and data files
      
      call mgt_read_irrops
      call mgt_read_chemapp
      call mgt_read_harvops
      call mgt_read_grazeops
      call mgt_read_sweepops
      call mgt_read_fireops
      call mgt_read_mgtops
      
      !! read structural operations files
      call scen_read_grwway
      call scen_read_filtstrip
      call scen_read_bmpuser
      !call scen_septic_read
         
      !! call readseptwq         !! read the septic database (read from HRU_READ)
      call readpcom              !! read the plant community database
      
      call cntbl_read
      call cons_prac_read
      call overland_n_read
      call landuse_read

      call bac_read_lsinit
      call pst_read_lsinit
      
      call hyd_connect
      
      call object_read_output

      !! read decision table data for conditional management
      call condition_read     
      
      ! read reservoir data
      call res_read_init
      call res_read_hyd
      call res_read_sed
      call res_read_nut
      call res_read_pst
      call res_read_weir
      call res_read
      
      call header_snutc
      
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
      
      call ch_read_init
      call ch_read_hyd
      call ch_read_sed
      call ch_read_nut
      call ch_read_pst
      call ch_read
      
      call channel_allo
      
      do ich = 1, sp_ob%chan
        !! initialize flow routing variables
        call ch_ttcoef (ich)
      end do
         
      do irch = 1, sp_ob%chan
        i = sp_ob1%chan + irch - 1 
        idat = ob(i)%props
        call ch_initial (idat, irch)
      end do

      !call ru_read
      
      call time_conc_init

      call ru_allo
            
      !! allocate and initialize reservoir variables
      call res_allo
      call res_objects
      call res_initial

      call aqu_read
      call aqu_initial

      !read calibration data (if included)
      call cal_read_parms
      call update_read_parm

      call update_init
            
      !! read update data
      !call update_sched_read
      call update_read_cond
            
      !! read soft calibration parameters
      call codes_read_cal
      call lsu_read_elements        !defining landscape units by hru
      !call reg_read_elements        !defining regions by lsu and/or hru
      call lcu_read_softcal         !soft data for landscape calibration (needs to be renamed)***
      call ls_read_lsparms_cal
      call pl_read_regions_cal      !soft data for hru_lte calibration
      call pl_read_parms_cal
      call aqu_read_elements        !defining regions by aquifer
      call ch_read_elements        !defining regions by channel
      call res_read_elements        !defining regions by reservoir
      call rec_read_elements        !defining regions by recall object (point source, gage data, model output, etc)
      call ch_read_orders_cal
      call ch_read_parms_cal

      !! write headers in output files
      call output_landscape_init
      call header_channel
      call header_aquifer
      call header_sd_channel
      call header_mgt
      call header_yield
      call header_hyd
      call header_reservoir
      call header_wetland

      call header_write
            
      !! set cross walk for auto management operations
      do ihru = 1, sp_ob%hru
        isched = hru(ihru)%mgt_ops
        if (sched(isched)%num_autos > 0) then
           sched(isched)%irr = 1
           !! crosswalk with conditional.ctl
           do iauto = 1, sched(isched)%num_autos
             do ictl = 1, db_mx%d_tbl
               if (sched(isched)%auto_name(iauto) == d_tbl(ictl)%name) then
                 sched(isched)%num_db(iauto) = ictl
               end if
             end do
           end do
         end if
      end do
      
      call cal_init
      
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

      call cal_control
      
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      
      write (*,1001)
 1001 format (/," Execution successfully completed ")

	  stop
      end