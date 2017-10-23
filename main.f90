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

      use parm, only : hru, ihru, mhru, prog, scenario 
      use hydrograph_module
      use ru_module
      use hru_module
      use climate_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use jrw_datalib_module
      use conditional_module
      use reservoir_module
      use input_file_module
      use organic_mineral_mass_module
      !use output_landscape_module
      
      implicit none
      integer :: date_time(8)
      character*10 b(3)
      integer :: mres, ob1, ob2, ires, imp, mrch, irch, isdc, imax, iup
      integer :: ibac, mbac, mbac_db, ii, iob, idfn, isb, ielem, ifld
      integer :: istr_db, mstr_prac, istr, iscenario, j, ichan, idat
      integer :: isched, iauto, ictl
      integer :: isdh, idb, ihru_s, ical, icvmax
      real :: rto, sumn, t_ch, ch_slope, ch_n, ch_l, tov
      character(len=16):: chg_typ
      real :: chg_val, absmin, absmax, diff, meas
      integer :: num_db, mx_elem, ireg, ilum, iihru, iter, icn, iesco, iord

      prog = "SWAT+ Oct 23 2017    MODULAR Rev 2017.40"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "              Revision 40             ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

!! process input
      		
      call hyd_read_objs
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
     write (9000,*) 'DIAGNOSTICS.OUT     review for missing files' 
          
      call basin_cc_read
      call basin_prm_read
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
      call bac_lsparms_read                           !! read the bacteria data parameters
      call septicparm_read 
      call cli_atmodep_read
      
      !! read management scheduling and data files
      
      call mgt_irrops_read
      call mgt_chemapp_read
      call mgt_harvops_read
      call mgt_grazeops_read
      call mgt_sweepops_read
      call mgt_fireops_read
      call mgt_mgtops_read
      
      !! read structural operations files
      call scen_grwway_read
      call scen_filtstrip_read
      call scen_bmpuser_read
      !call scen_septic_read
         
      !! call readseptwq         !! read the septic database (read from HRU_READ)
      call readpcom              !! read the plant community database
      
      call cntbl_read
      call cons_prac_read
      call overland_n_read
      call landuse_read
        
      call bac_lsinit_read
      call pst_lsinit_read
      
      call hyd_read_connect
      
      call object_output_read
      mhru = sp_ob%hru
      mres = sp_ob%res

      !! read decision table data for conditional management
      call condition_read     
      
      ! read reservoir data
      call res_init_read
      call res_hyd_read
      call res_sed_read
      call res_nut_read
      call res_pst_read
      call res_weir_read
      call res_read
      
      !! set the object number for each hru-to point to weather station
      if (sp_ob%hru > 0) then
        call hru_read    
        call hru_soil_init
      end if

      call hru_lte_read
      call sd_channel_read
      
      call ls_link
        
      call rte_read_nut
      
      call ch_init_read
      call ch_hyd_read
      call ch_sed_read
      call ch_nut_read
      call ch_pst_read
      call ch_read
      
      call channel_allo (sp_ob%chan)
      
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
      call res_allo (db_mx%res)
      call res_objects
      call res_initial
      
      !! set reservoir object numbers for hru's in flood plain without surface storage

      call drainage_area

      !! read modflow inputs  **Ryan**
      
      call aqu_read
      call aqu_initial

      !read calibration data (if included)
      call cal_parms_read
      call update_parm_read

      call update_init
            
      !! read update data
      !call update_sched_read
      call update_cond_read
            
      !! read soft calibration parameters
      call codes_cal_read
      call lsu_elements_read        !defining landscape units by hru
      call reg_elements_read        !defining regions by lsu and/or hru
      call lcu_softcal_read         !soft data for landscape calibration (needs to be renamed)***
      call ls_parms_cal_read
      call pl_regions_cal_read      !soft data for hru_lte calibration
      call pl_parms_cal_read
      call aqu_elements_read        !defining regions by aquifer
      call cha_elements_read        !defining regions by channel
      call res_elements_read        !defining regions by reservoir
      call rec_elements_read        !defining regions by recall object (point source, gage data, model output, etc)
      call ch_regions_cal_read
      call ch_parms_cal_read

      !! write headers in output files
      call output_landscape_init
      call header_channel
      call header_aquifer
      call header_sd_channel
      call header_soils
      call header_mgt
      call header_yield
      call header_hyd
      call header_reservoir
      call header_wetland

      call header_write
            
      !! set cross walk for auto management operations
      do ihru = 1, mhru
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

!! save initial values
      scenario = 1

      do iscen = 1, scenario
        !! simulate watershed processes
        if (time%step < 0) then
          !! export coefficient - average annual
          time%end_sim = 1
          call command
        else
          call time_control
        end if
      end do
      
      call cal_control
      
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      
      write (*,1001)
 1001 format (/," Execution successfully completed ")

	  stop
      end