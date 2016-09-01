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

      use parm
      use hydrograph_module
      use subbasin_module
      use hru_module
!      use wateruse_module
      use climate_module
      use aquifer_module
      use channel_module
      use sd_hru_module
      use sd_channel_module
      use basin_module
      use jrw_datalib_module
      use conditional_module
      use reservoir_module
      use input_file_module
      !use output_landscape_module
      
      implicit none
      integer :: mres, ob1, ob2, ires, imp, mrch, irch, isdc, imax, iup
      integer :: ibac, mbac, mbac_db, ii, iob, idfn, isb, ielem, ifld
      integer :: istr_db, mstr_prac, istr, iscenario, j, ichan, idat
      integer :: isched, iauto, ictl
      integer :: isdh, idb, ihru_s
      real :: rto, sumn, t_ch, ch_slope, ch_n, ch_l, tov
      character(len=16):: chg_typ
      real :: chg_val, absmin, absmax, diff, meas
      integer :: num_db, mx_elem, ireg, ilum, iihru, iter, icn, iesco

      prog = "SWAT+ Sep 1 2016    MODULAR Rev 2016.25"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "              Revision 25             ",/,             &
     &          "      Soil & Water Assessment Tool    ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)

!! process input
      
      open (4444,file="diagnostics.out")
      write (4444,4445) 
4445  format (1x,'FILENAME',21x,'REC',3x,'MAX',9x,'FILE STATUS')
		
      call hyd_read_objs
      call readtime_read
      if (time%step > 0) then
        time%dtm = 1440. / time%step
      end if
      call readcio_read
!!!  open file to print all output files that are written
      open (9000,file='files_out.out')
      write (9000,*) 'FILES_OUT - OUTPUT FILES WRITTEN'
      
      call basin_cc_read
      call basin_prm_read
      call basin_prm_default
      call basin_print_codes_read

      write (*,*) 'reading from precipitation file'
      call cli_pmeas
      write (*,*) 'reading from temperature file'
      call cli_tmeas
      write (*,*) 'reading from solar radiation file'
      call cli_smeas
      write (*,*) 'reading from relative humidity file'
      call cli_hmeas
      write (*,*) 'reading from wind file'
      call cli_wmeas
      write (*,*) 'reading from precipitation file'
      
      call cli_wgnread
      write (*,*) 'reading from wgn file'
      call cli_staread
      write (*,*) 'reading from wx station file'
      
      call sep_read
      call solt_db_read
      call topo_read
      call toposub_read
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
      call atmoparm_read
      
      !! read management scheduling and data files
      
      call mgt_irrops_read
      call mgt_fertops_read
      call mgt_autofertops_read
      call mgt_contfertops_read
      call mgt_pestops_read
      call mgt_contpestops_read
      call mgt_harvops_read
      call mgt_grazeops_read
      call mgt_sweepops_read
      call mgt_mgtops_read
      
      !! read structural operations files
      call scen_terrace_read
      call scen_stripcrop_read
      call scen_rsdmgt_read
      call scen_plparmup_read
      call scen_grwway_read
      call scen_fire_read
      call scen_filtstrip_read
      call scen_contour_read
      call scen_bmpuser_read
      !call scen_septic_read
         
      !! call readseptwq         !! read the septic database (read from HRU_READ)
      call readpcom             !! read the plant community database
      
      call cntbl_read
      call landuse_read
        
      call bac_lsinit_read
      call pst_lsinit_read
      
      call hyd_read_connect
      
      call object_output_read
      mhru = sp_ob%hru
      mres = sp_ob%res

      ! read reservoir data
      call res_hyd_read
      call res_sed_read
      call res_nut_read
      call res_pst_read
      call res_weir_read
      
      !! set the object number for each hru-to point to weather station
      if (sp_ob%hru > 0) then
        call hru_read     
        call hru_soil_init (mres)
      end if

      !read calibration data (if included)
      call cal_parms_read
      call update_parm_read

      call update_init
            
      !! read decision table data for conditional management
      call condition_read

      !! read update data
      !call update_sched_read
      call update_cond_read

      call sd_hru_read
      call sd_channel_read
      
      call ls_link
        
      call rte_read_nut
      
      call ch_read_hyd (imax)
      call channel_allo (sp_ob%chan)
      do ich = 1, sp_ob%chan
        !! initialize flow routing variables
        call ch_ttcoef (ich)
      end do
      call ch_read_sed
      call ch_read_nut
      call ch_read_pst
      
      do irch = 1, sp_ob%chan
        i = sp_ob1%chan + irch - 1 
        idat = ob(i)%props
        call ch_initial (idat, irch)
      end do

      call sub_read
      
      call time_conc_init

      call sub_allo
            
      !! allocate reservoir variables
      call res_allo (mres)
      call res_initial (mres)
      
      call res_objects
      
      !! set reservoir object numbers for hru's in flood plain without surface storage

      call drainage_area

      !! read modflow inputs  **Ryan**
      
      call aqu_read
      call aqu_initial

            
      !! read soft calibration parameters
      call codes_cal_read
      call ls_regions_cal_read      !soft data for hru calibration
      call ls_regions_calt_read     !soft data for hru_lte calibration
      call ls_parms_cal_read
      call pl_regions_cal_read     !soft data for hru_lte calibration
      call pl_parms_cal_read
      call ch_regions_cal_read
      call ch_parms_cal_read

      call output_landscape_init

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
      
      !save hru initial conditions if calibrating
      if (cal_codes%ls == 'y') then
        do ihru = 1, mhru
          hru_init(ihru) = hru(ihru)
          soil_init(ihru) = soil(ihru)
          pcom_init(ihru) = pcom(ihru)
        end do
        do ihru = 1, sp_ob%hru_lte
          sd_init(ihru) = sd(ihru)
        end do
      end if
      
      !save sdc initial conditions if calibrating
      if (cal_codes%chsed == 'y') then
        do isdc = 1, sp_ob%chandeg
          sdch_init(isdc) = sd_ch(isdc)
        end do
      end if
      
      ! compute unit hydrograph parameters for subdaily runoff
      if (time%step > 0) call unit_hyd
      
      call dr_sub
        
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
      
      !calibrate hydrology
      if (cal_codes%hyd == 'y') then
        call cal_hyd
        call calt_hyd
		do ireg = 1, db_mx%lscalt_reg
		  do ilum = 1, lscalt(ireg)%lum_num
            lscalt(ireg)%lum(ilum)%meas%srr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%srr
            lscalt(ireg)%lum(ilum)%meas%lfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%lfr
            lscalt(ireg)%lum(ilum)%meas%pcr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%pcr
            lscalt(ireg)%lum(ilum)%meas%etr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%etr
            lscalt(ireg)%lum(ilum)%meas%tfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%tfr
            
            write (5000,500) lscalt(ireg)%lum(ilum)%name, lscalt(ireg)%lum(ilum)%ha, lscalt(ireg)%lum(ilum)%nbyr, &
                    lscalt(ireg)%lum(ilum)%precip_aa_sav,                                                         &
                    lscalt(ireg)%lum(ilum)%meas, lscalt(ireg)%lum(ilum)%aa, lscalt(ireg)%lum(ilum)%prm
		  end do
        end do  
      
	    do isdh = 1, sp_ob%hru_lte
	      idb = sd(isdh)%props
		  write (4999,400) sd(isdh)%name, sd_db(idb)%dakm2, sd(isdh)%cn2, sd(isdh)%cn3_swf, sd_db(idb)%tc,       &
		    sd_db(idb)%soildep, sd(isdh)%perco, sd_db(isdh)%slope, sd_db(idb)%slopelen,        &
		    sd(isdh)%etco,  sd_db(idb)%sy, sd_db(idb)%abf, sd(idb)%revapc,                     &
		    sd_db(idb)%percc, sd_db(idb)%sw, sd_db(idb)%gw, sd_db(idb)%gwflow,                 &
		    sd_db(idb)%gwdeep, sd_db(idb)%snow, sd_db(idb)%xlat, sd_db(idb)%itext,             &
		    sd_db(idb)%tropical, sd_db(idb)%igrow1, sd_db(idb)%igrow2, sd_db(idb)%plant, sd(isdh)%stress,      &
		    sd_db(idb)%ipet, sd_db(idb)%irr, sd_db(idb)%irrsrc, sd_db(idb)%tdrain,             &
            sd_db(idb)%uslek, sd_db(idb)%uslec, sd_db(idb)%uslep, sd_db(idb)%uslels
	    end do
		
      end if
        
      !calibrate plant growth
      if (cal_codes%plt == 'y') then
        call cal_plant
      end if
      
      !calibrate sediment yield from uplands (hru's)
      if (cal_codes%sed == 'y') then
        call cal_sed
      end if
      
      !calibrate channel sediment 
      if (cal_codes%chsed == 'y') then
        call cal_chsed
      end if
      
      do i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
        close (i)
      end do
      close(124)
      
      write (*,1001)
 1001 format (/," Execution successfully completed ")
  400 format (a16,19f12.3,4i12,12x,a4,f12.3,3i12,5f12.3)
  500 format (a16,f12.3,i12,f12.3,2(1x,a16,10f12.3),10f12.3)	
	  stop
      end