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

      prog = "SWAT+ Mar 6 2017    MODULAR Rev 2017.31"

      write (*,1000)
 1000 format(1x,"                  SWAT+               ",/,             &
     &          "              Revision 31             ",/,             &
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
        call hru_soil_init (mres)
      end if

      !read calibration data (if included)
      call cal_parms_read
      call update_parm_read

      call update_init
            
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

      !call sub_read
      
      call time_conc_init

      call sub_allo
            
      !! allocate and initialize reservoir variables
      call res_allo (mres)
      call res_objects
      call res_initial (mres)
      
      !! set reservoir object numbers for hru's in flood plain without surface storage

      call drainage_area

      !! read modflow inputs  **Ryan**
      
      call aqu_read
      call aqu_initial

            
      !! read soft calibration parameters
      call codes_cal_read
      call lcu_elements_read        !defining regions by hru
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

      !! set
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
      
      !save initial conditions if calibrating
      ical = 0
      if (cal_codes%hyd_hru == 'y' .or. cal_codes%hyd_hrul == 'y'.or.       &
             cal_codes%plt == 'y' .or. cal_codes%sed == 'y' .or.            &
             cal_codes%nut == 'y' .or. cal_codes%chsed == 'y' .or.          &
             cal_codes%chnut == 'y' .or. cal_codes%res == 'y') ical = 1
             
      if (ical == 1) then
        do ihru = 1, mhru
          hru_init(ihru) = hru(ihru)
          soil_init(ihru) = soil(ihru)
          rsd_init(ihru) = rsd1(ihru)
          pcom_init(ihru) = pcom(ihru)
        end do
      end if
      
      !save hru_lte initial conditions if calibrating
      if (cal_codes%hyd_hrul == 'y') then
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
      
      !calibrate hydrology for hru
      if (cal_codes%hyd_hru == 'y') then
        call cal_hyd
        !print calibrated hydrology for hru_lte
		do ireg = 1, db_mx%lcu_reg
		  do ilum = 1, lscal(ireg)%lum_num
            lscal(ireg)%lum(ilum)%meas%srr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%srr
            lscal(ireg)%lum(ilum)%meas%lfr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%lfr
            lscal(ireg)%lum(ilum)%meas%pcr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%pcr
            lscal(ireg)%lum(ilum)%meas%etr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%etr
            lscal(ireg)%lum(ilum)%meas%tfr = lscal(ireg)%lum(ilum)%precip_aa_sav * lscal(ireg)%lum(ilum)%meas%tfr
            
            write (5000,500) lscal(ireg)%lum(ilum)%name, lscal(ireg)%lum(ilum)%ha, lscal(ireg)%lum(ilum)%nbyr,  &
                    lscal(ireg)%lum(ilum)%precip_aa_sav, lscal(ireg)%lum(ilum)%meas, lscal(ireg)%lum(ilum)%aa,  &
                    lscal(ireg)%lum(ilum)%prm
		  end do
        end do  

        !loop through to find the number of variable updates for calibration.upd from soft calibration
        icvmax = 0
        do ireg = 1, db_mx%lcu_reg
          do ilum = 1, lscalt(ireg)%lum_num
            if (abs(lscalt(ireg)%lum(ilum)%prm%cn) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%esco) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%lat_len) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%k_lo) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%slope) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%tconc) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%etco) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%perco) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%revapc) > 1.e-6) icvmax = icvmax + 1
            if (abs(lscalt(ireg)%lum(ilum)%prm%cn3_swf) > 1.e-6) icvmax = icvmax + 1
	      end do
	    end do
        write (5000,500) ' calibration.upd developed from soft data calibration'
        write (5000,500) icvmax
        write (5000,500) ' NAME   CHG_TYP   VAL    CONDS    LYR1   LYR2    YEAR1   YEAR2   DAY1   DAY2   OBJ_TOT'
        
        !write to calibration.upd and use region and land use as conditions
	    do ireg = 1, db_mx%lcu_reg
          do ilum = 1, lscalt(ireg)%lum_num
            if (abs(lscalt(ireg)%lum(ilum)%prm%cn) > 1.e-6) then
              write (5000,500) ls_prms(1)%name, ls_prms(1)%chg_typ, lscal(ireg)%lum(ilum)%prm%cn, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%esco) > 1.e-6) then
              write (5000,500) ls_prms(2)%name, ls_prms(2)%chg_typ, lscal(ireg)%lum(ilum)%prm%esco, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%lat_len) > 1.e-6) then
              write (5000,500) ls_prms(3)%name, ls_prms(3)%chg_typ, lscal(ireg)%lum(ilum)%prm%lat_len, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%k_lo) > 1.e-6) then
              write (5000,500) ls_prms(4)%name, ls_prms(4)%chg_typ, lscal(ireg)%lum(ilum)%prm%k_lo, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%slope) > 1.e-6) then
              write (5000,500) ls_prms(5)%name, ls_prms(5)%chg_typ, lscal(ireg)%lum(ilum)%prm%slope, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%tconc) > 1.e-6) then
              write (5000,500) ls_prms(6)%name, ls_prms(6)%chg_typ, lscal(ireg)%lum(ilum)%prm%tconc, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%etco) > 1.e-6) then
              write (5000,500) ls_prms(7)%name, ls_prms(7)%chg_typ, lscal(ireg)%lum(ilum)%prm%etco, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%perco) > 1.e-6) then
              write (5000,500) ls_prms(8)%name, ls_prms(8)%chg_typ, lscal(ireg)%lum(ilum)%prm%perco, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%revapc) > 1.e-6) then
              write (5000,500) ls_prms(9)%name, ls_prms(9)%chg_typ, lscal(ireg)%lum(ilum)%prm%revapc, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
            if (abs(lscalt(ireg)%lum(ilum)%prm%cn3_swf) > 1.e-6) then
              write (5000,500) ls_prms(10)%name, ls_prms(10)%chg_typ, lscal(ireg)%lum(ilum)%prm%cn3_swf, '   0    2    0    0    0    0    0    0    0'
              write (5000,500) '   region    =', lscal(ireg)%name
              write (5000,500) '   landuse   =', lscal(ireg)%lum(ilum)%name
            end if
	      end do
	    end do
      end if

      !calibrate hydrology for hru_lte
      if (cal_codes%hyd_hrul == 'y') then
        call calt_hyd
        !print calibrated hydrology for hru_lte
		do ireg = 1, db_mx%lcu_reg
		  do ilum = 1, lscalt(ireg)%lum_num
            lscalt(ireg)%lum(ilum)%meas%srr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%srr
            lscalt(ireg)%lum(ilum)%meas%lfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%lfr
            lscalt(ireg)%lum(ilum)%meas%pcr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%pcr
            lscalt(ireg)%lum(ilum)%meas%etr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%etr
            lscalt(ireg)%lum(ilum)%meas%tfr = lscalt(ireg)%lum(ilum)%precip_aa_sav * lscalt(ireg)%lum(ilum)%meas%tfr
            
            write (5000,500) lscalt(ireg)%name, lscalt(ireg)%lum(ilum)%ha, lscalt(ireg)%lum(ilum)%nbyr,           &
                    lscalt(ireg)%lum(ilum)%precip_aa_sav, lscalt(ireg)%lum(ilum)%meas, lscalt(ireg)%lum(ilum)%aa, &
                    lscalt(ireg)%lum(ilum)%prm	
		  end do
        end do  

	    do isdh = 1, sp_ob%hru_lte
	      idb = sd(isdh)%props
		  write (4999,400) sd(isdh)%name, sd_db(idb)%dakm2, sd(isdh)%cn2, sd(isdh)%cn3_swf, sd_db(idb)%tc,      &
		    sd_db(idb)%soildep, sd(isdh)%perco, sd_db(isdh)%slope, sd_db(idb)%slopelen,                         &
		    sd(isdh)%etco, sd_db(idb)%sy, sd_db(idb)%abf, sd(idb)%revapc,                                       &
		    sd_db(idb)%percc, sd_db(idb)%sw, sd_db(idb)%gw, sd_db(idb)%gwflow,                                  &
		    sd_db(idb)%gwdeep, sd_db(idb)%snow, sd_db(idb)%xlat, sd_db(idb)%itext,                              &
		    sd_db(idb)%tropical, sd_db(idb)%igrow1, sd_db(idb)%igrow2, sd_db(idb)%plant, sd(isdh)%stress,       &
		    sd_db(idb)%ipet, sd_db(idb)%irr, sd_db(idb)%irrsrc, sd_db(idb)%tdrain,                              &
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
        !print calibrated hydrology for hru_lte
		do ireg = 1, db_mx%ch_reg
          do iord = 1, chcal(ireg)%ord_num
            write (5000,502) chcal(ireg)%ord(iord)%name, chcal(ireg)%ord(iord)%length, chcal(ireg)%ord(iord)%nbyr,  &
                    chcal(ireg)%ord(iord)%meas, chcal(ireg)%ord(iord)%aa, chcal(ireg)%ord(iord)%prm
		  end do
        end do  

	    do isdc = 1, sp_ob%chandeg
	      idb = sd_ch(isdc)%props
		  write (4999,400) sd_chd(idb)%name, sd_chd(idb)%order, sd_chd(idb)%route_db, sd_chd(idb)%chw,          &
              sd_chd(idb)%chd, sd_chd(idb)%chs, sd_chd(idb)%chl, sd_chd(idb)%chn, sd_chd(idb)%chk,              &
              sd_ch(isdc)%cherod, sd_ch(isdc)%cov, sd_chd(idb)%hc_cov, sd_chd(idb)%chseq, sd_chd(idb)%d50,      &
              sd_chd(idb)%clay, sd_chd(idb)%bd, sd_chd(idb)%chss, sd_chd(idb)%bedldcoef, sd_chd(idb)%tc,        &
              sd_ch(isdc)%shear_bnk, sd_ch(isdc)%hc_erod, sd_chd(idb)%hc_hgt, sd_chd(idb)%hc_ini
	    end do
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
  !400 format (a16,19f12.3,4i12,12x,a4,f12.3,3i12,5f12.3)
  400 format (2a16,i12,20f12.3)
  500 format (a16,f12.3,i12,f12.3,2(1x,a16,10f12.3),10f12.3)
  501 format (a16,f12.3,i12,f12.3,2(1x,a16,10f12.3),10f12.3,9i5)
  !502 format (a16,f12.3,i12,2(1x,a16,10f12.3),10f12.3)
  502 format (a16,f12.3,i12,2(1x,a16,4f12.3),4f12.3)
	  stop
      end