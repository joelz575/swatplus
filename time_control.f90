      subroutine time_control

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    biomix(:)   |none          |biological mixing efficiency.
!!                               |Mixing of soil due to activity of earthworms
!!                               |and other soil biota. Mixing is performed at
!!                               |the end of every calendar year.
!!    nhru        |none          |number of HRUs in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    iix         |none          |sequence number of current year in rotation
!!    iiz         |none          |sequence number of current crop grown
!!                               |within the current year
!!    j           |none          |counter
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: sim_inityr, std3, xmon, sim_initday, clicon, command
!!    SWAT: writed, writem, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use maximum_data_module
      use calibration_data_module
      use plant_data_module
      use mgt_operations_module
      use hru_module, only : curyr, hru, ihru, ipl, nhru, nop, pcom, phubase, yr_skip, timest  
      use time_module
      use climate_module
      use basin_module
      use sd_channel_module
      use hru_lte_module
      use basin_module
      use hydrograph_module, only : sp_ob
      use output_landscape_module
      
      integer :: j, iix, iiz, ic, mon, ii, idp
      integer :: isce = 1
      real :: xx
      character(len=16):: chg_parm, chg_typ

      time%yrc = time%yrc_start

      do curyr = 1, time%nbyr
        time%yrs = curyr
        
        !! initialize annual variables for hru's
        if (sp_ob%hru > 0) call sim_inityr

        !! determine beginning and ending dates of simulation in current year
        if (Mod(time%yrc,4) == 0) then 
          ndays = ndays_leap
          time%num_leap = time%num_leap + 1
        else 
          ndays = ndays_noleap
        end if

        !! set beginning day of simulation for year
        if (time%yrs > 1 .or. time%day_start == 0) then
          time%day_start = 1
        end if

        !! set ending day of simulation for year
        time%day_end_yr = ndays(13)
        if (time%yrs == time%nbyr .and. time%day_end > 0) then
          time%day_end_yr = time%day_end
          time%day_end_yr =  amin0 (time%day_end_yr, ndays(13))  ! if user inputs 366 on non-leap year
        end if
        
        !! sum years of printing for average annual writes
        if (time%yrs > pco%nyskip) then
          time%yrs_prt = time%yrs_prt + float(time%day_end_yr - time%day_start + 1)
        else
          !! tell user they are skipping more years than simulating
          time%yrs_prt = time%nbyr
        end if

        do julian_day = time%day_start, time%day_end_yr      !! begin daily loop
          time%day = julian_day
          !! determine month and day of month - time%mo and time%day_mo
          call xmon
          
          write (*,1234) cal_sim, time%mo, time%day_mo, time%yrc
          !! check for end of month, year and simulation
          time%end_mo = 0
          time%end_yr = 0
          time%end_sim = 0
          if (time%end_aa_prt == 1) then
            time%end_aa_prt = 0
            time%prt_int_cur = 0.
          end if
          if (time%day == ndays(time%mo+1)) then
            time%end_mo = 1
          end if
          if (time%day == time%day_end_yr) then
            time%end_yr = 1
            if (time%yrs == time%nbyr) then
              time%end_sim = 1
              time%yrs_prt = time%yrs_prt / (365. + (time%num_leap / time%nbyr))
            end if
            if (pco%aa_numint > 0) then
              if (time%yrc == pco%aa_yrs(time%prt_int_cur)) then
                time%end_aa_prt = 1
                time%yrs_prt_int = time%yrs_prt_int / (365. + (time%num_leap / time%nbyr))
                time%prt_int_cur = time%prt_int_cur + 1 
              end if
            end if
          end if

          !! check time interval for daily printing
          if (pco%day_print_over == 'n') then
          if (pco%day_print == 'n') then 
            if (time%day >= pco%day_start .and. time%yrc >= pco%yrc_start) then
              pco%day_print = 'y'
            end if 
          else
            if (time%day > pco%day_end .and. time%yrc == pco%yrc_end) then
              pco%day_print = 'n'
              pco%day_print_over = 'y'
            else
              pco%int_day_cur = pco%int_day_cur + 1
              if (pco%int_day_cur > pco%int_day) pco%int_day_cur = 1
            end if
          end if
          end if

          !! initialize variables at beginning of day for hru's
          if (sp_ob%hru > 0) call sim_initday

          dtot = dtot + 1.

          if (time%yrs > pco%nyskip) ndmo(time%mo) = ndmo(time%mo) + 1

          call climate_control      !! read in/generate weather
          
          call cli_atmodep_time_control     !! set array counter for atmospheric deposition

          !! conditional reset of land use and management
          do iupd = 1, db_mx%cond_up
            do j = 1, sp_ob%hru
              id = upd_cond(iupd)%cond_num
              call conditions (id, j)
              call actions (id, j)
            end do
          end do

          !! allocate water for water rights objects
          !call water_allocation

          call command              !! command loop
          
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,J)      !!start parallelization Jaehak 2016
    !$OMP DO SCHEDULE(STATIC,1)          
          !do isd = 1, sp_ob%hru_lte
          !  call hru_lte_control (isd)
          !end do
    !$OMP END DO 
    !$OMP END PARALLEL
          
          !if (db_mx%lsu_elem > 0) call basin_output
          !if (db_mx%lsu_out > 0) call lsu_output
          !do isd = 1, sp_ob%hru_lte
          !  call hru_lte_output (isd)
          !end do
          
          !call soil_write  

        do ihru = 1, nhru  
          isched = hru(j)%mgt_ops
          if (sched(isched)%num_ops > 0) then
          if (time%day_start > 180 .and. wst(iwst)%lat < 0) then
            if (time%day == 180) then
              isched = hru(j)%mgt_ops
              if (sched(isched)%mgt_ops(nop(ihru))%op /= "skip") then
                dorm_flag = 1
                call mgt_operatn
                dorm_flag = 0
              endif
              nop(ihru) = nop(ihru) + 1
              if (nop(ihru) > sched(isched)%num_ops) then
                nop(ihru) = 1
              end if
      
              phubase(ihru) = 0.
	        yr_skip(ihru) = 0
	      endif
	    end if
	    endif
        end do

        end do                                        !! end daily loop

        !! perform end-of-year processes
        
        call cal_sum_output
        
        do j = 1, sp_ob%hru_lte
          !! zero yearly balances after using them in soft data calibration (was in hru_lte_output)
          hltwb_y(j) = hwbz
          hltnb_y(j) = hnbz
          hltpw_y(j) = hpwz
          hltls_y(j) = hlsz
        end do
        
        do ich = 1, sp_ob%chandeg
          !! zero yearly balances after using them in soft data calibration (was in sd_channel_output)
          chsd_y(ich) = chsdz
        end do
        
        do j = 1, sp_ob%hru
          !! zero yearly balances after using them in soft data calibration (was in hru_output)
          hwb_y(j) = hwbz
          hnb_y(j) = hnbz
          hpw_y(j) = hpwz
          hls_y(j) = hlsz
          
          !! compute biological mixing at the end of every year
          if (hru(j)%hyd%biomix > 1.e-6) call mgt_newtillmix (j, hru(j)%hyd%biomix, 0)

          !! update sequence number for year in rotation to that of
          !! the next year and reset sequence numbers for operations
          do ipl = 1, pcom(j)%npl
            idp = pcom(j)%plcur(ipl)%idplt
            if (idp > 0) then
              if (pldb(idp)%idc == 7) then
                pcom(j)%plcur(ipl)%curyr_mat = pcom(j)%plcur(ipl)%curyr_mat + 1
                pcom(j)%plcur(ipl)%curyr_mat = Min(pcom(j)%plcur(ipl)%curyr_mat,pldb(idp)%mat_yrs)
              end if
            end if
          end do

          if (time%day_start < 181) then
            isched = hru(j)%mgt_ops
            if (sched(isched)%num_ops > 0) then
              if (sched(isched)%mgt_ops(nop(j))%op /= "skip") then
                dorm_flag = 1
                ihru = j
                call mgt_operatn
                dorm_flag = 0
              end if
              nop(j) = nop(j) + 1
              if (nop(j) > sched(isched)%num_ops) then
                nop(j) = 1
              end if
            
              phubase(j) = 0.
              yr_skip(j) = 0
            end if
          end if
        end do

      !! update simulation year
      time%yrc = time%yrc + 1
      end do            !!     end annual loop
      
      !! ave annual calibration output and reset time for next simulation
      call cal_ave_output
      time = time_init

      return
 1234 format (1x, a, ' Executing month/day/year ', 2i4, 2x,i4)
      end subroutine time_control