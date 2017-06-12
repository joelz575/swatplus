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
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    mcr         |none          |max number of crops grown per year
!!    nhru        |none          |number of HRUs in watershed
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    i           |julian date   |current day in simulation--loop counter
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    iida        |julian date   |day being simulated (current julian day)

!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
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

      use jrw_datalib_module
      use parm
      use time_module
      use mgtops_module
      use climate_module
      use basin_module
      use sd_channel_module
      use hru_lte_module
      use basin_module
      
      integer :: j, iix, iiz, ic, mon, ii
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
        else 
          ndays = ndays_noleap
        end if

        !! set beginning day of simulation for year
        if (time%yrs > 1 .or. time%idaf == 0) then
          time%idaf = 1
        end if

        !! set ending day of simulation for year
        time%idal = ndays(13)
        if (time%yrs == time%nbyr .and. time%idal_in > 0) then
          time%idal = time%idal_in
          time%idal =  amin0 (time%idal, ndays(13))  ! if user inputs 366 on non-leap year
        end if
        
        !! sum years of printing for average annual writes
        if (time%yrs > pco%nyskip) then
          time%yrs_prt = time%yrs_prt + float(time%idal - time%idaf + 1)
        else
          !! tell user they are skipping more years than simulating
          time%yrs_prt = time%nbyr
        end if
        
        !! set current julian date to begin annual simulation
        iida = time%idaf

        call xmon
       if (ifirstatmo == 1) then
         ifirstatmo = 0
         if (bsn_cc%atmo == 2) then 
           iyr_at = iyr_atmo1
           mo_at = mo_atmo1
            do
              mo_atmo = mo_atmo + 1
              if (iyr_at == time%yrc .and. mo_at == i_mo) exit
              mo_at = mo_at + 1
              if (mo_at > 12) then
                mo_at = 1
                iyr_at = iyr_at + 1
              endif
              if (mo_atmo > 1000) exit
            end do  
         endif
       endif
       
        do i = time%idaf, time%idal       !! begin daily loop
            
          !! determine month
          iida = i
          call xmon
          time%day = i
          write (*,1234) time%yrs, time%day
          time%mo = i_mo
          !! check for end of month, year and simulation
          time%end_mo = 0
          time%end_yr = 0
          time%end_sim = 0
          if (time%end_aa_prt == 1) then
            time%end_aa_prt = 0
            time%prt_int_cur = 0.
          end if
          if (i == ndays(i_mo+1)) then
            time%end_mo = 1
          end if
          if (i == time%idal) then
            time%end_yr = 1
            if (time%yrs == time%nbyr) then
              time%end_sim = 1
              time%yrs_prt = time%yrs_prt / 365.
            end if
            if (time%yrc == pco%aa_yrs(time%prt_int_cur)) then
              time%end_aa_prt = 1
              time%yrs_prt_int = time%yrs_prt_int / 365.
              time%prt_int_cur = time%prt_int_cur + 1 
            end if
          end if

          !! check time interval for daily printing
          if (time%yrc >= pco%yr_start .and. time%day >= pco%jd_start .and. time%yrc <= pco%yr_end  &
                                                    .and. time%day <= pco%jd_end) then
            int_print = int_print + 1
            if (int_print > pco%interval) int_print = 1
          end if
          
          !! initialize variables at beginning of day for hru's
          if (sp_ob%hru > 0) call sim_initday

          dtot = dtot + 1.

          if (time%yrs > pco%nyskip) ndmo(i_mo) = ndmo(i_mo) + 1

          call climate_control      !! read in/generate weather

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
          
          call soil_write  

        do ihru = 1, nhru  
          isched = hru(j)%mgt_ops
          if (sched(isched)%num_ops > 0) then
          if (time%idaf > 180 .and. wst(iwst)%lat < 0) then
            if (i == 180) then
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
        
        do j = 1, mhru
          !! zero yearly balances after using them in soft data calibration (was in hru_output)
          hwb_y(j) = hwbz
          hnb_y(j) = hnbz
          hpw_y(j) = hpwz
          hls_y(j) = hlsz
          
          !! compute biological mixing at the end of every year
          !! if (biomix(j) > .001) call mgt_tillmix (j, biomix(j), 0)
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

          !! update target nitrogen content of yield with data from
          !! year just simulated
          do ic = 1, mcr
            xx = Real(time%yrs)
            tnylda(j) = (tnylda(j) * xx + tnyld(j)) / (xx + 1.)
          end do

          if (time%idaf < 181) then
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
      
      call cal_ave_output

      return
 1234 format (1x,' Executing year/day ', 2i4)
      end subroutine time_control