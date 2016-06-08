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
      
      integer :: j, iix, iiz, ic, mon, ii
      integer :: isce = 1
      real :: xx
      character(len=16):: chg_parm, chg_typ

      time%yrc = time%yrc_start

      do curyr = 1, time%nbyr
        time%yrs = curyr
        
        !! initialize annual variables for hru's
        if (sp_ob%hru > 0) call sim_inityr

        !!determine beginning and ending dates of simulation in current year
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
          if (i == ndays(i_mo+1)) then
            time%end_mo = 1
          end if
          if (i == time%idal) then
            time%end_yr = 1
            if (time%yrs == time%nbyr) then
              time%end_sim = 1
              time%yrs_prt = time%yrs_prt / 365.
            end if
          end if

          !! initialize variables at beginning of day for hru's
          if (sp_ob%hru > 0) call sim_initday

          dtot = dtot + 1.

          if (time%yrs > pco%nyskip) ndmo(i_mo) = ndmo(i_mo) + 1

          call climate_control      !! read in/generate weather
          
          !! check to determine if scenarios need to be set
          if (db_mx%updates > 0) then
          do while (time%day == upd_sched(isce)%day .and. time%yrc ==        & 
                                                    upd_sched(isce)%year)
            if (upd_sched(isce)%cond == 'null') then
              if (upd_sched(isce)%typ == 'parameters') then
                do ichg_par = 1, upd_sched(isce)%num
                  do ispu = 1, upd_sched(isce)%upd_prm(ichg_par)%num_tot
                    ielem = upd_sched(isce)%upd_prm(ichg_par)%num(ispu)
                    chg_parm = upd_sched(isce)%upd_prm(ichg_par)%name
                    chg_typ = upd_sched(isce)%upd_prm(ichg_par)%chg_typ
                    chg_val = upd_sched(isce)%upd_prm(ichg_par)%val
                    absmin = cal_parms(upd_sched(isce)%upd_prm(ichg_par)%num_db)%absmin
                    absmax = cal_parms(upd_sched(isce)%upd_prm(ichg_par)%num_db)%absmax
                    num_db = upd_sched(isce)%upd_prm(ichg_par)%num_db
                    call current_par_value (ielem, lyr, chg_parm, chg_typ, chg_val, absmin, absmax, num_db)
                  end do
                end do
              else if (upd_sched(isce)%typ == 'structure') then
                do ichg_par = 1, upd_sched(isce)%num
                  do ispu = 1, upd_sched(isce)%upd_prm(ichg_par)%num_tot
                    ielem = upd_sched(isce)%upd_prm(ichg_par)%num(ispu)
                    call structure_set_parms (upd_sched(isce)%name, upd_sched(isce)%upd_prm(ichg_par)%num_db, ielem)
                  end do
                end do
              else if (upd_sched(isce)%typ == 'land_use_mgt') then
                !! change management or entire land use
              end if
            end if
            isce = isce + 1
          end do
          end if

          !! conditional reset of land use and management
          if (time%day == 1) then  !only on first day of year
            do iupd = 1, db_mx%updates
              if (upd_sched(iupd)%typ == 'land_use' .and. upd_sched(iupd)%cond /= 'null') then
                do j = 1, mhru
                  id = upd_sched(iupd)%cond_num
                  call conditions (id, j)
                  call actions (id, j)
                end do
              end if
            end do
          end if
          
          !! allocate water for water rights objects
          !call water_allocation

          call command              !! command loop
          
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
        
        !! sum output for soft data calibration
        if (cal_codes%ls == 'y') then
          do ireg = 1, db_mx%lscal_reg
            do ilu = 1, lscal(ireg)%lum_num
            if (lscal(ireg)%msubs <= 0) then
              lscal(ireg)%lum(ilu)%ha = 0.
              lscal(ireg)%lum(ilu)%precip = 0.
              lscal(ireg)%lum(ilu)%sim = lscal_z  !! zero all calibration parameters
              do ihru = 1, mhru
                if (hru(ihru)%land_use_mgt == lscal(ireg)%lum(ilu)%lum_no) then
                  ha_hru = 100. * hru(ihru)%km      ! 10 * ha * mm --> m3
                  lscal(ireg)%lum(ilu)%ha = lscal(ireg)%lum(ilu)%ha + ha_hru
                  lscal(ireg)%lum(ilu)%precip = lscal(ireg)%lum(ilu)%precip + (10. * ha_hru * hwb_y(ihru)%precip)
                  lscal(ireg)%lum(ilu)%sim%srr = lscal(ireg)%lum(ilu)%sim%srr + (10. * ha_hru * hwb_y(ihru)%surq_gen)
                  lscal(ireg)%lum(ilu)%sim%etr = lscal(ireg)%lum(ilu)%sim%etr + (10. * ha_hru * hwb_y(ihru)%et)
                  lscal(ireg)%lum(ilu)%sim%tfr = lscal(ireg)%lum(ilu)%sim%tfr + (10. * ha_hru * hwb_y(ihru)%qtile)
                  lscal(ireg)%lum(ilu)%sim%sed = lscal(ireg)%lum(ilu)%sim%sed + (ha_hru * hls_y(ihru)%sedyld)
                  !add nutrients
                end if
              end do
            else
              do isub = 1, lscal(ireg)%msubs
              end do
            end if
            end do  !lum_num
            
            do ilu = 1, lscal(ireg)%lum_num
              if (lscal(ireg)%lum(ilu)%ha > 1.e-6) then
                lscal(ireg)%lum(ilu)%nbyr = lscal(ireg)%lum(ilu)%nbyr + 1
                !! convert back to mm, t/ha, kg/ha
                lscal(ireg)%lum(ilu)%precip_aa = lscal(ireg)%lum(ilu)%precip_aa + lscal(ireg)%lum(ilu)%precip / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%srr = lscal(ireg)%lum(ilu)%aa%srr + lscal(ireg)%lum(ilu)%sim%srr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%etr = lscal(ireg)%lum(ilu)%aa%etr + lscal(ireg)%lum(ilu)%sim%etr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%tfr = lscal(ireg)%lum(ilu)%aa%tfr + lscal(ireg)%lum(ilu)%sim%tfr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%sed = lscal(ireg)%lum(ilu)%aa%sed + lscal(ireg)%lum(ilu)%sim%sed / lscal(ireg)%lum(ilu)%ha
                ! add nutrients
              end if
            end do
          end do    !reg
        end if

        do j = 1, mhru
          !! zero yearly balances after using them in soft data calibration (was in hru_output)
          hwb_y(j) = hwbz
          hnb_y(j) = hnbz
          hpw_y(j) = hpwz
          hls_y(j) = hlsz
          
          !! compute biological mixing at the end of every year
          !! if (biomix(j) > .001) call mgt_tillmix (j,biomix(j))
          if (hru(j)%hyd%biomix > .001)                              &
                   call mgt_newtillmix (j,hru(j)%hyd%biomix)

          !! update sequence number for year in rotation to that of
          !! the next year and reset sequence numbers for operations
          do ipl = 1, npl(j)
            idp = pcom(j)%plcur(ipl)%idplt
            if (idp > 0) then
              if (pldb(idp)%idc == 7) then
                pcom(j)%plcur(ipl)%curyr_mat =                            &
                    pcom(j)%plcur(ipl)%curyr_mat + 1
                pcom(j)%plcur(ipl)%curyr_mat =                            &
                    Min(pcom(j)%plcur(ipl)%curyr_mat,pldb(idp)%mat_yrs)
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

        !! average output for soft data calibration
        if (cal_codes%ls == 'y') then
          do ireg = 1, db_mx%lscal_reg
            do ilu = 1, lscal(ireg)%lum_num
              if (lscal(ireg)%lum(ilu)%nbyr > 0) then
                !! convert back to mm, t/ha, kg/ha
                lscal(ireg)%lum(ilu)%precip_aa = lscal(ireg)%lum(ilu)%precip_aa / lscal(ireg)%lum(ilu)%nbyr
                lscal(ireg)%lum(ilu)%aa%srr = lscal(ireg)%lum(ilu)%aa%srr / lscal(ireg)%lum(ilu)%nbyr
                lscal(ireg)%lum(ilu)%aa%etr = lscal(ireg)%lum(ilu)%aa%etr / lscal(ireg)%lum(ilu)%nbyr
                lscal(ireg)%lum(ilu)%aa%tfr = lscal(ireg)%lum(ilu)%aa%tfr / lscal(ireg)%lum(ilu)%nbyr
                lscal(ireg)%lum(ilu)%aa%sed = lscal(ireg)%lum(ilu)%aa%sed / lscal(ireg)%lum(ilu)%nbyr
                ! add nutrients
              end if
            end do
          end do
        end if

      return
 1234 format (1x,' Executing year/day ', 2i4)
      end subroutine time_control