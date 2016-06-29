      subroutine cal_hyd

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

      !calibrate hydrology
        ical_hyd = 0
        
        ! 1st cn adjustment
        isim = 0
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn < 1.e-6) then
            isim = 1
            do ihru_s = 1, lscal(ireg)%num_tot
              iihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for 1st surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                !call time_control - check if reinitializing in working
                  
                diff = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%srr
                chg_val = diff / 10.     !assume 10 mm runoff for 1 cn
                lscal(ireg)%lum(ilum)%prm_prev%cn = lscal(ireg)%lum(ilum)%prm%cn
                lscal(ireg)%lum(ilum)%prm%cn = lscal(ireg)%lum(ilum)%prm%cn + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                
                if (lscal(ireg)%lum(ilum)%prm%cn >= ls_prms(1)%pos) then
                  chg_val = ls_prms(1)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn <= ls_prms(1)%neg) then
                  chg_val = ls_prms(1)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if

                cn2(iihru) = cn2(iihru) + chg_val
                cn2(iihru) = amin1 (cn2(iihru), 95.)
                cn2(iihru) = Max (cn2(iihru), 35.)
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        ! 1st cn adjustment 
        if (isim > 0) call time_control

          ! adjust surface runoff using cn
          do icn = 1, 2
          isim = 0
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn < 1.e-6) then
            isim = 1
            !check all hru's for proper lum
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                rmeas = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
                chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%cn - lscal(ireg)%lum(ilum)%prm%cn)                  &
                            * (lscal(ireg)%lum(ilum)%aa%srr - rmeas) / (lscal(ireg)%lum(ilum)%prev%srr - rmeas)
                lscal(ireg)%lum(ilum)%prm_prev%cn = lscal(ireg)%lum(ilum)%prm%cn
                lscal(ireg)%lum(ilum)%prm%cn = lscal(ireg)%lum(ilum)%prm%cn + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                                
                if (lscal(ireg)%lum(ilum)%prm%cn >= ls_prms(1)%pos) then
                  chg_val = ls_prms(1)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn <= ls_prms(1)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%cn - ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if

                cn2(iihru) = cn2(iihru) + chg_val
                cn2(iihru) = amin1 (cn2(iihru), 95.)
                cn2(iihru) = Max (cn2(iihru), 35.)
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            end if
          end do
        end do
        ! cn adjustment
        if (isim > 0) call time_control
        ! if within uncertainty limits (in each lum) - go on to next variable
        end do      ! icn
          
        ! 1st esco adjustment
        isim = 0
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            !check all hru's for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%etr) / soft)
            if (diff > .025 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%esco < 1.e-6) then
            isim = 1
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for 1st surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                !call time_control - check if reinitializing in working
                  
                diff = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%etr
                chg_val = diff / 200.     ! increment esco .05 for every 10 mm difference
                lscal(ireg)%lum(ilum)%prm_prev%esco = lscal(ireg)%lum(ilum)%prm%esco
                lscal(ireg)%lum(ilum)%prm%esco = lscal(ireg)%lum(ilum)%prm%esco + chg_val
                lscal(ireg)%lum(ilum)%prev%etr = lscal(ireg)%lum(ilum)%aa%etr
                           
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco - lscal(ireg)%lum(ilum)%prm%esco
                if (hru(iihru)%hyd%esco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%esco
                  lscal(ireg)%lum(ilum)%prm%esco = chg_val
                  hru(iihru)%hyd%esco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%esco = 1.
                end if
                if (hru(iihru)%hyd%esco <= ls_prms(2)%neg) then
                  chg_val = ls_prms(2)%neg - lscal(ireg)%lum(ilum)%prm_prev%esco
                  lscal(ireg)%lum(ilum)%prm%esco = chg_val
                  hru(iihru)%hyd%esco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%esco = 1.
                end if
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        ! 1st esco adjustment 
        if (isim > 0) call time_control
        
        ! adjust et using esco
        do iesco = 1, 2
          isim = 0
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            !check all hru's for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%etr) / soft)
            if (diff > .025 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%esco < 1.e-6) then
            isim = 1
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for 1st surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                rmeas = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
                chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%esco - lscal(ireg)%lum(ilum)%prm%esco)                  &
                            * (lscal(ireg)%lum(ilum)%aa%etr - rmeas) / (lscal(ireg)%lum(ilum)%prev%etr - rmeas)
                lscal(ireg)%lum(ilum)%prm_prev%esco = lscal(ireg)%lum(ilum)%prm%esco
                lscal(ireg)%lum(ilum)%prm%esco = lscal(ireg)%lum(ilum)%prm%esco + chg_val
                lscal(ireg)%lum(ilum)%prev%etr = lscal(ireg)%lum(ilum)%aa%etr
                           
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco - lscal(ireg)%lum(ilum)%prm%esco
                if (hru(iihru)%hyd%esco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%esco
                  lscal(ireg)%lum(ilum)%prm%esco = chg_val
                  hru(iihru)%hyd%esco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%esco = 1.
                end if
                if (hru(iihru)%hyd%esco <= ls_prms(2)%neg) then
                  chg_val = ls_prms(2)%neg - lscal(ireg)%lum(ilum)%prm_prev%esco
                  lscal(ireg)%lum(ilum)%prm%esco = chg_val
                  hru(iihru)%hyd%esco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%esco = 1.
                end if
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
                
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
          end do
          ! et adjustment 
          if (isim > 0) call time_control
          ! if within uncertainty limits (in each lum) - go on to next variable
        
        end do      ! iesco
        
        ! 1st k adjustment (upper layers) for lateral flow
        isim = 0
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            !check all hru's for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .025 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%k < 1.e-6) then
            isim = 1
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for 1st surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                !call time_control - check if reinitializing in working
                  
                diff = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%lfr
                chg_val = diff       ! increment k by chg_val % - its percent change
                lscal(ireg)%lum(ilum)%prm_prev%k = lscal(ireg)%lum(ilum)%prm%k
                lscal(ireg)%lum(ilum)%prm%k = lscal(ireg)%lum(ilum)%prm%k + chg_val
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                           
                if (lscal(ireg)%lum(ilum)%prm%k >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%k = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%k = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%k <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%k = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%k = 1.
                end if

                do ily = 1, soil(ihru)%nly
                  soil(ihru)%phys(ily)%k =  soil(ihru)%phys(ily)%k / (1. - chg_val / 100.)
                  soil_init(ihru)%phys(ily)%k = soil(ihru)%phys(ily)%k
                end do
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        ! 1st k adjustment 
        if (isim > 0) call time_control

          ! adjust lateral flow using k
          do ik = 1, 2
          isim = 0
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .025 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%k < 1.e-6) then
            isim = 1
            !check all hru's for proper lum
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                rmeas = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
                chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%k - lscal(ireg)%lum(ilum)%prm%k)                  &
                            * (lscal(ireg)%lum(ilum)%aa%lfr - rmeas) / (lscal(ireg)%lum(ilum)%prev%lfr - rmeas)
                lscal(ireg)%lum(ilum)%prm%k = lscal(ireg)%lum(ilum)%prm%k + chg_val
                lscal(ireg)%lum(ilum)%prm_prev%k = lscal(ireg)%lum(ilum)%prm%k
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                                
                if (lscal(ireg)%lum(ilum)%prm%k >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%k = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%k = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%k <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%k = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%k = 1.
                end if

                do ily = 1, soil(ihru)%nly
                  soil(ihru)%phys(ily)%k =  soil(ihru)%phys(ily)%k / (1. - chg_val / 100.)
                  soil_init(ihru)%phys(ily)%k = soil(ihru)%phys(ily)%k
                end do
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        ! k adjustment 
        if (isim > 0) call time_control
        ! if within uncertainty limits (in each lum) - go on to next variable
        end do

        ! 1st k_lo adjustment (bottom layer) for percolation
        isim = 0
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            !check all hru's for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%pcr) / soft)
            if (diff > .025 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%k_lo < 1.e-6) then
            isim = 1
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for 1st surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                !call time_control - check if reinitializing in working
                chg_val = diff       ! increment k_lo 1% for 1% difference in perc
                lscal(ireg)%lum(ilum)%prm_prev%k_lo = lscal(ireg)%lum(ilum)%prm%k_lo
                lscal(ireg)%lum(ilum)%prm%k_lo = lscal(ireg)%lum(ilum)%prm%k_lo + chg_val
                lscal(ireg)%lum(ilum)%prev%pcr = lscal(ireg)%lum(ilum)%aa%pcr
                           
                if (lscal(ireg)%lum(ilum)%prm%k_lo >= ls_prms(4)%pos) then
                  lscal(ireg)%lum(ilum)%prm%k_lo = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%k_lo = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%k_lo <= ls_prms(4)%neg) then
                  lscal(ireg)%lum(ilum)%prm%k_lo = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%k_lo = 1.
                end if

                lly = soil(ihru)%nly
                soil(ihru)%phys(lly)%k =  soil(ihru)%phys(lly)%k * chg_val / 30.
                soil_init(ihru)%phys(lly)%k = soil(ihru)%phys(lly)%k
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            end if
            end do
        end do
        ! 1st k_lo adjustment 
        if (isim > 0) call time_control

          ! adjust percolation using k of lowest layer
          do ik_lo = 1, 2
          isim = 0
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            soft = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%pcr) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%k_lo < 1.e-6) then
            isim = 1
            !check all hru's for proper lum
            do ihru_s = 1, lscal(ireg)%num_tot
              ihru = lscal(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%lum_no == hru(ihru)%land_use_mgt) then
                !set parms for surface runoff calibration and rerun
                hru(iihru) = hru_init(iihru)
                soil(iihru) = soil_init(iihru)
                pcom(iihru) = pcom_init(iihru)
                rmeas = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
                chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%k_lo - lscal(ireg)%lum(ilum)%prm%k_lo)                  &
                            * (lscal(ireg)%lum(ilum)%aa%pcr - rmeas) / (lscal(ireg)%lum(ilum)%prev%pcr - rmeas)
                lscal(ireg)%lum(ilum)%prm%k_lo = lscal(ireg)%lum(ilum)%prm%k_lo + chg_val
                lscal(ireg)%lum(ilum)%prm_prev%k_lo = lscal(ireg)%lum(ilum)%prm%k_lo
                lscal(ireg)%lum(ilum)%prev%pcr = lscal(ireg)%lum(ilum)%aa%pcr
                                
                if (lscal(ireg)%lum(ilum)%prm%k_lo >= ls_prms(4)%pos) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%k_lo - ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm%k_lo = ls_prms(4)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%k_lo = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%k_lo <= ls_prms(4)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%k_lo - ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm%k_lo = ls_prms(4)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%k_lo = 1.
                end if

                lly = soil(ihru)%nly
                soil(ihru)%phys(lly)%k = chg_val * soil(ihru)%phys(lly)%k
                soil_init(ihru)%phys(lly)%k = soil(ihru)%phys(lly)%k
              end if
            end do
            end if
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
          end do
        ! k_lo adjustment 
        if (isim > 0) call time_control
        ! if within uncertainty limits (in each lum) - go on to next variable
        
        end do      ! ik_lo  

	  return
      end subroutine cal_hyd