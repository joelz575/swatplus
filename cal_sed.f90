      subroutine cal_sed

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
        
        ! 1st tconc adjustment
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            if (lscal(ireg)%msubs <= 0) then
              !check all hru's for proper lum
              do iihru = 1, mhru
                !set parms for 1st surface runoff calibration and rerun
                if (hru(iihru)%land_use_mgt == lscal(ireg)%lum(ilum)%lum_no) then
                  hru(iihru) = hru_init(iihru)
                  soil(iihru) = soil_init(iihru)
                  pcom(iihru) = pcom_init(iihru)
                  lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                  lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                  chg_val = lscal(ireg)%lum(ilum)%meas%sed / lscal(ireg)%lum(ilum)%aa%sed
                  chg_val = chg_val ** 1.7857
                  chg_val = amin1 (chg_val, ls_prms(4)%pos)
                  chg_val = amax1 (chg_val, ls_prms(4)%neg)
                  lscal(ireg)%lum(ilum)%prm%tconc = chg_val
                  tconc(iihru) = tconc(iihru) / chg_val
                  tconc(iihru) = amin1 (tconc(iihru), 1400.)
                  tconc(iihru) = amax1 (tconc(iihru), 0.)
                end if
              end do
            else
              !loop through each subbasin in the region and each hru in each subbasin
            end if
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
        end do
        ! 1st cn adjustment 
        call time_control
        
        do iter = 1, 2
          ! adjust surface runoff using cn
          do isl = 1, 3
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            if (lscal(ireg)%msubs <= 0) then
              !check all hru's for proper lum
              do iihru = 1, mhru
                !set parms for 1st surface runoff calibration and rerun
                if (hru(iihru)%land_use_mgt == lscal(ireg)%lum(ilum)%lum_no) then
                  hru(iihru) = hru_init(iihru)
                  soil(iihru) = soil_init(iihru)
                  pcom(iihru) = pcom_init(iihru)
                  meas = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%cn - lscal(ireg)%lum(ilum)%prm%cn)                  &
                            * (lscal(ireg)%lum(ilum)%aa%srr - meas) / (lscal(ireg)%lum(ilum)%prev%srr - meas)
                  chg_val = amin1 (chg_val, ls_prms(1)%pos)
                  chg_val = amax1 (chg_val, ls_prms(1)%neg)
                  lscal(ireg)%lum(ilum)%prm_prev%cn = lscal(ireg)%lum(ilum)%prm%cn
                  lscal(ireg)%lum(ilum)%prm%cn = lscal(ireg)%lum(ilum)%prm%cn + chg_val
                  lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                  
                  terr_sl = terrace_db(istr)%sl_len
                  xm = 0.6 * (1. - Exp(-35.835 * hru(iihru)%topo%slope))    
                  sin_sl = Sin(Atan(hru(iihru)%topo%slope))
                  usle_ls(iihru) = (terr_sl / 22.128) ** xm * (65.41 * sin_sl * sin_sl + 4.56 * sin_sl + .065)
                  usle_mult(iihru) = soil(iihru)%phys(1)%rock * hru(iihru)%sol%usle_k * terr_p * usle_ls(iihru) * 11.8
        
                  cn2(iihru) = cn2(iihru) + chg_val
                  cn2(iihru) = amin1 (cn2(iihru), 95.)
                  cn2(iihru) = amax1 (cn2(iihru), 35.)
                  call curno (cn2(iihru), iihru)
                end if
              end do
            else
              !loop through each subbasin in the region and each hru in each subbasin
            end if
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
        end do
        ! cn adjustment 
        call time_control
        ! if within uncertainty limits (in each lum) - go on to next variable
        
        end do      ! icn
          
        ! 1st esco adjustment
        do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            if (lscal(ireg)%msubs <= 0) then
              !check all hru's for proper lum
              do iihru = 1, mhru
                !set parms for 1st surface runoff calibration and rerun
                if (hru(iihru)%land_use_mgt == lscal(ireg)%lum(ilum)%lum_no) then
                  hru(iihru) = hru_init(iihru)
                  soil(iihru) = soil_init(iihru)
                  pcom(iihru) = pcom_init(iihru)
                  lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                  lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa
                  !call time_control - check if reinitializing in working
                  
                  diff = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%etr
                  chg_val = diff / 200.     ! increment esco .05 for every 10 mm difference
                  !chg_val = amin1 (chg_val, ls_prms(2)%pos)
                  !chg_val = amax1 (chg_val, ls_prms(2)%neg)
                  lscal(ireg)%lum(ilum)%prm%esco = chg_val
                  hru(iihru)%hyd%esco = hru(iihru)%hyd%esco - chg_val
                  hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, 1.)
                  hru(iihru)%hyd%esco = amax1 (hru(iihru)%hyd%esco, .01)
                end if
              end do
            else
              !loop through each subbasin in the region and each hru in each subbasin
            end if
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
        end do
        ! 1st esco adjustment 
        call time_control
        
        ! adjust et using esco
        do iesco = 1, 5
          do ireg = 1, db_mx%lscal_reg
          do ilum = 1, lscal(ireg)%lum_num
            if (lscal(ireg)%msubs <= 0) then
              !check all hru's for proper lum
              do iihru = 1, mhru
                !set parms for 1st surface runoff calibration and rerun
                if (hru(iihru)%land_use_mgt == lscal(ireg)%lum(ilum)%lum_no) then
                  hru(iihru) = hru_init(iihru)
                  soil(iihru) = soil_init(iihru)
                  pcom(iihru) = pcom_init(iihru)
                  meas = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%esco - lscal(ireg)%lum(ilum)%prm%esco)                  &
                            * (lscal(ireg)%lum(ilum)%aa%etr - meas) / (lscal(ireg)%lum(ilum)%prev%etr - meas)
                  chg_val = amin1 (chg_val, ls_prms(1)%pos)
                  chg_val = amax1 (chg_val, ls_prms(1)%neg)
                  lscal(ireg)%lum(ilum)%prm_prev%esco = lscal(ireg)%lum(ilum)%prm%esco
                  lscal(ireg)%lum(ilum)%prm%esco = lscal(ireg)%lum(ilum)%prm%esco + chg_val
                  lscal(ireg)%lum(ilum)%prev%etr = lscal(ireg)%lum(ilum)%aa%etr
                  hru(iihru)%hyd%esco = hru(iihru)%hyd%esco + chg_val
                  hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, 1.)
                  hru(iihru)%hyd%esco = amax1 (hru(iihru)%hyd%esco, .01)
                end if
              end do
            else
              !loop through each subbasin in the region and each hru in each subbasin
            end if
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end do
          end do
          ! et adjustment 
          call time_control
          ! if within uncertainty limits (in each lum) - go on to next variable
        
        end do      ! iesco
        end do      ! iter
      
	  return
      end subroutine cal_sed