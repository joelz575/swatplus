      subroutine cal_chsed

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

      !calibrate sediment
        ical_sed = 0
        
        ! 1st cover adjustment for channel widening
        do ireg = 1, db_mx%chcal_reg
          do iord = 1, chcal(ireg)%ord_num
              do ich_s = 1, chcal(ireg)%num_tot
                iich = chcal(ireg)%num(ich_s)
                !set parms for 1st surface runoff calibration and rerun
                sd_ch(iich) = sdch_init(iich)
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                chg_val = chcal(ireg)%ord(iord)%meas%chw / chcal(ireg)%ord(iord)%aa%chw
                chg_val = chg_val
                chg_val = amin1 (chg_val, ch_prms(1)%pos)
                chg_val = Max (chg_val, ch_prms(1)%neg)
                chcal(ireg)%ord(iord)%prm%cov = chg_val
                if (chg_val > .001) then
                sd_ch(iich)%cov = sd_ch(iich)%cov / chg_val
                end if
              end do
            chcal(ireg)%ord(iord)%aa = chcal_z
          end do
        end do
        ! 1st cover adjustment 
        call time_control
        
        do iter = 1, 2
          ! further cover adjustment for channel widening
          do isl = 1, 2
          do ireg = 1, db_mx%chcal_reg
          do iord = 1, chcal(ireg)%ord_num
              do ich_s = 1, chcal(ireg)%num_tot
                iich = chcal(ireg)%num(ich_s)
                !set parms for 1st surface runoff calibration and rerun
                  sd_ch(iich) = sdch_init(iich)
                  meas = chcal(ireg)%ord(iord)%meas%chw
                  chg_val = - (chcal(ireg)%ord(iord)%prm_prev%cov - chcal(ireg)%ord(iord)%prm%cov)                  &
                            * (chcal(ireg)%ord(iord)%aa%chw - meas) / (chcal(ireg)%ord(iord)%prev%chw - meas)
                  chg_val = amin1 (chg_val, ls_prms(1)%pos)
                  chg_val = Max (chg_val, ls_prms(1)%neg)
                  chcal(ireg)%ord(iord)%prm_prev%cov = chcal(ireg)%ord(iord)%prm%cov
                  chcal(ireg)%ord(iord)%prm%cov = chcal(ireg)%ord(iord)%prm%cov + chg_val
                  chcal(ireg)%ord(iord)%prev%chw = chcal(ireg)%ord(iord)%aa%chw
                  
                  if (chg_val > .001) then
                  sd_ch(iich)%cov = sd_ch(iich)%cov / chg_val
                  end if
              end do
            chcal(ireg)%ord(iord)%aa = chcal_z
          end do
        end do
        ! cover adjustment 
        call time_control
        ! if within uncertainty limits (in each ord) - go on to next variable
        
        end do
          
        ! 1st erodibility adjustment for channel downcutting
        do ireg = 1, db_mx%chcal_reg
          do iord = 1, chcal(ireg)%ord_num
              do ich_s = 1, chcal(ireg)%num_tot
                iich = chcal(ireg)%num(ich_s)
                !set parms for 1st surface runoff calibration and rerun
                sd_ch(iich) = sdch_init(iich)
                chcal(ireg)%ord(iord)%prm_prev = chcal(ireg)%ord(iord)%prm
                chcal(ireg)%ord(iord)%prev = chcal(ireg)%ord(iord)%aa

                chg_val = chcal(ireg)%ord(iord)%meas%chd / chcal(ireg)%ord(iord)%aa%chd
                chg_val = chg_val
                chg_val = amin1 (chg_val, ch_prms(2)%pos)
                chg_val = Max (chg_val, ch_prms(2)%neg)
                chcal(ireg)%ord(iord)%prm%erod = chg_val
                if (chg_val > .001) then
                  sd_ch(iich)%cherod = sd_ch(iich)%cherod / chg_val
                end if
              end do
            chcal(ireg)%ord(iord)%aa = chcal_z
          end do
        end do
        ! 1st erod adjustment 
        call time_control
        
        ! further cover adjustment for channel widening
        do isl = 1, 2
          do ireg = 1, db_mx%chcal_reg
            do iord = 1, chcal(ireg)%ord_num
              do ich_s = 1, chcal(ireg)%num_tot
                iich = chcal(ireg)%num(ich_s)
                !set parms for 1st surface runoff calibration and rerun
                  sd_ch(iich) = sdch_init(iich)
                  meas = chcal(ireg)%ord(iord)%meas%chd
                  chg_val = - (chcal(ireg)%ord(iord)%prm_prev%erod - chcal(ireg)%ord(iord)%prm%erod)                  &
                            * (chcal(ireg)%ord(iord)%aa%chd - meas) / (chcal(ireg)%ord(iord)%prev%chd - meas)
                  chg_val = amin1 (chg_val, ls_prms(2)%pos)
                  chg_val = Max (chg_val, ls_prms(2)%neg)
                  chcal(ireg)%ord(iord)%prm_prev%erod = chcal(ireg)%ord(iord)%prm%erod
                  chcal(ireg)%ord(iord)%prm%erod = chcal(ireg)%ord(iord)%prm%erod + chg_val
                  chcal(ireg)%ord(iord)%prev%chd = chcal(ireg)%ord(iord)%aa%chd
                  
                  if (chg_val > .001) then
                    sd_ch(iich)%cherod = sd_ch(iich)%cherod / chg_val
                  end if
              end do
              chcal(ireg)%ord(iord)%aa = chcal_z
            end do
          end do
        ! channel downcutting adjustment 
        call time_control
        end do

        end do      ! iter
      
	  return
      end subroutine cal_chsed