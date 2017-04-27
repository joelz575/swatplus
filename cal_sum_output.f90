      subroutine cal_sum_output
    
      use sd_channel_module
      use hru_lte_module
       
        !! sum landscape output for soft data calibration
        if (cal_codes%hyd_hru == 'y' .or. cal_codes%hyd_hru == 'y') then
          !calibrate hru's
          do ireg = 1, db_mx%lsu_reg
            do ilu = 1, lscal(ireg)%lum_num
              lscal(ireg)%lum(ilu)%ha = 0.
              lscal(ireg)%lum(ilu)%precip = 0.
              lscal(ireg)%lum(ilu)%sim = lscal_z  !! zero all calibration parameters
              do ihru_s = 1, region(ireg)%num_tot
                ihru = region(ireg)%num(ihru_s)
                if (lscal(ireg)%lum(ilu)%meas%name == hru(ihru)%land_use_mgt_c) then
                  ha_hru = 10. * region(ireg)%hru_ha(ihru)      ! 10 * ha * mm --> m3
                  lscal(ireg)%lum(ilu)%ha = lscal(ireg)%lum(ilu)%ha + ha_hru
                  lscal(ireg)%lum(ilu)%precip = lscal(ireg)%lum(ilu)%precip + (10. * ha_hru * hwb_y(ihru)%precip)
                  lscal(ireg)%lum(ilu)%sim%srr = lscal(ireg)%lum(ilu)%sim%srr + (10. * ha_hru * hwb_y(ihru)%surq_gen)
                  lscal(ireg)%lum(ilu)%sim%lfr = lscal(ireg)%lum(ilu)%sim%lfr + (10. * ha_hru * hwb_y(ihru)%latq)
                  lscal(ireg)%lum(ilu)%sim%pcr = lscal(ireg)%lum(ilu)%sim%pcr + (10. * ha_hru * hwb_y(ihru)%perc)
                  lscal(ireg)%lum(ilu)%sim%etr = lscal(ireg)%lum(ilu)%sim%etr + (10. * ha_hru * hwb_y(ihru)%et)
                  lscal(ireg)%lum(ilu)%sim%tfr = lscal(ireg)%lum(ilu)%sim%tfr + (10. * ha_hru * hwb_y(ihru)%qtile)
                  lscal(ireg)%lum(ilu)%sim%sed = lscal(ireg)%lum(ilu)%sim%sed + (ha_hru * hls_y(ihru)%sedyld)
                  !add nutrients
                end if
              end do
            end do  !lum_num
            
            do ilu = 1, lscal(ireg)%lum_num
              if (lscal(ireg)%lum(ilu)%ha > 1.e-6) then
                lscal(ireg)%lum(ilu)%nbyr = lscal(ireg)%lum(ilu)%nbyr + 1
                !! convert back to mm, t/ha, kg/ha
                lscal(ireg)%lum(ilu)%precip_aa = lscal(ireg)%lum(ilu)%precip_aa + lscal(ireg)%lum(ilu)%precip / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%srr = lscal(ireg)%lum(ilu)%aa%srr + lscal(ireg)%lum(ilu)%sim%srr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%lfr = lscal(ireg)%lum(ilu)%aa%lfr + lscal(ireg)%lum(ilu)%sim%lfr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%pcr = lscal(ireg)%lum(ilu)%aa%pcr + lscal(ireg)%lum(ilu)%sim%pcr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%etr = lscal(ireg)%lum(ilu)%aa%etr + lscal(ireg)%lum(ilu)%sim%etr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%tfr = lscal(ireg)%lum(ilu)%aa%tfr + lscal(ireg)%lum(ilu)%sim%tfr / (10. * lscal(ireg)%lum(ilu)%ha)
                lscal(ireg)%lum(ilu)%aa%sed = lscal(ireg)%lum(ilu)%aa%sed + lscal(ireg)%lum(ilu)%sim%sed / lscal(ireg)%lum(ilu)%ha
                ! add nutrients
              end if
            end do
          end do    !reg
        end if
        
        if (cal_codes%hyd_hrul == 'y') then
          !calibrate hru_lte's
          do ireg = 1, db_mx%lsu_reg
            do ilu = 1, lscalt(ireg)%lum_num
              lscalt(ireg)%lum(ilu)%ha = 0.
              lscalt(ireg)%lum(ilu)%precip = 0.
              lscalt(ireg)%lum(ilu)%sim = lscal_z  !! zero all calibration parameters
              do ihru_s = 1, region(ireg)%num_tot
                ihru = region(ireg)%num(ihru_s)
                !if (lscal(ireg)%lum(ilu)%lum_no == hru(ihru)%land_use_mgt) then
                  ha_hru = 10. * region(ireg)%hru_ha(ihru)      ! 10 * ha * mm --> m3
                  lscalt(ireg)%lum(ilu)%ha = lscalt(ireg)%lum(ilu)%ha + ha_hru
                  lscalt(ireg)%lum(ilu)%precip = lscalt(ireg)%lum(ilu)%precip + (10. * ha_hru * hltwb_y(ihru)%precip)
                  lscalt(ireg)%lum(ilu)%sim%srr = lscalt(ireg)%lum(ilu)%sim%srr + (10. * ha_hru * hltwb_y(ihru)%surq_gen)
                  lscalt(ireg)%lum(ilu)%sim%lfr = lscalt(ireg)%lum(ilu)%sim%lfr + (10. * ha_hru * hltwb_y(ihru)%latq)
                  lscalt(ireg)%lum(ilu)%sim%pcr = lscalt(ireg)%lum(ilu)%sim%pcr + (10. * ha_hru * hltwb_y(ihru)%perc)
                  lscalt(ireg)%lum(ilu)%sim%etr = lscalt(ireg)%lum(ilu)%sim%etr + (10. * ha_hru * hltwb_y(ihru)%et)
                  lscalt(ireg)%lum(ilu)%sim%tfr = lscalt(ireg)%lum(ilu)%sim%tfr + (10. * ha_hru * hltwb_y(ihru)%qtile)
                  lscalt(ireg)%lum(ilu)%sim%sed = lscalt(ireg)%lum(ilu)%sim%sed + (ha_hru * hltls_y(ihru)%sedyld)
                  !add nutrients
                !end if
              end do
            end do  !lum_num
            
            do ilu = 1, lscalt(ireg)%lum_num
              if (lscalt(ireg)%lum(ilu)%ha > 1.e-6) then
                lscalt(ireg)%lum(ilu)%nbyr = lscalt(ireg)%lum(ilu)%nbyr + 1
                !! convert back to mm, t/ha, kg/ha
                lscalt(ireg)%lum(ilu)%precip_aa = lscalt(ireg)%lum(ilu)%precip_aa + lscalt(ireg)%lum(ilu)%precip / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%srr = lscalt(ireg)%lum(ilu)%aa%srr + lscalt(ireg)%lum(ilu)%sim%srr / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%lfr = lscalt(ireg)%lum(ilu)%aa%lfr + lscalt(ireg)%lum(ilu)%sim%lfr / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%pcr = lscalt(ireg)%lum(ilu)%aa%pcr + lscalt(ireg)%lum(ilu)%sim%pcr / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%etr = lscalt(ireg)%lum(ilu)%aa%etr + lscalt(ireg)%lum(ilu)%sim%etr / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%tfr = lscalt(ireg)%lum(ilu)%aa%tfr + lscalt(ireg)%lum(ilu)%sim%tfr / (10. * lscalt(ireg)%lum(ilu)%ha)
                lscalt(ireg)%lum(ilu)%aa%sed = lscalt(ireg)%lum(ilu)%aa%sed + lscalt(ireg)%lum(ilu)%sim%sed / lscalt(ireg)%lum(ilu)%ha
                ! add nutrients
              end if
            end do
          end do    !reg
        end if
          
        !! sum landscape output for plant soft data calibration
        if (cal_codes%plt == 'y') then
          !calibrate plnt growth
          do ireg = 1, db_mx%plcal_reg
            do ilu = 1, plcal(ireg)%lum_num
              plcal(ireg)%lum(ilu)%ha = 0.
              plcal(ireg)%lum(ilu)%precip = 0.
              plcal(ireg)%lum(ilu)%sim = plcal_z  !! zero all calibration parameters
              do ihru_s = 1, plcal(ireg)%num_tot
                ihru = plcal(ireg)%num(ihru_s)
                if (plcal(ireg)%lum(ilu)%meas%name == hlt(ihru)%plant) then
                  ha_hru = 10. * region(ireg)%hru_ha(ihru)      ! 10 * ha * mm --> m3
                  plcal(ireg)%lum(ilu)%ha = plcal(ireg)%lum(ilu)%ha + ha_hru
                  plcal(ireg)%lum(ilu)%precip = plcal(ireg)%lum(ilu)%precip + (10. * ha_hru * hltwb_y(ihru)%precip)
                  plcal(ireg)%lum(ilu)%sim%yield = plcal(ireg)%lum(ilu)%sim%yield + (10. * ha_hru * hlt(ihru)%yield)
                  plcal(ireg)%lum(ilu)%sim%npp = plcal(ireg)%lum(ilu)%sim%npp + (10. * ha_hru * hlt(ihru)%npp)
                  plcal(ireg)%lum(ilu)%sim%lai_mx = plcal(ireg)%lum(ilu)%sim%lai_mx + (10. * ha_hru * hlt(ihru)%lai_mx)
                  plcal(ireg)%lum(ilu)%sim%wstress = plcal(ireg)%lum(ilu)%sim%wstress + (10. * ha_hru * hltpw_y(ihru)%strsw)
                  plcal(ireg)%lum(ilu)%sim%astress = plcal(ireg)%lum(ilu)%sim%astress + (10. * ha_hru * hltpw_y(ihru)%strsa)
                  plcal(ireg)%lum(ilu)%sim%tstress = plcal(ireg)%lum(ilu)%sim%tstress + (ha_hru * hltpw_y(ihru)%strstmp)
                end if
              end do
            end do  !lum_num
            
            do ilu = 1, plcal(ireg)%lum_num
              if (plcal(ireg)%lum(ilu)%ha > 1.e-6) then
                plcal(ireg)%lum(ilu)%nbyr = plcal(ireg)%lum(ilu)%nbyr + 1
                !! convert back to mm, t/ha, kg/ha
                plcal(ireg)%lum(ilu)%precip_aa = plcal(ireg)%lum(ilu)%precip_aa + plcal(ireg)%lum(ilu)%precip / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%yield = plcal(ireg)%lum(ilu)%aa%yield + plcal(ireg)%lum(ilu)%sim%yield / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%npp = plcal(ireg)%lum(ilu)%aa%npp + plcal(ireg)%lum(ilu)%sim%npp / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%lai_mx = plcal(ireg)%lum(ilu)%aa%lai_mx + plcal(ireg)%lum(ilu)%sim%lai_mx / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%wstress = plcal(ireg)%lum(ilu)%aa%wstress + plcal(ireg)%lum(ilu)%sim%wstress / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%astress = plcal(ireg)%lum(ilu)%aa%astress + plcal(ireg)%lum(ilu)%sim%astress / (10. * plcal(ireg)%lum(ilu)%ha)
                plcal(ireg)%lum(ilu)%aa%tstress = plcal(ireg)%lum(ilu)%aa%tstress + plcal(ireg)%lum(ilu)%sim%tstress / plcal(ireg)%lum(ilu)%ha
                ! add nutrients
              end if
            end do
          end do    !reg
        end if

        !! sum channel output for soft data calibration
        if (cal_codes%chsed == 'y' .and. cal_codes%sed == 'n' .and. cal_codes%plt == 'n' .and. cal_codes%hyd_hru == 'n' .and. cal_codes%hyd_hrul == 'n') then
          do ireg = 1, db_mx%ch_reg
            do iord = 1, chcal(ireg)%ord_num
              chcal(ireg)%ord(iord)%length = 0.
              chcal(ireg)%ord(iord)%sim = chcal_z  !! zero all calibration parameters
              do ich_s = 1, chcal(ireg)%num_tot
                ich = chcal(ireg)%num(ich_s)
                if (chcal(ireg)%ord(iord)%meas%name == sd_ch(ich)%order) then
                  chcal(ireg)%ord(iord)%nbyr = chcal(ireg)%ord(iord)%nbyr + 1
                  chcal(ireg)%ord(iord)%length = chcal(ireg)%ord(iord)%length + sd_ch(ich)%chl
                  chcal(ireg)%ord(iord)%aa%chw = chcal(ireg)%ord(iord)%aa%chw + chsd_y(ich)%deg_bank_m * sd_ch(ich)%chl
                  chcal(ireg)%ord(iord)%aa%chd = chcal(ireg)%ord(iord)%aa%chd + chsd_y(ich)%deg_btm_m * sd_ch(ich)%chl
                  chcal(ireg)%ord(iord)%aa%hc = chcal(ireg)%ord(iord)%aa%hc + chsd_y(ich)%hc_m * sd_ch(ich)%chl
                  chcal(ireg)%ord(iord)%aa%fpd = chcal(ireg)%ord(iord)%aa%fpd !+ chsd_y()%dep_fp_m * sd_ch(ich)%chl
                end if
              end do
            end do
            !average the channel data by length
            do iord = 1, chcal(ireg)%ord_num
              if (chcal(ireg)%ord(iord)%nbyr > 0) then
                !! convert back to mm, t/ha, kg/ha
                if (chcal(ireg)%ord(iord)%length > 1.e-6) then
                  chcal(ireg)%ord(iord)%aa%chd = chcal(ireg)%ord(iord)%aa%chd / chcal(ireg)%ord(iord)%length
                  chcal(ireg)%ord(iord)%aa%chw = chcal(ireg)%ord(iord)%aa%chw / chcal(ireg)%ord(iord)%length
                  chcal(ireg)%ord(iord)%aa%hc = chcal(ireg)%ord(iord)%aa%hc / chcal(ireg)%ord(iord)%length
                  chcal(ireg)%ord(iord)%aa%fpd = chcal(ireg)%ord(iord)%aa%fpd / chcal(ireg)%ord(iord)%length
                end if
              end if
            end do
          end do    !reg
        end if

      return
 1234 format (1x,' Executing year/day ', 2i4)
      
      end subroutine cal_sum_output