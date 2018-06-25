      subroutine cal_hyd

      use hru_module, only : cn2, hru, hru_init
      use soil_module
      use plant_module
      use hydrograph_module
      use ru_module
      use aquifer_module
      use channel_module
      use hru_lte_module
      use sd_channel_module
      use basin_module
      use maximum_data_module
      use calibration_data_module
      use conditional_module
      use reservoir_module
      use organic_mineral_mass_module
      use time_module
      
      implicit none
      
      integer :: iter_all      !none      |counter
      integer :: iterall       !none      |counter
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: icn           !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          |end of loop
      integer :: ietco         !none      |counter
      integer :: ik            !none      |counter
      integer :: nly           !          |end of loop
      integer :: iperco        !none      |counter
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          | 
      real :: dep_below_soil   !          |     
      
         
      

      !calibrate hydrology
        ical_hyd = 0
        iter_all = 1
        iter_ind = 1

      do iterall = 1, iter_all
        ! 1st cn2 adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%srr
                chg_val = diff / 15.     !assume 10 mm runoff for 1 cn
                lscal(ireg)%lum(ilum)%prm_prev%cn = lscal(ireg)%lum(ilum)%prm%cn
                lscal(ireg)%lum(ilum)%prm%cn = lscal(ireg)%lum(ilum)%prm%cn + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                
                if (lscal(ireg)%lum(ilum)%prm%cn >= ls_prms(1)%pos) then
                  chg_val = ls_prms(1)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn <= ls_prms(1)%neg) then
                  chg_val = ls_prms(1)%neg - lscal(ireg)%lum(ilum)%prm_prev%cn
                  lscal(ireg)%lum(ilum)%prm%cn = ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn = 1.
                end if

            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st surface runoff calibration and rerun
                call hru_re_initialize (iihru)
                cn2(iihru) = cn2(iihru) + chg_val
                cn2(iihru) = amin1 (cn2(iihru), ls_prms(1)%up)
                cn2(iihru) = Max (cn2(iihru), ls_prms(1)%lo)
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! 1st cn2 adjustment 
        if (isim > 0) then
          cal_sim = " first cn2 adj "
          call time_control
        end if

          ! adjust surface runoff using cn2
          do icn = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn < 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%srr - lscal(ireg)%lum(ilum)%aa%srr
                if (abs(denom) > 1.e-6) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%cn - lscal(ireg)%lum(ilum)%prm%cn)                  &
                    * (lscal(ireg)%lum(ilum)%aa%srr - rmeas) / denom
                else
                  chg_val = diff / 200.
                end if
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
            
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st surface runoff calibration and rerun
                call hru_re_initialize (iihru)
                cn2(iihru) = cn2(iihru) + chg_val
                cn2(iihru) = amin1 (cn2(iihru), ls_prms(1)%up)
                cn2(iihru) = Max (cn2(iihru), ls_prms(1)%lo)
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            !else
            !lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
            end if
          end do
        end do
          
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! cn2 adjustment
        if (isim > 0) then
          cal_sim =  " cn2 adj "
          call time_control
        end if
        end do      ! icn
          
        ! 1st esco adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%etr) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%etr
                chg_val = - diff / 200.     ! increment etco .05 for every 10 mm difference
                lscal(ireg)%lum(ilum)%prm_prev%etco = lscal(ireg)%lum(ilum)%prm%etco
                lscal(ireg)%lum(ilum)%prm%etco = lscal(ireg)%lum(ilum)%prm%etco + chg_val
                lscal(ireg)%lum(ilum)%prev%etr = lscal(ireg)%lum(ilum)%aa%etr
                
                if (lscal(ireg)%lum(ilum)%prm%etco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%etco
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%etco <= ls_prms(2)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%etco - ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                           
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st et calibration
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco + chg_val
                hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, ls_prms(2)%up)
                hru(iihru)%hyd%esco = Max (hru(iihru)%hyd%esco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
                hru(iihru)%hyd%epco = hru(iihru)%hyd%epco - chg_val
                hru(iihru)%hyd%epco = amin1 (hru(iihru)%hyd%epco, ls_prms(2)%up)
                hru(iihru)%hyd%epco = Max (hru(iihru)%hyd%epco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%epco = hru(iihru)%hyd%epco
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
        do iihru = 1, sp_ob%hru
          call hru_re_initialize (iihru)
        end do
        ! 1st esco adjustment 
        if (isim > 0) then
          cal_sim =  " first esco adj "
          call time_control
        end if
        
        ! adjust et using esco
        do ietco = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%etr) / soft)
            if (diff > .01 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%etco < 1.e-6) then
            isim = 1

                rmeas = lscal(ireg)%lum(ilum)%meas%etr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%etr - lscal(ireg)%lum(ilum)%aa%etr
                if (abs(denom) > 1.e-6) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%etco - lscal(ireg)%lum(ilum)%prm%etco)                  &
                    * (lscal(ireg)%lum(ilum)%aa%etr - rmeas) / denom
                else
                  chg_val = diff / 200.
                end if
                lscal(ireg)%lum(ilum)%prm_prev%etco = lscal(ireg)%lum(ilum)%prm%etco
                lscal(ireg)%lum(ilum)%prm%etco = lscal(ireg)%lum(ilum)%prm%etco + chg_val
                lscal(ireg)%lum(ilum)%prev%etr = lscal(ireg)%lum(ilum)%aa%etr
                      
                if (lscal(ireg)%lum(ilum)%prm%etco >= ls_prms(2)%pos) then
                  chg_val = ls_prms(2)%pos - lscal(ireg)%lum(ilum)%prm_prev%etco
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%etco <= ls_prms(2)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%etco - ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm%etco = ls_prms(2)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%etco = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
                iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for et calibration
                hru(iihru)%hyd%esco = hru(iihru)%hyd%esco + chg_val
                hru(iihru)%hyd%esco = amin1 (hru(iihru)%hyd%esco, ls_prms(2)%up)
                hru(iihru)%hyd%esco = Max (hru(iihru)%hyd%esco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%esco = hru(iihru)%hyd%esco
                hru(iihru)%hyd%epco = hru(iihru)%hyd%epco - chg_val
                hru(iihru)%hyd%epco = amin1 (hru(iihru)%hyd%epco, ls_prms(2)%up)
                hru(iihru)%hyd%epco = Max (hru(iihru)%hyd%epco, ls_prms(2)%lo)
                hru_init(iihru)%hyd%epco = hru(iihru)%hyd%epco
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
          end do
          
          ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
          !zero plant calibration data in case plants are calibrated
          do ireg = 1, db_mx%plcal_reg
            do ilum = 1, plcal(ireg)%lum_num
              plcal(ireg)%lum(ilum)%nbyr = 0
              plcal(ireg)%lum(ilum)%precip_aa = 0.
              plcal(ireg)%lum(ilum)%aa = plcal_z
            end do
          end do
          ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
          ! et adjustment 
          if (isim > 0) then
            cal_sim =  " esco adj "
            call time_control
          end if
        
        end do      ! iesco

        ! 1st lat_len adjustment for lateral soil flow
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .1 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%lat_len < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%lfr
                chg_val = - diff        ! increment lst_len by 1 m for 1 mm difference
                lscal(ireg)%lum(ilum)%prm_prev%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len
                lscal(ireg)%lum(ilum)%prm%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len + chg_val
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                           
                if (lscal(ireg)%lum(ilum)%prm%lat_len >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%pos
                  chg_val = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%lat_len <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%neg
                  chg_val = ls_prms(3)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                hru(iihru)%topo%lat_len = hru(iihru)%topo%lat_len + chg_val * 2.
                hru(iihru)%topo%lat_len = amin1 (hru(iihru)%topo%lat_len, ls_prms(3)%up)
                hru(iihru)%topo%lat_len = Max (hru(iihru)%topo%lat_len, ls_prms(3)%lo)
                hru_init(iihru)%topo%lat_len = hru(iihru)%topo%lat_len
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
        ! 1st lat_len adjustment 
        if (isim > 0) then
          cal_sim =  " first lat_len adj "
          call time_control
        end if

        ! adjust lat_len adjustment for lateral soil flow
        do ik = 1, iter_ind
          isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%lfr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%lat_len < 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%lfr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%lfr - lscal(ireg)%lum(ilum)%aa%lfr
                if (abs(denom) > 1.e-6) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%lat_len - lscal(ireg)%lum(ilum)%prm%lat_len)                  &
                    * (lscal(ireg)%lum(ilum)%aa%lfr - rmeas) / denom
                else
                  chg_val = - diff    ! 1 m for each mm
                end if
                lscal(ireg)%lum(ilum)%prm_prev%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len
                lscal(ireg)%lum(ilum)%prm%lat_len = lscal(ireg)%lum(ilum)%prm%lat_len + chg_val
                lscal(ireg)%lum(ilum)%prev%lfr = lscal(ireg)%lum(ilum)%aa%lfr
                           
                if (lscal(ireg)%lum(ilum)%prm%lat_len >= ls_prms(3)%pos) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%pos
                  chg_val = ls_prms(3)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%lat_len <= ls_prms(3)%neg) then
                  lscal(ireg)%lum(ilum)%prm%lat_len = ls_prms(3)%neg
                  chg_val = ls_prms(3)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%lat_len = 1.
                end if
                
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                hru(iihru)%topo%lat_len = hru(iihru)%topo%lat_len + chg_val
                hru(iihru)%topo%lat_len = amin1 (hru(iihru)%topo%lat_len, ls_prms(3)%up)
                hru(iihru)%topo%lat_len = Max (hru(iihru)%topo%lat_len, ls_prms(3)%lo)
                hru_init(iihru)%topo%lat_len = hru(iihru)%topo%lat_len
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
        ! lat_len adjustment for lateral soil flow
        if (isim > 0) then
          cal_sim =  " lat_len adj "
          call time_control
        end if
        end do  
          
        ! 1st dep_imp for percolation
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            !check all hru"s for proper lum
            soft = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%pcr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%perco < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                chg_val = soft / lscal(ireg)%lum(ilum)%aa%pcr      ! linear relationship
                lscal(ireg)%lum(ilum)%prm_prev%perco = lscal(ireg)%lum(ilum)%prm%perco 
                lscal(ireg)%lum(ilum)%prm%perco = lscal(ireg)%lum(ilum)%prm%perco * chg_val
                lscal(ireg)%lum(ilum)%prev%pcr = lscal(ireg)%lum(ilum)%aa%pcr
                           
                !if (lscal(ireg)%lum(ilum)%prm%perco >= ls_prms(8)%pos) then
                !  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%pos
                !  chg_val = ls_prms(8)%pos
                !  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                !end if
                !if (lscal(ireg)%lum(ilum)%prm%perco <= ls_prms(8)%neg) then
                !  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%neg
                !  chg_val = ls_prms(8)%neg
                !  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                !end if

            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                nly = soil(iihru)%nly
                dep_below_soil = hru(iihru)%hyd%dep_imp - soil(iihru)%phys(nly)%d
                if (soft < 1.e-6 .or. dep_below_soil < 1.e-6) then
                  dep_below_soil = 0.
                else
                  if (chg_val < .9) then
                    !perc too high
                    if (dep_below_soil > 51.) then
                      dep_below_soil = 50.
                    else
                      !dep_below_soil = 0.8 * chg_val * dep_below_soil
                      dep_below_soil = chg_val * dep_below_soil
                    end if
                  end if
                  if (chg_val > 1.1) then
                    !perc too low
                    if (dep_below_soil > 51.) then
                      dep_below_soil = 50.
                    else
                      dep_below_soil =  dep_below_soil / (0.8 * chg_val)
                      if (dep_below_soil > 50.) dep_below_soil = 50.
                    end if
                  end if
                end if
                dep_below_soil = amin1 (dep_below_soil, ls_prms(8)%up)
                dep_below_soil = Max (dep_below_soil, ls_prms(8)%lo)
                hru(iihru)%hyd%dep_imp = soil(iihru)%phys(nly)%d + dep_below_soil
                hru_init(iihru)%hyd%dep_imp = hru(iihru)%hyd%dep_imp
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            else
            lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
            end if
            end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
        ! 1st perco adjustment 
        if (isim > 0) then
          cal_sim =  " first perco adj "
          call time_control
        end if

          ! adjust percolation using dep_imp
          do iperco = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%pcr) / soft)
            if (diff > .05 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%perco < 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%pcr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%pcr - lscal(ireg)%lum(ilum)%aa%pcr
                chg_val = (soft - lscal(ireg)%lum(ilum)%aa%pcr) / soft
                
                !if (abs(denom) > 1.e-6) then
                !  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%perco - lscal(ireg)%lum(ilum)%prm%perco) *         &
                !        (lscal(ireg)%lum(ilum)%aa%pcr - rmeas) / denom
                !else
                !  chg_val = (soft - lscal(ireg)%lum(ilum)%aa%pcr) / 200. 
                !end if
                lscal(ireg)%lum(ilum)%prm%perco = lscal(ireg)%lum(ilum)%prm%perco + chg_val
                lscal(ireg)%lum(ilum)%prm_prev%perco = lscal(ireg)%lum(ilum)%prm%perco
                lscal(ireg)%lum(ilum)%prev%pcr = lscal(ireg)%lum(ilum)%aa%pcr
                                
                !if (lscal(ireg)%lum(ilum)%prm%perco >= ls_prms(8)%pos) then
                !  chg_val = lscal(ireg)%lum(ilum)%prm_prev%perco - ls_prms(8)%pos
                !  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%pos
                !  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                !end if
                !if (lscal(ireg)%lum(ilum)%prm%perco <= ls_prms(8)%neg) then
                !  chg_val = lscal(ireg)%lum(ilum)%prm_prev%perco - ls_prms(8)%neg
                !  lscal(ireg)%lum(ilum)%prm%perco = ls_prms(8)%neg
                !  lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
                !end if

            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                nly = soil(iihru)%nly
                dep_below_soil = hru(iihru)%hyd%dep_imp - soil(iihru)%phys(nly)%d
                if (soft < 1.e-6 .or. dep_below_soil < 1.e-6) then
                  dep_below_soil = 0.
                end if
                if (chg_val > .01) then
                  !perc too low
                  !dep_below_soil = (.05 / chg_val) * dep_below_soil
                  dep_below_soil = dep_below_soil / chg_val
                end if
                if (chg_val < -.01) then
                  !perc too high
                  dep_below_soil = exp(chg_val) * dep_below_soil
                end if

                dep_below_soil = amin1 (dep_below_soil, ls_prms(8)%up)
                dep_below_soil = Max (dep_below_soil, ls_prms(8)%lo)
                hru(iihru)%hyd%dep_imp = soil(iihru)%phys(nly)%d + dep_below_soil
                hru_init(iihru)%hyd%dep_imp = hru(iihru)%hyd%dep_imp
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            else
            lscal(ireg)%lum(ilum)%prm_lim%perco = 1.
            end if
          end do
          end do
          
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
        ! perco adjustment 
        if (isim > 0) then
          cal_sim =  " perco adj "
          call time_control
        end if
        
        end do      ! iperco  

        ! 1st cn3_swf adjustment
        isim = 0
        do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn3_swf < 1.e-6) then
            isim = 1
            
                lscal(ireg)%lum(ilum)%prm_prev = lscal(ireg)%lum(ilum)%prm
                lscal(ireg)%lum(ilum)%prev = lscal(ireg)%lum(ilum)%aa

                diff = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa - lscal(ireg)%lum(ilum)%aa%srr
                !chg_val = - diff / 300.     !assume 10 mm runoff for .3 cn3_swf
                chg_val = - diff / 700.     !assume 10 mm runoff for .7 cn3_swf
                lscal(ireg)%lum(ilum)%prm_prev%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf
                lscal(ireg)%lum(ilum)%prm%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf >= ls_prms(10)%pos) then
                  chg_val = ls_prms(10)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf <= ls_prms(10)%neg) then
                  chg_val = ls_prms(10)%neg - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(10)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if

            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                hru(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf + chg_val
                hru(iihru)%hyd%cn3_swf = amin1 (hru(iihru)%hyd%cn3_swf, ls_prms(10)%up)
                hru(iihru)%hyd%cn3_swf = Max (hru(iihru)%hyd%cn3_swf, ls_prms(10)%lo)
                hru_init(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
          end if
          end do
        end do
        
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
          
        ! 1st cn3_swf adjustment 
        if (isim > 0) then
          cal_sim =  " first cn3_swf adj "
          call time_control
        end if

          ! adjust surface runoff using cn3_swf
          do icn = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%lsu_reg
          do ilum = 1, region(ireg)%nlum
            soft = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - lscal(ireg)%lum(ilum)%aa%srr) / soft)
            if (diff > .02 .and. lscal(ireg)%lum(ilum)%ha > 1.e-6 .and. lscal(ireg)%lum(ilum)%prm_lim%cn3_swf < 1.e-6) then
            isim = 1
            
                rmeas = lscal(ireg)%lum(ilum)%meas%srr * lscal(ireg)%lum(ilum)%precip_aa
                denom = lscal(ireg)%lum(ilum)%prev%srr - lscal(ireg)%lum(ilum)%aa%srr
                if (abs(denom) > 1.e-6) then
                  chg_val = - (lscal(ireg)%lum(ilum)%prm_prev%cn3_swf - lscal(ireg)%lum(ilum)%prm%cn3_swf)                  &
                    * (lscal(ireg)%lum(ilum)%aa%srr - rmeas) / denom
                else
                  chg_val = - diff / 300.
                end if
                lscal(ireg)%lum(ilum)%prm_prev%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf
                lscal(ireg)%lum(ilum)%prm%cn3_swf = lscal(ireg)%lum(ilum)%prm%cn3_swf + chg_val
                lscal(ireg)%lum(ilum)%prev%srr = lscal(ireg)%lum(ilum)%aa%srr
                                
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf >= ls_prms(1)%pos) then
                  chg_val = ls_prms(1)%pos - lscal(ireg)%lum(ilum)%prm_prev%cn3_swf
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(1)%pos
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
                if (lscal(ireg)%lum(ilum)%prm%cn3_swf <= ls_prms(1)%neg) then
                  chg_val = lscal(ireg)%lum(ilum)%prm_prev%cn3_swf - ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm%cn3_swf = ls_prms(1)%neg
                  lscal(ireg)%lum(ilum)%prm_lim%cn3_swf = 1.
                end if
            
            !check all hru"s for proper lum
            do ihru_s = 1, region(ireg)%num_tot
              iihru = region(ireg)%num(ihru_s)
              if (lscal(ireg)%lum(ilum)%meas%name == hru(iihru)%lum_group_c) then
                !set parms for 1st perco calibration
                hru(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf + chg_val
                hru(iihru)%hyd%cn3_swf = amin1 (hru(iihru)%hyd%cn3_swf, ls_prms(10)%up)
                hru(iihru)%hyd%cn3_swf = Max (hru(iihru)%hyd%cn3_swf, ls_prms(10)%lo)
                hru_init(iihru)%hyd%cn3_swf = hru(iihru)%hyd%cn3_swf
                call curno (cn2(iihru), iihru)
              end if
            end do
            lscal(ireg)%lum(ilum)%nbyr = 0
            lscal(ireg)%lum(ilum)%precip_aa = 0.
            lscal(ireg)%lum(ilum)%aa = lscal_z
            end if
          end do
        end do
          
        !zero plant calibration data in case plants are calibrated
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
          end do
        end do
        ! re-initialize all hru data
          do iihru = 1, sp_ob%hru
            call hru_re_initialize (iihru)
          end do
        ! cn3_swf adjustment
        if (isim > 0) then
          cal_sim =  " cn3_swf adj "
          call time_control
        end if
        end do      ! icn
          
      end do    ! iter_all loop
        
      cal_codes%hyd_hru = "n"
      
	  return
      end subroutine cal_hyd