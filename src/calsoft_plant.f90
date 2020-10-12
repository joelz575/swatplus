      subroutine calsoft_plant

      use hru_module, only : ihru 
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
      use plant_module
      
      implicit none
      
      integer :: iter_all      !          |end of loop
      integer :: iterall       !none      |counter
      integer :: isim          !          |
      integer :: ireg          !none      |counter
      integer :: ilum          !none      |counter
      integer :: iihru         !none      |counter
      integer :: ihru_s        !none      |counter
      integer :: iter_ind      !          !end of loop
      integer :: ist           !          |
      integer :: ipl           !none      |counter for plants in the hru
      integer :: nvar          !          |number of plant cal variables (1=lai_pot, 2=harv_idx)
      real :: rmeas            !          |
      real :: denom            !          |
      real :: soft             !          |
      real :: diff             !          |
      real :: chg_val          !          | 
      
      !calibrate crop yields
        iter_all = 1
        iter_ind = 1
        nvar = 2
        
      do iterall = 1, iter_all
        ! 1st lai potential adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .02 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6 .and. plcal(ireg)%lum(ilum)%prm_lim%lai_pot < 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (soft - plcal(ireg)%lum(ilum)%aa%yield) / soft
                chg_val =  diff * 5.     !assume 1 lai for every 20% difference in yield
                plcal(ireg)%lum(ilum)%prm_prev%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot
                plcal(ireg)%lum(ilum)%prm%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%lai_pot >= pl_prms(ireg)%prm(ilum)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum)%pos - plcal(ireg)%lum(ilum)%prm_prev%lai_pot
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%lai_pot <= pl_prms(ireg)%prm(ilum)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum)%neg - plcal(ireg)%lum(ilum)%prm_prev%lai_pot
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%plg(ipl)%cpnm) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot + chg_val
                  pcom(iihru)%plcur(ipl)%lai_pot = amin1 (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum)%up)
                  pcom(iihru)%plcur(ipl)%lai_pot = Max (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum)%lo)
                  pcom_init(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot
                end if
              end do
            end do
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
            plcal(ireg)%lum(ilum)%sim%yield = 0.
            plcal(ireg)%lum(ilum)%ha = 0.
            
          end if
          end do
        end do
        ! 1st lai potential adj
        if (isim > 0) then
          cal_sim =  " first lai potential adj "
          call time_control
        end if

          ! adjust plant growth using potential lai
          do ist = 1, iter_ind
          isim = 0
          do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .02 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6 .and. plcal(ireg)%lum(ilum)%prm_lim%lai_pot < 1.e-6) then
            isim = 1
            
                rmeas = plcal(ireg)%lum(ilum)%meas%yield
                denom = plcal(ireg)%lum(ilum)%prev%yield - plcal(ireg)%lum(ilum)%aa%yield
                if (abs(denom) > 1.e-6) then
                  chg_val = - (plcal(ireg)%lum(ilum)%prm_prev%lai_pot - plcal(ireg)%lum(ilum)%prm%lai_pot) *                &
                              (plcal(ireg)%lum(ilum)%aa%yield - rmeas) / denom
                else
                  chg_val = diff / 5.     !assume 1 lai for every 20% difference in yield
                end if
                plcal(ireg)%lum(ilum)%prm_prev%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot
                plcal(ireg)%lum(ilum)%prm%lai_pot = plcal(ireg)%lum(ilum)%prm%lai_pot + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                                
                if (plcal(ireg)%lum(ilum)%prm%lai_pot >= pl_prms(ireg)%prm(ilum)%pos) then
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%lai_pot <= pl_prms(ireg)%prm(ilum)%neg) then
                  plcal(ireg)%lum(ilum)%prm%lai_pot = pl_prms(ireg)%prm(ilum)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%lai_pot = 1.
                end if
            
            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%plg(ipl)%cpnm) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot + chg_val
                  pcom(iihru)%plcur(ipl)%lai_pot = amin1 (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum)%up)
                  pcom(iihru)%plcur(ipl)%lai_pot = Max (pcom(iihru)%plcur(ipl)%lai_pot, pl_prms(ireg)%prm(ilum)%lo)
                  pcom_init(iihru)%plcur(ipl)%lai_pot = pcom(iihru)%plcur(ipl)%lai_pot
                end if
              end do
            end do
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
            plcal(ireg)%lum(ilum)%sim%yield = 0.
            plcal(ireg)%lum(ilum)%ha = 0.

            end if
          end do
        end do
        ! plant potential lai adjustment
        if (isim > 0) then
          cal_sim =  " lai potential adj "
          call time_control
        end if
        end do      ! ist
          
          
        ! 1st plant harvest index adjustment
        isim = 0
        do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .02 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6 .and. plcal(ireg)%lum(ilum)%prm_lim%harv_idx < 1.e-6) then
            isim = 1
            
                plcal(ireg)%lum(ilum)%prm_prev = plcal(ireg)%lum(ilum)%prm
                plcal(ireg)%lum(ilum)%prev = plcal(ireg)%lum(ilum)%aa

                diff = (plcal(ireg)%lum(ilum)%meas%yield - plcal(ireg)%lum(ilum)%aa%yield) / plcal(ireg)%lum(ilum)%meas%yield
                chg_val = diff / 4.     !assume frac diff over 4.
                plcal(ireg)%lum(ilum)%prm_prev%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx
                plcal(ireg)%lum(ilum)%prm%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                
                if (plcal(ireg)%lum(ilum)%prm%harv_idx >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%harv_idx <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%plg(ipl)%cpnm) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx + chg_val
                  pcom(iihru)%plcur(ipl)%harv_idx = amin1 (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%harv_idx = Max (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx
                end if
              end do
            end do
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
            plcal(ireg)%lum(ilum)%sim%yield = 0.
            plcal(ireg)%lum(ilum)%ha = 0.
            
          end if
          end do
        end do
        ! 1st plant harvest index adjustment 
        if (isim > 0) then
          cal_sim = " first harvest index adj "
          call time_control
        end if

          ! adjust plant growth using harvest index parameter
          do ist = 1, 2     !iter_ind
          isim = 0
          do ireg = 1, db_mx%plcal_reg
          do ilum = 1, plcal(ireg)%lum_num
            soft = plcal(ireg)%lum(ilum)%meas%yield
            diff = 0.
            if (soft > 1.e-6) diff = abs((soft - plcal(ireg)%lum(ilum)%aa%yield) / soft)
            if (diff > .02 .and. plcal(ireg)%lum(ilum)%ha > 1.e-6 .and. plcal(ireg)%lum(ilum)%prm_lim%harv_idx < 1.e-6) then
            isim = 1
            
                rmeas = plcal(ireg)%lum(ilum)%meas%yield
                denom = plcal(ireg)%lum(ilum)%prev%yield - plcal(ireg)%lum(ilum)%aa%yield
                if (abs(denom) > 1.e-6) then
                  chg_val = - (plcal(ireg)%lum(ilum)%prm_prev%harv_idx - plcal(ireg)%lum(ilum)%prm%harv_idx) *                &
                              (plcal(ireg)%lum(ilum)%aa%yield - rmeas) / denom
                else
                  chg_val = - diff / 4.
                end if
                plcal(ireg)%lum(ilum)%prm_prev%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx
                plcal(ireg)%lum(ilum)%prm%harv_idx = plcal(ireg)%lum(ilum)%prm%harv_idx + chg_val
                plcal(ireg)%lum(ilum)%prev%yield = plcal(ireg)%lum(ilum)%aa%yield
                                
                if (plcal(ireg)%lum(ilum)%prm%harv_idx >= pl_prms(ireg)%prm(ilum+nvar)%pos) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%pos - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%pos
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if
                if (plcal(ireg)%lum(ilum)%prm%harv_idx <= pl_prms(ireg)%prm(ilum+nvar)%neg) then
                  chg_val = pl_prms(ireg)%prm(ilum+nvar)%neg - plcal(ireg)%lum(ilum)%prm_prev%harv_idx
                  plcal(ireg)%lum(ilum)%prm%harv_idx = pl_prms(ireg)%prm(ilum+nvar)%neg
                  plcal(ireg)%lum(ilum)%prm_lim%harv_idx = 1.
                end if

            !! re-initialize all objects
            call re_initialize

            do ihru_s = 1, plcal(ireg)%num_tot
              iihru = plcal(ireg)%num(ihru_s)
              do ipl = 1, pcom(iihru)%npl
                if (plcal(ireg)%lum(ilum)%meas%name == pcom(iihru)%plg(ipl)%cpnm) then
                  !set potential lai parm
                  pcom(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx + chg_val
                  pcom(iihru)%plcur(ipl)%harv_idx = amin1 (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%up)
                  pcom(iihru)%plcur(ipl)%harv_idx = Max (pcom(iihru)%plcur(ipl)%harv_idx, pl_prms(ireg)%prm(ilum+nvar)%lo)
                  pcom_init(iihru)%plcur(ipl)%harv_idx = pcom(iihru)%plcur(ipl)%harv_idx
                end if
              end do
            end do
            plcal(ireg)%lum(ilum)%nbyr = 0
            plcal(ireg)%lum(ilum)%precip_aa = 0.
            plcal(ireg)%lum(ilum)%aa = plcal_z
            plcal(ireg)%lum(ilum)%sim%yield = 0.
            plcal(ireg)%lum(ilum)%ha = 0.

            end if
          end do
        end do
        ! plant harvest index adjustment
        if (isim > 0) then
          cal_sim = " harvest index adj "
          call time_control
        end if
        end do      ! ist
          
      end do    ! iter_all loop

	  return
      end subroutine calsoft_plant
