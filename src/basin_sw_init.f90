      subroutine basin_sw_init

      use time_module
      use hydrograph_module
      use calibration_data_module
      use output_landscape_module
      use basin_module
      use maximum_data_module

      implicit none

      integer :: ihru     !none      |counter
      integer :: iihru    !          |
      integer :: ilsu     !none      |counter
      integer :: ielem    !          |
      real :: const       !          |constant used for rate, days, etc

        ! summing subbasin output for the basin
        do ihru = 1, sp_ob%hru
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hru") then
              bwb_d%sw_init = bwb_d%sw_init + hwb_d(iihru)%sw_init * const
              bwb_d%sw_final = bwb_d%sw_final + hwb_d(iihru)%sw_final * const
            end if
          end if
        end do
        bwb_m%sw_init = bwb_d%sw_init
        bwb_m%sw_final = bwb_d%sw_final
        bwb_y%sw_init = bwb_d%sw_init
        bwb_y%sw_final = bwb_d%sw_final
        bwb_a%sw_init = bwb_d%sw_init
        bwb_a%sw_final = bwb_d%sw_final

        ! or if it is not routed and not in a subbasin
        do ihru = 1, sp_ob%hru_lte
          iihru = lsu_elem(ihru)%obtypno
          if (lsu_elem(iihru)%bsn_frac > 1.e-12) then
            const = lsu_elem(iihru)%bsn_frac
            if (lsu_elem(iihru)%obtyp == "hlt") then
              !const = lsu_elem(iihru)%bsn_frac
              bwb_d%sw_init = bwb_d%sw_init + hltwb_d(iihru)%sw_init * const
              bwb_d%sw_final = bwb_d%sw_final + hltwb_d(iihru)%sw_final * const
            end if
          end if
        end do

      do ilsu = 1, db_mx%lsu_out
        ! summing HRU output for the landscape unit(
        do ielem = 1, lsu_out(ilsu)%num_tot
          ihru = lsu_out(ilsu)%num(ielem)
          if (lsu_elem(ihru)%ru_frac > 1.e-9) then
            const = lsu_elem(ihru)%ru_frac
            if (lsu_elem(ihru)%obtyp == "hru") then
              ruwb_d%sw_init = ruwb_d%sw_init + hwb_d(ihru)%sw_init * const
              ruwb_d%sw_final = ruwb_d%sw_final + hwb_d(ihru)%sw_final * const
            end if
            ! summing HRU_LTE output
            if (lsu_elem(ihru)%obtyp == "hlt") then
              ruwb_d%sw_init = ruwb_d%sw_init + hltwb_d(ihru)%sw_init * const
              ruwb_d%sw_final = ruwb_d%sw_final + hltwb_d(ihru)%sw_final * const
            end if
          end if
        end do    !ielem
        ruwb_d%sw_init = ruwb_d%sw_init
        ruwb_d%sw_final = ruwb_d%sw_final
        ruwb_y%sw_init = ruwb_d%sw_init
        ruwb_y%sw_final = ruwb_d%sw_final
        ruwb_a%sw_init = ruwb_d%sw_init
        ruwb_a%sw_final = ruwb_d%sw_final
      end do      !ilsu

      end subroutine basin_sw_init