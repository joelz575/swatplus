      subroutine sd_channel_nutrients (ht1)
      type (hyd_output), intent (inout) :: ht1
      integer :: nb_overb = 0
      
      inut = 0
      ifld = 1
      if (ob(icmd)%props2 > 0) then   
        !! 2-stage ditch
        if (sd_ch(ich)%overbank == "ob") then
          nd_overb = nd_overb + 1
          !! over-bank full flow
          conc_no3 = 1000. * ht1%no3 / ht1%flo
          denit = exp(rte_nut(inut)%no3_slp_ob * alog(conc_no3) + rte_nut(inut)%no3_int_ob)
          area_m2 = 1000. * sd_ch(ich)%chl * field_db(ifld)%wid
          turbid = ht1%sed / rte_nut(inut)%turb_tss_slp
          turbid_reduc = - (rte_nut(inut)%turb_slp * turbid + rte_nut(inut)%turb_int) * area_m2   !reduction is positive
          turbid_reduc = Max (0., turbid_reduc)
          sed_reduc = rte_nut(inut)%tss_slp * turbid_reduc + rte_nut(inut)%tss_int
          tp_reduc = rte_nut(inut)%tp_slp * turbid_reduc + rte_nut(inut)%tp_int
          srp_reduc = rte_nut(inut)%srp_slp * tp_reduc + rte_nut(inut)%srp_int
        else
          !! under-bank full flow
          denit = rte_nut(inut)%no3_slp_ub * ht1%no3 + rte_nut(inut)%no3_int_ub
        end if
      else
        !! single stage ditch
        denit = rte_nut(inut)%no3_slp * ht1%no3 + rte_nut(inut)%no3_int
      end if

      !! calc reductions in t and kg --> kg=ppm*m3/1000.
      sed_reduc_t = sed_reduc * ht1%flo / 1000000.
      no3_reduc_kg = denit * area_m2* 24.
      tp_reduc_kg = tp_reduc * ht1%flo / 1000.
      srp_reduc_kg = srp_reduc * ht1%flo / 1000.
      
      return
      
      end subroutine sd_channel_nutrients