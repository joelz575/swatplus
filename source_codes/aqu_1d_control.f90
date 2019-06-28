      subroutine aqu_1d_control 
    
      use aquifer_module
      use time_module
      use hydrograph_module
      use climate_module, only : wst
      use maximum_data_module
      use constituent_mass_module
      
      implicit none
      
      integer :: iaq            !none      |counter
      integer :: iaqdb          !          |
      integer :: icha           !          |
      integer :: iob_out        !          !object type out
      integer :: iout           !none      |counter
      integer :: ii             !none      |counter
      integer :: icontrib      !none      |counter
      real :: stor_init         !          |
      real :: conc_no3          !          |
      real :: step              !          |
      real :: contrib_len
      real :: contrib_len_left

      !! set pointers to aquifer database and weather station
      iaq = ob(icmd)%num
      iaqdb = ob(icmd)%props
      iwst = ob(icmd)%wst
      stor_init = aqu_d(iaq)%stor
      
      ob(icmd)%hd(1) = hz
      ob(icmd)%hd(2) = hz
      if (cs_db%num_tot > 0) then
        obcs(icmd)%hd(1) = hin_csz
        obcs(icmd)%hd(2) = hin_csz
      end if

      !convert from m^3 to mm
      aqu_d(iaq)%rchrg = ob(icmd)%hin%flo / (10. * ob(icmd)%area_ha)
      
      !! lag recharge from bottom of soil to water table ** disabled
      !aqu_d(iaq)%rchrg = (1. - aqu_prm(iaqdb)%delay_e) * aqu_d(iaq)%rchrg + aqu_prm(iaqdb)%delay_e * aqu_st(iaq)%rchrg_prev
      
      aqu_prm(iaq)%rchrg_prev = aqu_d(iaq)%rchrg
      
      !! add recharge to aquifer storage
      aqu_d(iaq)%stor = aqu_d(iaq)%stor + aqu_d(iaq)%rchrg
      
      !! compute flow and substract from storage
      if (aqu_d(iaq)%dep_wt < aqu_prm(iaq)%flo_min) then
        aqu_d(iaq)%flo = aqu_d(iaq)%flo * aqu_prm(iaqdb)%alpha_e + aqu_d(iaq)%rchrg * (1. - aqu_prm(iaqdb)%alpha_e)
        aqu_d(iaq)%flo = Max (0., aqu_d(iaq)%flo)
        aqu_d(iaq)%flo = Min (aqu_d(iaq)%stor, aqu_d(iaq)%flo)
        aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%flo
      else
        aqu_d(iaq)%flo = 0.
      endif

      !! set hydrograph flow from aquifer- convert mm to m3
      ob(icmd)%hd(1)%flo = 10. * aqu_d(iaq)%flo * ob(icmd)%area_ha
      
      !! compute seepage through aquifer and subtract from storage
      aqu_d(iaq)%seep = aqu_d(iaq)%rchrg * aqudb(iaqdb)%seep
      aqu_d(iaq)%seep = amin1 (aqu_d(iaq)%seep, aqu_d(iaq)%stor)
      ob(icmd)%hd(2)%flo = 10. * aqu_d(iaq)%seep * ob(icmd)%area_ha
      
      aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%seep
      
      !! compute revap (deep root uptake from aquifer) and subtract from storage
      if (aqu_d(iaq)%dep_wt < aqu_prm(iaq)%revap_min) then
        aqu_d(iaq)%revap = wst(iwst)%weat%pet * aqu_prm(iaq)%revap_co
        aqu_d(iaq)%revap = amin1 (aqu_d(iaq)%revap, aqu_d(iaq)%stor)
        aqu_d(iaq)%stor = aqu_d(iaq)%stor - aqu_d(iaq)%revap
      else
        aqu_d(iaq)%revap = 0.
      end if

      !! compute groundwater depth from surface
      aqu_d(iaq)%dep_wt = aqudb(iaqdb)%dep_bot - (aqu_d(iaq)%stor / (1000. * aqudb(iaqdb)%spyld))
      aqu_d(iaq)%dep_wt = amax1 (0., aqu_d(iaq)%dep_wt)
      
      !! compute groundwater height - datum: above bottom of channel
      !aqu_d(iaq)%hgt = aqu_d(iaq)%hgt * aqu_prm(iaqdb)%alpha_e + aqu_d(iaq)%rchrg * (1. - aqu_prm(iaqdb)%alpha_e) /   & 
      !                                       (800. * aqudb(iaqdb)%spyld * aqu_prm(iaq)%alpha + 1.e-6)       
      !aqu_d(iaq)%hgt = Max(1.e-6, aqu_d(iaq)%hgt)

      !! compute nitrate recharge into the aquifer
      aqu_d(iaq)%rchrg_n = ob(icmd)%hin%no3 / (10. * ob(icmd)%area_ha)
      if (ob(icmd)%hin%no3 > 1.) then
        ii = 1
      end if
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 + aqu_d(iaq)%rchrg_n
      aqu_prm(iaq)%rchrgn_prev = aqu_d(iaq)%rchrg_n
      
      !! compute nitrate return flow out of aquifer
      if (aqu_d(iaq)%stor > 1.e-6) then
        conc_no3 = aqu_d(iaq)%no3 / aqu_d(iaq)%stor
      else
        conc_no3 = 0.
      endif
      ob(icmd)%hd(1)%no3 = conc_no3 * aqu_d(iaq)%flo
      ob(icmd)%hd(1)%no3 = amin1(ob(icmd)%hd(1)%no3, aqu_d(iaq)%no3)
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 - ob(icmd)%hd(1)%no3
      aqu_d(iaq)%no3gw = ob(icmd)%hd(1)%no3
      
      !revapno3 = conc * revap -- dont include nitrate uptake by plant
      
      !! compute nitrate seepage out of aquifer
      aqu_d(iaq)%seepno3 = conc_no3 * aqu_d(iaq)%seep
      aqu_d(iaq)%seepno3 = amin1(aqu_d(iaq)%seepno3, aqu_d(iaq)%no3)
      aqu_d(iaq)%no3 = aqu_d(iaq)%no3 - aqu_d(iaq)%seepno3
      ob(icmd)%hd(2)%no3 = aqu_d(iaq)%seepno3
      
      !! compute mineral p flow from aquifer - m^3 * ppm * 1000 kg/m^3 = 1/1000
      aqu_d(iaq)%minp = ob(icmd)%hin%flo * aqudb(iaqdb)%minp / 1000.
      !! set hydrograph soluble p from aquifer- convert kg/ha to m3
      ob(icmd)%hd(1)%solp = 10. * aqu_d(iaq)%minp * ob(icmd)%area_ha
      ob(icmd)%hd(1)%solp = ob(icmd)%hin%flo * aqudb(iaqdb)%minp / 1000.

      !! compute fraction of flow to each channel in the aquifer
      !! if connected to aquifer - add flow
      if (db_mx%aqu2d > 0) then
        contrib_len = aq_ch(iaq)%len_tot * aqu_d(iaq)%flo / aqudb(iaqdb)%bf_max
      
        !! find the first channel contributing
        icontrib = 0
        do icha = 1, aq_ch(iaq)%num_tot
          if (contrib_len >= aq_ch(iaq)%ch(icha)%len_left) then
            icontrib = icha
            contrib_len_left = aq_ch(iaq)%ch(icha)%len_left + aq_ch(iaq)%ch(icha)%len
            exit
          end if
        end do
        !! set fractions for flow to each channel
        do icha = 1, aq_ch(iaq)%num_tot
          if (icha >= icontrib .and. icontrib > 0) then
            aq_ch(iaq)%ch(icha)%flo_fr = aq_ch(iaq)%ch(icha)%len / contrib_len_left
          else
            aq_ch(iaq)%ch(icha)%flo_fr = 0.
          end if
        end do
        !! save hydrographs to distribute on following day
        aq_ch(iaq)%hd = ob(icmd)%hd(1)
      end if

      ! compute outflow objects (flow to channels, reservoirs, or aquifer)
      ! if flow from hru is directly routed
      iob_out = icmd
      aqu_d(iaq)%flo_cha = 0.
      aqu_d(iaq)%flo_res = 0.
      aqu_d(iaq)%flo_ls = 0.
      do iout = 1, ob(iob_out)%src_tot
        select case (ob(iob_out)%obtyp_out(iout))
        case ("cha")
          aqu_d(iaq)%flo_cha = aqu_d(iaq)%flo_cha + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
        case ("sdc")
          aqu_d(iaq)%flo_cha = aqu_d(iaq)%flo_cha + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
        case ("res")
          aqu_d(iaq)%flo_res = aqu_d(iaq)%flo_res + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
        case ("aqu")
          aqu_d(iaq)%flo_ls = aqu_d(iaq)%flo_ls + aqu_d(iaq)%flo * ob(iob_out)%frac_out(iout)
        end select
      end do

      if (time%step > 0) then
        do ii = 1, time%step
          step = real(time%step)
          ob(icmd)%ts(1,ii) = ob(icmd)%hd(1) / step
        end do
      end if

      return
      end subroutine aqu_1d_control