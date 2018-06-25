      subroutine aqu_1d_control 
    
      use aquifer_module
      use time_module
      use hydrograph_module
      use climate_module, only : wst
      
      implicit none
      
      integer :: iaq            !none      |counter
      integer :: iaqdb          !          |
      integer :: iob_out        !          !object type out
      integer :: iout           !none      |counter
      integer :: ii             !none      |counter
      real :: stor_init         !          |
      real :: conc_no3          !          |
      real :: step              !          |
      real :: err               !          |

      !! set pointers to aquifer database and weather station
      iaq = ob(icmd)%num
      iaqdb = ob(icmd)%props
      iwst = ob(icmd)%wst
      stor_init = aqu(iaq)%stor
      
      !convert from m^3 to mm
      aqu(iaq)%rchrg = ob(icmd)%hin%flo / (10. * ob(icmd)%area_ha)
      
      !! lag recharge from bottom of soil to water table
   !   aqu(iaq)%rchrg = (1. - aqu_prm(iaqdb)%delay_e) * aqu(iaq)%rchrg + aqu_prm(iaqdb)%delay_e * aqu_st(iaq)%rchrg_prev
      aqu_st(iaq)%rchrg_prev = aqu(iaq)%rchrg
      
      !! add recharge to aquifer storage
      aqu(iaq)%stor = aqu(iaq)%stor + aqu(iaq)%rchrg
      
      !! compute flow and substract from storage
      if (aqu(iaq)%stor > aqu_st(iaq)%flo_min) then
        aqu(iaq)%flo = aqu(iaq)%flo * aqu_prm(iaqdb)%alpha_e + aqu(iaq)%rchrg * (1. - aqu_prm(iaqdb)%alpha_e)
        aqu(iaq)%flo = Max (0., aqu(iaq)%flo)
        aqu(iaq)%flo = Min (aqu(iaq)%stor, aqu(iaq)%flo)
        aqu(iaq)%stor = aqu(iaq)%stor - aqu(iaq)%flo
      else
        aqu(iaq)%flo = 0.
      endif

      !! set hydrograph flow from aquifer- convert mm to m3
      ob(icmd)%hd(1)%flo = 10. * aqu(iaq)%flo * ob(icmd)%area_ha
      
      !! compute seepage through aquifer and subtract from storage
      aqu(iaq)%seep = aqu(iaq)%rchrg * aqudb(iaqdb)%seep
      aqu(iaq)%seep = amin1 (aqu(iaq)%seep, aqu(iaq)%stor)
      aqu(iaq)%stor = aqu(iaq)%stor - aqu(iaq)%seep
      
      !! compute revap (deep root uptake from aquifer) and subtract from storage
      if (aqu(iaq)%stor > aqu_st(iaq)%revap_min) then
        aqu(iaq)%revap = wst(iwst)%weat%pet * aqu_st(iaq)%revap_co
        aqu(iaq)%revap = amin1 (aqu(iaq)%revap, aqu(iaq)%stor)
        aqu(iaq)%stor = aqu(iaq)%stor - aqu(iaq)%revap
      else
        aqu(iaq)%revap = 0.
      end if

      !! compute groundwater height - datum: above bottom of channel
      aqu(iaq)%hgt = aqu(iaq)%hgt * aqu_prm(iaqdb)%alpha_e + aqu(iaq)%rchrg * (1. - aqu_prm(iaqdb)%alpha_e) /   & 
                                             (800. * aqudb(iaqdb)%spyld * aqudb(iaqdb)%alpha + 1.e-6)       
      aqu(iaq)%hgt = Max(1.e-6, aqu(iaq)%hgt)

      !! compute nitrate recharge into the aquifer
      aqu(iaq)%rchrg_n = ob(icmd)%hin%no3 / (10. * ob(icmd)%area_ha)
      aqu(iaq)%rchrg_n = (1. - aqu_prm(iaqdb)%delay_e) * aqu(iaq)%rchrg_n + aqu_prm(iaqdb)%delay_e *       &
                                                                                 aqu_st(iaq)%rchrgn_prev
      aqu(iaq)%no3 = aqu(iaq)%no3 + aqu(iaq)%rchrg_n
      aqu_st(iaq)%rchrgn_prev = aqu(iaq)%rchrg_n
      
      !! compute nitrate return flow out of aquifer
      if (aqu(iaq)%stor > 1.e-6) then
        conc_no3 = aqu(iaq)%no3 / aqu(iaq)%stor
      else
        conc_no3 = 0.
      endif
      ob(icmd)%hd(1)%no3 = conc_no3 * aqu(iaq)%flo
      ob(icmd)%hd(1)%no3 = amin1(ob(icmd)%hd(1)%no3, aqu(iaq)%no3)
      aqu(iaq)%no3 = aqu(iaq)%no3 - ob(icmd)%hd(1)%no3
      aqu(iaq)%no3gw = ob(icmd)%hd(1)%no3
      
      !revapno3 = conc * revap -- dont include nitrate uptake by plant
      
      !! compute nitrate seepage out of aquifer
      aqu(iaq)%seepno3 = conc_no3 * aqu(iaq)%seep
      aqu(iaq)%seepno3 = amin1(aqu(iaq)%seepno3, aqu(iaq)%no3)
      aqu(iaq)%no3 = aqu(iaq)%no3 - aqu(iaq)%seepno3
      
      !! compute mineral p flow from aquifer - m^3 * ppm * 1000 kg/m^3 = 1/1000
      aqu(iaq)%minp = ob(icmd)%hin%flo * aqudb(iaqdb)%minp / 1000.
      !! set hydrograph flow from aquifer- convert mm to m3
      ob(icmd)%hd(1)%sedp = 10. * aqu(iaq)%flo * ob(icmd)%area_ha

      ! compute outflow objects (flow to channels, reservoirs, or landscape)
      ! if flow from hru is directly routed
      iob_out = icmd
      aqu(iaq)%flo_cha = 0.
      aqu(iaq)%flo_res = 0.
      aqu(iaq)%flo_ls = 0.
      do iout = 1, ob(iob_out)%src_tot
        select case (ob(iob_out)%obtyp_out(iout))
        case ("cha")
          aqu(iaq)%flo_cha = aqu(iaq)%flo_cha + aqu(iaq)%flo * ob(iob_out)%frac_out(iout)
        case ("res")
          aqu(iaq)%flo_res = aqu(iaq)%flo_res + aqu(iaq)%flo * ob(iob_out)%frac_out(iout)
        case ("aqu")
          aqu(iaq)%flo_ls = aqu(iaq)%flo_ls + aqu(iaq)%flo * ob(iob_out)%frac_out(iout)
        end select
      end do

      
      if (time%step > 0) then
        do ii = 1, time%step
          step = real(time%step)
          ob(icmd)%ts(1,ii) = ob(icmd)%hd(1) / step
        end do
      end if
      
      err = stor_init - aqu(iaq)%stor + aqu(iaq)%rchrg - aqu(iaq)%flo - aqu(iaq)%seep - aqu(iaq)%revap
      if (iaq == 1) then
        sumrchrg = sumrchrg + aqu(iaq)%rchrg
        sumflo = sumflo + aqu(iaq)%flo
        sumseep = sumseep + aqu(iaq)%seep
        sumrevap = sumrevap + aqu(iaq)%revap
      end if
      
      return
      end subroutine aqu_1d_control