      subroutine aqu_1d_control 
    
      use aquifer_module
      use time_module
      use hydrograph_module
      use climate_module, only : wst
      use maximum_data_module
      use constituent_mass_module
      use pesticide_data_module
      use aqu_pesticide_module
      
      implicit none
      
      integer :: iaq            !none       |counter
      integer :: iaqdb          !           |
      integer :: icha           !           |
      integer :: iob_out        !           !object type out
      integer :: iout           !none       |counter
      integer :: ii             !none       |counter
      integer :: icontrib       !none       |counter
      integer :: ipest          !none       |counter
      integer :: ipest_db       !none       |pesticide number from pesticide data base
      real :: stor_init         !           |
      real :: conc_no3          !           |
      real :: step              !           |
      real :: contrib_len
      real :: contrib_len_left
      real :: pest_init         !kg/ha      |amount of pesticide present at beginning of day
      real :: pest_end          !kg/ha      |amount of pesticide present at end of day
      real :: flow_mm           !mm         |total flow through aquifer - return flow + seepage
      real :: pest_kg_ha        !kg/ha      |soluble pesticide moving with flow
      real :: conc              !kg/m3      |concentraion of pesticide in flow
      real :: zdb1              !mm         |kd - flow factor for pesticide transport
      real :: kd                !(mg/kg)/(mg/L) |koc * carbon

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

      !! compute pesticide transport and decay
      do ipest = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(ipest)
        
        !! add incoming pesticide to storage
        cs_aqu(iaq)%pest(ipest) = cs_aqu(iaq)%pest(ipest) + obcs(icmd)%hin%pest(ipest)
        
        !! compute pesticide decay in the aquifer
        pest_init = cs_aqu(iaq)%pest(ipest)
        if (pest_init > 1.e-12) then
          pest_end = pest_init * pestcp(ipest_db)%decay_s
          cs_aqu(iaq)%pest(ipest) = pest_end
          aqupst_d(iaq)%pest(ipest)%react = (pest_init - pest_end)
        end if
            
        !! compute pesticide in aquifer flow
        kd = pestdb(ipest_db)%koc * aqu_d(iaq)%cbn / 100.
        !! assume specific yield = upper limit (effective vs total porosity) 
        !! and bulk density of 2.0 (ave of rock and soil - 2.65 and 1.35)
        !! mm = (mm/mm + (m^3/ton)*(ton/m^3)) * m * 1000.
        zdb1 = (aqudb(iaqdb)%spyld + kd * 2.0) * aqudb(iaqdb)%flo_dist * 1000.

        !! compute volume of flow through the layer - mm
        flow_mm = aqu_d(iaq)%flo + aqu_d(iaq)%seep

        !! compute concentration in the flow
        if (cs_aqu(iaq)%pest(ipest) >= 0.0001 .and. flow_mm > 0.) then
          pest_kg_ha =  cs_aqu(iaq)%pest(ipest) * (1. - Exp(-flow_mm / (zdb1 + 1.e-6)))
          conc = pest_kg_ha / flow_mm
          conc = Min (pestdb(ipest_db)%solub / 100., conc)      ! check solubility
          pest_kg_ha = conc * flow_mm
          if (pest_kg_ha >  cs_aqu(iaq)%pest(ipest)) pest_kg_ha = cs_aqu(iaq)%pest(ipest)
          
          !! return flow (1) and deep seepage (2)  kg = kg/ha * ha
          obcs(icmd)%hd(1)%pest(ipest) = conc * aqu_d(iaq)%flo * ob(icmd)%area_ha
          obcs(icmd)%hd(2)%pest(ipest) = conc * aqu_d(iaq)%seep * ob(icmd)%area_ha
          cs_aqu(iaq)%pest(ipest) =  cs_aqu(iaq)%pest(ipest) - pest_kg_ha
          aqupst_d(iaq)%pest(ipest)%sol_out = 1.e6 * pest_kg_ha * ob(icmd)%area_ha
        endif
      
        !! set pesticide output variables - mg
        aqupst_d(iaq)%pest(ipest)%tot_in = obcs(icmd)%hin%pest(ipest)
        !! assume frsol = 1 (all soluble)
        aqupst_d(iaq)%pest(ipest)%sol_out = 1. * obcs(icmd)%hd(1)%pest(ipest)
        aqupst_d(iaq)%pest(ipest)%sor_out = 0.
        aqupst_d(iaq)%pest(ipest)%stor = cs_aqu(iaq)%pest(ipest)
      end do
        
      !! compute outflow objects (flow to channels, reservoirs, or aquifer)
      !! if flow from hru is directly routed
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