      subroutine stor_surf
    
      use jrw_datalib_module, only : wet_hyd, wet_dat, res_weir, res_dat 
      use reservoir_module
      use parm, only : hru, sedyld, sanyld, silyld, clayld, sagyld, lagyld, grayld, sedminps, sedminpa,   &
        surqno3, sedorgn, sedorgp, qdr, ihru, pet_day, qday, resclai, resclao, resgrai, resgrao, reslagi, &
        reslago, ressagi, ressago, ressani, ressano, ressili, ressilo
      use conditional_module
      use climate_parms
      use hydrograph_module, only : wet, res, recall, ob
      use time_module
      use basin_module

      integer :: k, ii

      !! initialize variables for reservoir daily simulation
      call res_dayinit
      
      bypass = 1. - wet_hyd(ihyd)%frac
      frac_in = 1. - bypass - res_ob(jres)%area_ha / hru(ihru)%area_ha
      resflwi = qday * frac_in
      pet_day = wst(iwst)%weat%pet
      respcp = wst(iwst)%weat%precip
      
      ressedi = sedyld(ihru) * frac_in
      ressani = sanyld(ihru) * frac_in
      ressili = silyld(ihru) * frac_in
	  resclai = clayld(ihru) * frac_in 
	  ressagi = sagyld(ihru) * frac_in
	  reslagi = lagyld(ihru) * frac_in
	  resgrai = grayld(ihru) * frac_in
      solpesti = 0.   !add soluble pest
      sorpesti = 0.   !add sorbed pest

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      wet(j)%orgn = wet(j)%orgn + sedorgn(ihru) * frac_in
      wet(j)%sedp = wet(j)%sedp + sedorgp(ihru) * frac_in
      wet(j)%no3 = wet(j)%no3 + surqno3(ihru) * frac_in
      wet(j)%nh3 = wet(j)%nh3 + 0.  !add ammonium 
      wet(j)%no2 = wet(j)%no2 + 0.  !add no2
      wet(j)%solp = wet(j)%solp + (sedminps(ihru) + sedminpa(ihru)) * frac_in

      iprop = hru(ihru)%dbs%surf_stor
      ihyd = wet_dat(iprop)%hyd
      ihyd = wet_dat(idat)%hyd
      ised = wet_dat(idat)%sed
      id = wet_dat(idat)%release
        !calc release from decision table
        do iac = 1, d_tbl(id)%acts
          action = "n"
          do ial = 1, d_tbl(id)%alts
            if (d_tbl(id)%act_hit(ial) == "y" .and. d_tbl(id)%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == 'y') then
            select case (d_tbl(id)%act(iac)%option)
            case ("rate")
              resflwo = d_tbl(id)%act(iac)%const * 86400.
            case ("days")
              select case (d_tbl(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = res_ob(ihyd)%pvol
                case ("evol")
                  b_lo = res_ob(ihyd)%evol
              end select
              resflwo = (res(jres)%flo - b_lo) / d_tbl(id)%act(iac)%const
            case ("weir")
              resflwo = res_weir(ihyd)%c * res_weir(ihyd)%k * res_weir(ihyd)%w * (res_h ** 1.5)
            case ("meas")
              irel = int(d_tbl(id)%act(iac)%const)
              select case (recall(irel)%typ)
              case (1)    !daily
                resflwo = recall(irel)%hd(time%day,time%yrs)%flo
              case (2)    !monthly
                resflwo = recall(irel)%hd(time%mo,time%yrs)%flo
              case (3)    !annual
                resflwo = recall(irel)%hd(1,time%yrs)%flo
              end select
            end select
          end if
        end do    ! iac

      !! update surface area
      !! wetland on hru - solve quadratic to find new depth
      x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(jres)%flo / wet_ob(ihyd)%pvol)
      if (x1 < 1.e-6) then
        wet_h = 0.
      else
        wet_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
        wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
      end if
      wet_ob(jres)%area_ha = wet_ob(ihyd)%psa * (1. + wet_hyd(ihyd)%acoef * wet_h)
        
      !! perform reservoir nutrient balance
      inut = res_dat(idat)%nut
      call res_nutrient (jres, inut)

      !! perform reservoir pesticide transformations
      ipst = res_dat(idat)%pst
      call res_pest (jres, ipst)

      !! set values for routing variables
      ob(icmd)%hd(1)%temp = 0.                  !!undefined
      qdr(ihru) = resflwo / (10. * hru(ihru)%area_ha) + qday * bypass
        
      sedyld(ihru) = ressedo / hru(ihru)%area_ha + sedyld(ihru) * bypass
      sanyld(ihru) = ressano / hru(ihru)%area_ha + sanyld(ihru) * bypass
      silyld(ihru) = ressilo / hru(ihru)%area_ha + silyld(ihru) * bypass
	  clayld(ihru) = resclao / hru(ihru)%area_ha + clayld(ihru) * bypass 
	  sagyld(ihru) = ressago / hru(ihru)%area_ha + sagyld(ihru) * bypass
	  lagyld(ihru) = reslago / hru(ihru)%area_ha + lagyld(ihru) * bypass
	  grayld(ihru) = resgrao / hru(ihru)%area_ha + grayld(ihru) * bypass
      solpesti = 0.   !add soluble pest
      sorpesti = 0.   !add sorbed pest

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      sedorgn(ihru) = resorgno / hru(ihru)%area_ha + sedorgn(ihru) * bypass
      sedorgp(ihru) = resorgpo / hru(ihru)%area_ha + sedorgp(ihru) * bypass
      surqno3(ihru) = resno3o/ hru(ihru)%area_ha  + surqno3(ihru) * bypass
      !nh3 = resnh3o + 0.  !add ammonium 
      !no2  = resno2o + 0.  !add no2
      sedminps(ihru) = ressolpo / hru(ihru)%area_ha / 2. + sedminps(ihru) * bypass
      sedminpa(ihru) = ressolpo / hru(ihru)%area_ha / 2. + sedminpa(ihru) * bypass
      
        !! summary calculations
        if (time%yrs > pco%nyskip) then
          !!calculate concentrations
          resorgnc = wet(j)%orgn / (wet(j)%flo+.1) * 1000.
          resno3c = wet(j)%no3 / (wet(j)%flo+.1) * 1000.
          resno2c = wet(j)%no2 / (wet(j)%flo+.1) * 1000.
          resnh3c = wet(j)%nh3 / (wet(j)%flo+.1) * 1000.
          resorgpc = wet(j)%sedp / (wet(j)%flo+.1) * 1000.
          ressolpc = wet(j)%solp / (wet(j)%flo+.1) * 1000.
          sedcon = wet(j)%sed * 1.e6
          
          wet_d(j)%vol = wet(j)%flo / 10000.      !m^3 -> ha-m
          wet_d(j)%area_ha = res_ob(jres)%area_ha
          wet_d(j)%flowi = flwi / 10000.             !m^3 -> ha-m
          wet_d(j)%flowo = flwo / 10000.             !m^3 -> ha-m
          wet_d(j)%ev = resev / 10000.               !m^3 -> ha-m
          wet_d(j)%sep = ressep / 10000.             !m^3 -> ha-m
          wet_d(j)%pcp = respcp / 10000.             !m^3 -> ha-m
          wet_d(j)%sedi = sedi 
          wet_d(j)%sedo = sedo
          wet_d(j)%sedcon = sedcon
          wet_d(j)%pesti = pesti
          wet_d(j)%reactw = reactw
          wet_d(j)%volatpst = volatpst
          wet_d(j)%setlpst = setlpst
          wet_d(j)%resuspst = resuspst
          wet_d(j)%difus = difus
          wet_d(j)%reactb = reactb
          wet_d(j)%pesto = pesto
          wet_d(j)%pstcon = pstcon
          wet_d(j)%spstcon = spstcon
          wet_d(j)%orgni = orgni
          wet_d(j)%orgno = orgno
          wet_d(j)%orgpi = orgpi
          wet_d(j)%orgpo = orgpo
          wet_d(j)%no3i = no3i
          wet_d(j)%no3o = no3o
          wet_d(j)%no2i = no2i
          wet_d(j)%no2o = no2o
          wet_d(j)%nh3i = nh3i
          wet_d(j)%nh3o = nh3o
          wet_d(j)%solpi = solpi
          wet_d(j)%solpo = solpo
          wet_d(j)%chlai = chlai
          wet_d(j)%chlao = chlao
          wet_d(j)%orgpc = orgpc
          wet_d(j)%solpc = solpc
          wet_d(j)%orgnc = orgnc
          wet_d(j)%no3c = no3c
          wet_d(j)%no2c = no2c
          wet_d(j)%nh3c = nh3c
        end if             

        if (time%yrs > pco%nyskip) then
          call wetland_output (j)
        endif
        
      return
      end subroutine stor_surf