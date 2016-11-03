      subroutine stor_surf (jres)
      use jrw_datalib_module
      use reservoir_module
      use parm

      integer :: k, ii

      !! initialize variables for reservoir daily simulation
      call res_dayinit
      
      bypass = 1. - res_hyd(ihyd)%frac
      frac_in = 1. - bypass - ressa / hru(ihru)%area_ha
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
      res(jres)%orgn = res(jres)%orgn + sedorgn(ihru) * frac_in
      res(jres)%sedp = res(jres)%sedp + sedorgp(ihru) * frac_in
      res(jres)%no3 = res(jres)%no3 + surqno3(ihru) * frac_in
      res(jres)%nh3 = res(jres)%nh3 + 0.  !add ammonium 
      res(jres)%no2 = res(jres)%no2 + 0.  !add no2
      res(jres)%solp = res(jres)%solp + (sedminps(ihru) + sedminpa(ihru)) * frac_in

      idat = res_ob(jres)%props
      ihyd = res_dat(idat)%hyd
      ised = res_dat(idat)%sed
      if(time%step == 0) then		!! urban modeling by J.Jeong
        !call from actions --> call res_hydro (jres, ihyd, ised)
        !! determine reservoir outflow
        irel = res_dat(idat)%release
        call conditions (irel, ihyd)
        call actions (irel, jres)
      else
        !call res_hourly
      endif


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
      sedminps(ihru) = ressolpo / hru(ihru)%area_ha / 2. + sedminps(ihru) *   &
                                                                  bypass
      sedminpa(ihru) = ressolpo / hru(ihru)%area_ha / 2. + sedminpa(ihru) *   &
                                                                  bypass
        !! summary calculations
        if (time%yrs > pco%nyskip) then
          !!calculate concentrations
          resorgnc = res(jres)%orgn / (res(jres)%flo+.1) * 1000.
          resno3c = res(jres)%no3 / (res(jres)%flo+.1) * 1000.
          resno2c = res(jres)%no2 / (res(jres)%flo+.1) * 1000.
          resnh3c = res(jres)%nh3 / (res(jres)%flo+.1) * 1000.
          resorgpc = res(jres)%sedp / (res(jres)%flo+.1) * 1000.
          ressolpc = res(jres)%solp / (res(jres)%flo+.1) * 1000.
          sedcon = res(jres)%sed * 1.e6
          
          resd(jres)%flowi = flwi / 86400. 
          resd(jres)%flowo = flwo / 86400.
          resd(jres)%sedi = sedi 
          resd(jres)%sedo = sedo
          resd(jres)%sedcon = sedcon
          resd(jres)%pesti = pesti
          resd(jres)%reactw = reactw
          resd(jres)%volatpst = volatpst
          resd(jres)%setlpst = setlpst
          resd(jres)%resuspst = resuspst
          resd(jres)%difus = difus
          resd(jres)%reactb = reactb
          resd(jres)%pesto = pesto
          resd(jres)%pstcon = pstcon
          resd(jres)%spstcon = spstcon
          resd(jres)%ev = resev
          resd(jres)%sep = ressep
          resd(jres)%pcp = respcp
          resd(jres)%flwim3 = flwim3
          resd(jres)%flwom3 = flwom3
          resd(jres)%orgni = orgni
          resd(jres)%orgno = orgno
          resd(jres)%orgpi = orgpi
          resd(jres)%orgpo = orgpo
          resd(jres)%no3i = no3i
          resd(jres)%no3o = no3o
          resd(jres)%no2i = no2i
          resd(jres)%no2o = no2o
          resd(jres)%nh3i = nh3i
          resd(jres)%nh3o = nh3o
          resd(jres)%solpi = solpi
          resd(jres)%solpo = solpo
          resd(jres)%chlai = chlai
          resd(jres)%chlao = chlao
          resd(jres)%orgpc = orgpc
          resd(jres)%solpc = solpc
          resd(jres)%orgnc = orgnc
          resd(jres)%no3c = no3c
          resd(jres)%no2c = no2c
          resd(jres)%nh3c = nh3c
        end if             

        if (time%yrs > pco%nyskip) then
          call reservoir_output(jres)
        endif
        
      return
      end subroutine stor_surf