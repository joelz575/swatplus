      subroutine res_control (jres)
      
      use basin_module
      use reservoir_data_module 
      use time_module
      use reservoir_module
      use climate_module
      use hydrograph_module
      use conditional_module
      
      implicit none
      
      integer :: k                    !              | 
      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      integer :: idat                 !              |
      integer :: ihyd                 !none          |counter
      integer :: ised                 !none          |counter
      integer :: irel                 !              |
      integer :: inut                 !none          |counter
      integer :: ob_cur

      !! initialize variables for reservoir daily simulation
      call res_dayinit

      resflwi = ob(icmd)%hin%flo
      respcp = wst(iwst)%weat%precip
      tair_mx = wst(iwst)%weat%tmax
      tair_mn = wst(iwst)%weat%tmin
      tair_av = wst(iwst)%weat%tave
      ressedi = ob(icmd)%hin%sed 
      ressani = ob(icmd)%hin%san 
      ressili = ob(icmd)%hin%sil 
	  resclai = ob(icmd)%hin%cla 
	  ressagi = ob(icmd)%hin%sag 
	  reslagi = ob(icmd)%hin%lag 
	  resgrai = ob(icmd)%hin%grv 

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      res(jres)%orgn = res(jres)%orgn + ob(icmd)%hin%orgn 
      res(jres)%sedp = res(jres)%sedp + ob(icmd)%hin%sedp 
      res(jres)%no3 = res(jres)%no3 + ob(icmd)%hin%no3 
      res(jres)%nh3 = res(jres)%nh3 + ob(icmd)%hin%nh3 
      res(jres)%no2 = res(jres)%no2 + ob(icmd)%hin%no2 
      res(jres)%solp = res(jres)%solp + ob(icmd)%hin%solp

      if (time%yrc > res_hyd(jres)%iyres .or. (time%mo >= res_hyd(jres)%mores   &
                                   .and. time%yrc == res_hyd(jres)%iyres)) then

        !! Adjust Reservoir Storage for Irrigation Diversions
        !call irr_res

        !! perform reservoir water/sediment balance
        idat = res_ob(jres)%props
        ihyd = res_dat(idat)%hyd
        ised = res_dat(idat)%sed
        if(time%step == 0) then		!! urban modeling by J.Jeong
	      !call from actions --> call res_hydro (jres, ihyd, ised)
          !! determine reservoir outflow
          irel = res_dat(idat)%release
          d_tbl => dtbl_res(irel)
          call conditions (ihyd)
          call res_hydro (jres, irel, ihyd, ised)
          call res_sediment (jres, ihyd, ised)
	    else
	      !call res_hourly
        endif

        !! perform reservoir nutrient balance
        inut = res_dat(idat)%nut
        call res_nutrient (jres, inut)

        !! perform reservoir pesticide transformations
        call res_pest (jres)

        !! set values for routing variables
        ob(icmd)%hd(1)%temp = 0.                  !!undefined
        ob(icmd)%hd(1)%flo = resflwo
        ob(icmd)%hd(1)%sed = ressedo
        ob(icmd)%hd(1)%orgn = resorgno
        ob(icmd)%hd(1)%sedp = resorgpo
        ob(icmd)%hd(1)%no3 = resno3o
        ob(icmd)%hd(1)%solp = ressolpo
        ob(icmd)%hd(1)%chla = reschlao
        ob(icmd)%hd(1)%nh3 = resnh3o
        ob(icmd)%hd(1)%no2 = resno2o
        ob(icmd)%hd(1)%cbod = 0.                    !!CBOD
        ob(icmd)%hd(1)%dox = 0.                     !!dissolved O2

        if (time%step > 0) then
          do ii = 1, time%step
            ob(icmd)%ts(1,ii)%temp = 0.           !!undefined
            ob(icmd)%ts(1,ii)%flo = resflwo / real(time%step)
            ob(icmd)%ts(1,ii)%sed = ressedo / real(time%step)
            ob(icmd)%ts(1,ii)%orgn = resorgno / real(time%step)
            ob(icmd)%ts(1,ii)%sedp = resorgpo / real(time%step)
            ob(icmd)%ts(1,ii)%no3 = resno3o / real(time%step)
            ob(icmd)%ts(1,ii)%solp = ressolpo / real(time%step)
            ob(icmd)%ts(1,ii)%chla = reschlao / real(time%step)
            ob(icmd)%ts(1,ii)%nh3 = resnh3o / real(time%step)
            ob(icmd)%ts(1,ii)%no2 = resno2o / real(time%step)
            ob(icmd)%ts(1,ii)%cbod = 0.          !!CBOD
            ob(icmd)%ts(1,ii)%dox = 0.          !!dis O2
          end do
        end if

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
          
          res_in_d(jres)%flo = res(jres)%flo / 10000.  !m^3 -> ha-m
          res_out_d(jres)%flo = res(jres)%flo / 10000.  !m^3 -> ha-m
          res_in_d(jres)%sed = ressedi 
          res_out_d(jres)%sed = ressedo
          res_in_d(jres)%orgn = orgni
          res_out_d(jres)%orgn = orgno
          res_in_d(jres)%sedp = orgpi
          res_out_d(jres)%sedp = orgpo
          res_in_d(jres)%no3 = no3i
          res_out_d(jres)%no3 = no3o
          res_in_d(jres)%no2 = no2i
          res_out_d(jres)%no2 = no2o
          res_in_d(jres)%nh3 = nh3i
          res_out_d(jres)%nh3 = nh3o
          res_in_d(jres)%solp = solpi
          res_out_d(jres)%solp = solpo
          res_in_d(jres)%chla = chlai
          res_out_d(jres)%chla = chlao
          res_in_d(jres)%cbod = cbodi
          res_out_d(jres)%cbod = cbodo

          !res_d(jres)%area_ha = res_ob(jres)%area_ha
          !res_d(jres)%ev = resev / 10000.           !m^3 -> ha-m
          !res_d(jres)%sep = ressep / 10000.         !m^3 -> ha-m
          !res_d(jres)%pcp = respcp / 10000.         !m^3 -> ha-m
          !res_d(jres)%sedcon = sedcon
          !res_d(jres)%reactw = reactw
          !res_d(jres)%volatpst = volatpst
          !res_d(jres)%setlpst = setlpst
          !res_d(jres)%resuspst = resuspst
          !res_d(jres)%difus = difus
          !res_d(jres)%reactb = reactb
          !res_d(jres)%pesto = pesto
          !res_d(jres)%pstcon = pstcon
          !res_d(jres)%spstcon = spstcon
          !res_d(jres)%orgpc = orgpc
          !res_d(jres)%solpc = solpc
          !res_d(jres)%orgnc = orgnc
          !res_d(jres)%no3c = no3c
          !res_d(jres)%no2c = no2c
          !res_d(jres)%nh3c = nh3c
        end if             
        
      else
        !! reservoir has not been constructed yet
        ob(icmd)%hd(1) = ob(icmd)%hin
      end if

      return
      end subroutine res_control