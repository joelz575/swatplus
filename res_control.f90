      subroutine res_control (jres)
      
      use basin_module
      use reservoir_data_module 
      use time_module
      use hru_module, only : pet_day, resclai, resgrai, reslagi, ressagi, ressani, ressili, hru
      use reservoir_module
      use climate_module
      use hydrograph_module, only : ob, res, hd, icmd, iwst
      
      implicit none
      
      integer :: k                    !              | 
      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      real :: resorgnc                !              | 
      real :: solpesti                !              |soluble pesticide 
      integer :: idat                 !              |
      integer :: ihyd                 !none          |counter
      integer :: ised                 !none          |counter
      integer :: irel                 !              |
      integer :: inut                 !none          |counter
      integer :: ipst                 !none          |counter
      integer :: inum2                !none          |inflow hydrograph storage location number 
      real :: pesti                   !              |
      real :: pesto                   !              |
      real :: pstcon                  !mg pst/m^3    |pest conc in res water
      real :: spstcon                 !mg pst/m^3    |pest conc in res sed layer 
      real :: sorpesti                !              |
      real :: orgni = 0.              !kg N          |org N entering res
      real :: orgno = 0.              !kg N          |org N leaving res
      real :: orgpi = 0.              !kg P          |org P entering res
      real :: orgpo = 0.              !kg P          |org P leaving res
      real :: no3i = 0.               !kg N          |nitrate N entering res
      real :: no3o = 0.               !kg N          |nitrate N leaving res
      real :: no2i = 0.               !kg N          |nitrite entering res
      real :: no2o = 0.               !kg N          |nitrite leaving res
      real :: nh3i = 0.               !kg N          |ammonia entering res
      real :: nh3o = 0.               !kg N          |ammonia leaving res
      real :: solpi = 0.              !kg P          |mineral P entering res
      real :: solpo = 0.              !kg P          |mineral P leaving res
      real :: chlai = 0.              !kg chla       |chlorophyll-a entering res 
      real :: chlao = 0.              !kg chla       |chlorophyll-a leaving res 
      real :: orgpc = 0.              !mg P/L        |ave org P conc in res
      real :: solpc = 0.              !mg P/L        |ave sol P conc in res
      real :: orgnc = 0.              !mg N/L        |ave org N in res
      real :: no3c = 0.               !mg N/L        |ave nitrate conc in res
      real :: no2c = 0.               !mg N/L        |ave nitrite conc in res
      real :: nh3c = 0.               !mg N/L        |ave ammonia conc in res
   
      !! initialize variables for reservoir daily simulation
      call res_dayinit

      resflwi = ob(icmd)%hin%flo
      pet_day = wst(iwst)%weat%pet
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
      solpesti = ob(icmd)%hin%psol
      sorpesti = ob(icmd)%hin%psor

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
          call conditions (irel, ihyd)
          !call actions (irel, jres)
          call res_hydro (jres, irel, ihyd, ised)
          call res_sediment (jres, ihyd, ised)
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
        ob(icmd)%hd(1)%flo = resflwo
        ob(icmd)%hd(1)%sed = ressedo
        ob(icmd)%hd(1)%orgn = resorgno
        ob(icmd)%hd(1)%sedp = resorgpo
        ob(icmd)%hd(1)%no3 = resno3o
        ob(icmd)%hd(1)%solp = ressolpo
        ob(icmd)%hd(1)%psol = solpesto
        ob(icmd)%hd(1)%psor = sorpesto
        ob(icmd)%hd(1)%chla = reschlao
        ob(icmd)%hd(1)%nh3 = resnh3o
        ob(icmd)%hd(1)%no2 = resno2o
        ob(icmd)%hd(1)%cbod = 0.                    !!CBOD
        ob(icmd)%hd(1)%dox = 0.                     !!dissolved O2
        ob(icmd)%hd(1)%bacp = ob(icmd)%hin%bacp     !!persistent bact
        ob(icmd)%hd(1)%baclp = ob(icmd)%hin%baclp   !!less persistent bact
        ob(icmd)%hd(1)%met1 = ob(icmd)%hin%met1     !!conservative metal #1
        ob(icmd)%hd(1)%met2 = ob(icmd)%hin%met2     !!conservative metal #2
        ob(icmd)%hd(1)%met3 = ob(icmd)%hin%met3     !!conservative metal #3

        if (time%step > 0) then
          do ii = 1, time%step
            ob(icmd)%ts(1,ii)%temp = 0.           !!undefined
            ob(icmd)%ts(1,ii)%flo = resflwo / real(time%step)
            ob(icmd)%ts(1,ii)%sed = ressedo / real(time%step)
            ob(icmd)%ts(1,ii)%orgn = resorgno / real(time%step)
            ob(icmd)%ts(1,ii)%sedp = resorgpo / real(time%step)
            ob(icmd)%ts(1,ii)%no3 = resno3o / real(time%step)
            ob(icmd)%ts(1,ii)%solp = ressolpo / real(time%step)
            ob(icmd)%ts(1,ii)%psol = solpesto / real(time%step)
            ob(icmd)%ts(1,ii)%psor = sorpesto / real(time%step)
            ob(icmd)%ts(1,ii)%chla = reschlao / real(time%step)
            ob(icmd)%ts(1,ii)%nh3 = resnh3o / real(time%step)
            ob(icmd)%ts(1,ii)%no2 = resno2o / real(time%step)
            ob(icmd)%ts(1,ii)%cbod = 0.          !!CBOD
            ob(icmd)%ts(1,ii)%dox = 0.          !!dis O2
            ob(icmd)%ts(1,ii)%bacp = hd(inum2)%bacp / real(time%step) !!persistent bact
            ob(icmd)%ts(1,ii)%baclp = hd(inum2)%baclp / real(time%step) !!less persist bact
            ob(icmd)%ts(1,ii)%met1 = hd(inum2)%met1 / real(time%step) !!cons metal #1
            ob(icmd)%ts(1,ii)%met2 = hd(inum2)%met2 / real(time%step) !!cons metal #2
            ob(icmd)%ts(1,ii)%met3 = hd(inum2)%met3 / real(time%step) !!cons metal #3

            ob(icmd)%ts(1,ii)%san = hd(inum2)%san / real(time%step) !!Sand out
            ob(icmd)%ts(1,ii)%sil = hd(inum2)%sil / real(time%step) !!Silt out
            ob(icmd)%ts(1,ii)%cla = hd(inum2)%cla / real(time%step) !!clay out
            ob(icmd)%ts(1,ii)%sag = hd(inum2)%sag / real(time%step) !!Small agg out
            ob(icmd)%ts(1,ii)%lag = hd(inum2)%lag/ real(time%step) !!Large agg out
            ob(icmd)%ts(1,ii)%grv = hd(inum2)%grv / real(time%step) !!Gravel out

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
          
          res_d(jres)%vol = res(jres)%flo / 10000.  !m^3 -> ha-m 
          res_d(jres)%area_ha = res_ob(jres)%area_ha
          res_d(jres)%flowi = resflwi / 10000.      !m^3 -> ha-m
          res_d(jres)%flowo = resflwo / 10000.      !m^3 -> ha-m
          res_d(jres)%ev = resev / 10000.           !m^3 -> ha-m
          res_d(jres)%sep = ressep / 10000.         !m^3 -> ha-m
          res_d(jres)%pcp = respcp / 10000.         !m^3 -> ha-m
          res_d(jres)%sedi = ressedi 
          res_d(jres)%sedo = ressedo
          res_d(jres)%sedcon = sedcon
          res_d(jres)%pesti = pesti
          res_d(jres)%reactw = reactw
          res_d(jres)%volatpst = volatpst
          res_d(jres)%setlpst = setlpst
          res_d(jres)%resuspst = resuspst
          res_d(jres)%difus = difus
          res_d(jres)%reactb = reactb
          res_d(jres)%pesto = pesto
          res_d(jres)%pstcon = pstcon
          res_d(jres)%spstcon = spstcon
          res_d(jres)%orgni = orgni
          res_d(jres)%orgno = orgno
          res_d(jres)%orgpi = orgpi
          res_d(jres)%orgpo = orgpo
          res_d(jres)%no3i = no3i
          res_d(jres)%no3o = no3o
          res_d(jres)%no2i = no2i
          res_d(jres)%no2o = no2o
          res_d(jres)%nh3i = nh3i
          res_d(jres)%nh3o = nh3o
          res_d(jres)%solpi = solpi
          res_d(jres)%solpo = solpo
          res_d(jres)%chlai = chlai
          res_d(jres)%chlao = chlao
          res_d(jres)%orgpc = orgpc
          res_d(jres)%solpc = solpc
          res_d(jres)%orgnc = orgnc
          res_d(jres)%no3c = no3c
          res_d(jres)%no2c = no2c
          res_d(jres)%nh3c = nh3c
        end if             
        
      else
        !! reservoir has not been constructed yet
        ob(icmd)%hd(1) = ob(icmd)%hin
      end if

      return
      end subroutine res_control