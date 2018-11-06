      subroutine stor_surf
    
      use reservoir_data_module
      use reservoir_module
      use hru_module, only : hru, sedyld, sanyld, silyld, clayld, sagyld, lagyld, grayld, sedminps, sedminpa,   &
        surqno3, sedorgn, sedorgp, qdr, ihru, pet_day, qday
      use conditional_module
      use climate_module
      use hydrograph_module
      use time_module
      use basin_module
      use channel_module
      
      implicit none
     
      real :: bypass                  !              | 
      real :: fracwet                 !              | 
      integer :: j                    !none          |counter
      integer :: iprop                !              | 
      integer :: id                   !              | 
      integer :: iac                  !none          |counter
      real :: action                  !              | 
      integer :: ial                  !none          |counter
      real :: b_lo                    !              |
      real :: res_h                   !              |
      real :: x1                      !              |
      real :: wet_h                   !              |
      real :: wet_h1                  !              |
      real :: flwi                    !m^3 H2O       |water entering pothole on day  
      real :: flwo                    !              |
      real :: sedi                    !metric tons   |sediment entering pothole on day
      real :: sedo                    !metric tons   |sed leaving res 
      integer :: k                    !              | 
      integer :: ii                   !none          |counter 
      integer :: jres                 !none          |reservoir number
      integer :: idat                 !              |
      integer :: ihyd                 !none          |counter
      integer :: ised                 !none          |counter
      integer :: irel                 !              |
      integer :: inut                 !none          |counter
      integer :: ipst                 !none          |counter
      integer :: ires = 0
      integer :: ichan = 0
      real :: wet_evap = 0.
      real :: wet_hru_area = 0.
      real :: wet_fr = 0.
      real :: wet_evapt = 0.
      real :: flowi = 0.
      real :: sani = 0.               !metric tons   |sand entering pothole on day
      real :: sili = 0.               !metric tons   |silt entering pothole on day
      real :: clai = 0.               !metric tons   |clay entering pothole on day
      real :: sagi = 0.               !metric tons   |small aggregates entering pothole on day
      real :: lagi = 0.               !metric tons   |large aggrigates entering pothole on day
      real :: grai = 0.               !metric tons   |gravel entering pothole on day
      real :: precipday = 0.
      
       j = ihru
       ires= hru(j)%dbs%surf_stor
       ichan = ob(icmd)%flood_ch_lnk
       ihyd = wet_dat(ires)%hyd
       hru(j)%water_fr = 0.
       wet_hru_area = 0.
      !! initialize variables for reservoir daily simulation

       hru(ihru)%water_seep = 0.
       wet_evap = 0.  
       
      !! initialize variables for reservoir daily simulation
      call res_dayinit
      
      bypass = 1. - wet_hyd(ihyd)%frac
      fracwet = 1. - bypass 
      fracwet = max (fracwet,0.)
      resflwi = qday * fracwet
      respcp = precipday
      
      ressedi = sedyld(ihru) * fracwet
      ressani = sanyld(ihru) * fracwet
      ressili = silyld(ihru) * fracwet
	  resclai = clayld(ihru) * fracwet 
	  ressagi = sagyld(ihru) * fracwet
	  reslagi = lagyld(ihru) * fracwet
	  resgrai = grayld(ihru) * fracwet
      solpesti = 0.   !add soluble pest
      sorpesti = 0.   !add sorbed pest

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      wet(ires)%orgn = wet(ires)%orgn + sedorgn(ihru) * fracwet
      wet(ires)%sedp = wet(ires)%sedp + sedorgp(ihru) * fracwet
      wet(ires)%no3 = wet(ires)%no3 + surqno3(ihru) * fracwet
      wet(ires)%nh3 = wet(ires)%nh3 + 0.  !add ammonium 
      wet(ires)%no2 = wet(ires)%no2 + 0.  !add no2
      wet(ires)%solp = wet(ires)%solp + (sedminps(ihru) + sedminpa(ihru)) * fracwet

      ised = wet_dat(idat)%sed
      id = wet_dat(idat)%release
      
      id = 0    !!jeff take a look

        !calc release from decision table
        do iac = 1, dtbl_res(id)%acts
          action = "n"
          do ial = 1, dtbl_res(id)%alts
            if (dtbl_res(id)%act_hit(ial) == "y" .and. dtbl_res(id)%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == "y") then
            select case (dtbl_res(id)%act(iac)%option)
            case ("rate")
              resflwo = dtbl_res(id)%act(iac)%const * 86400.
            case ("days")
              select case (dtbl_res(id)%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = res_ob(ihyd)%pvol
                case ("evol")
                  b_lo = res_ob(ihyd)%evol
              end select
              resflwo = (res(ires)%flo - b_lo) / dtbl_res(id)%act(iac)%const
            case ("weir")
              resflwo = res_weir(ihyd)%c * res_weir(ihyd)%k * res_weir(ihyd)%w * (res_h ** 1.5)
            case ("meas")
              irel = int(dtbl_res(id)%act(iac)%const)
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
      wet_hru_area= 0.
      wet_evap = 0.
      if (wet(ires)%flo > 0.) then
          x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ires)%flo / wet_ob(ires)%pvol)
          if (x1 < 1.e-6) then
            wet_h = 0.
          else
            wet_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
            wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
          end if
          wet_fr = (1. + wet_hyd(ihyd)%acoef * wet_h)
          wet_fr= min(wet_fr,1.)
          wet_hru_area = hru(ihru)%area_ha * wet_hyd(ihyd)%psa * wet_fr
                
         hru(ihru)%water_fr =  wet_hru_area / hru(ihru)%area_ha
         res_om_d(ires)%area_ha = wet_hru_area
         
    !!   compute evaporation

          call water_hru
          wet_evapt = pet_day*wet_hyd(ihyd)%evrsv * wet_hru_area * 10. 
          wet_evap = min(wet_evapt, wet(ires)%flo)
          wet(ires)%flo =  wet(ires)%flo - wet_evap
          hru(ihru)%water_seep = wet_hru_area * wet_hyd(ihyd)%k * 10.* 24.  
          hru(ihru)%water_seep = min(wet(ires)%flo, hru(ihru)%water_seep)
          wet(ires)%flo = wet(ires)%flo - hru(ihru)%water_seep 
      end if 
 
      !! perform reservoir nutrient balance
      inut = res_dat(idat)%nut
      !call res_nutrient (ires, inut)

      !! perform reservoir pesticide transformations
      ipst = res_dat(idat)%pst
      !call res_pest (ires)

      !! set values for routing variables
      ob(icmd)%hd(1)%temp = 0.                  !!undefined
      !! this line needs to be checked
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
          resorgnc = wet(ires)%orgn / (wet(ires)%flo+.1) * 1000.
          resno3c = wet(ires)%no3 / (wet(ires)%flo+.1) * 1000.
          resno2c = wet(ires)%no2 / (wet(ires)%flo+.1) * 1000.
          resnh3c = wet(ires)%nh3 / (wet(ires)%flo+.1) * 1000.
          resorgpc = wet(ires)%sedp / (wet(ires)%flo+.1) * 1000.
          ressolpc = wet(ires)%solp / (wet(ires)%flo+.1) * 1000.
          sedcon = wet(ires)%sed * 1.e6
          
          wet_in_d(jres)%flo = wet(jres)%flo / 10000.  !m^3 -> ha-m
          wet_out_d(jres)%flo = wet(jres)%flo / 10000.  !m^3 -> ha-m
          wet_in_d(jres)%sed = ressedi 
          wet_out_d(jres)%sed = ressedo
          wet_in_d(jres)%orgn = orgni
          wet_out_d(jres)%orgn = orgno
          wet_in_d(jres)%sedp = orgpi
          wet_out_d(jres)%sedp = orgpo
          wet_in_d(jres)%no3 = no3i
          wet_out_d(jres)%no3 = no3o
          wet_in_d(jres)%no2 = no2i
          wet_out_d(jres)%no2 = no2o
          wet_in_d(jres)%nh3 = nh3i
          wet_out_d(jres)%nh3 = nh3o
          wet_in_d(jres)%solp = solpi
          wet_out_d(jres)%solp = solpo
          wet_in_d(jres)%chla = chlai
          wet_out_d(jres)%chla = chlao
          wet_in_d(jres)%cbod = cbodi
          wet_out_d(jres)%cbod = cbodo

        end if             
        
      return
      end subroutine stor_surf