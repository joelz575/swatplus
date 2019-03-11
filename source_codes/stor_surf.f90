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
      character(len=1) :: action           !         |
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
       ised = wet_dat(ires)%sed
       id = wet_dat(ires)%release
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

      !! add incoming nutrients to those in reservoir
      !! equation 29.1.1 in SWAT manual
      wet(ihru)%orgn = wet(ihru)%orgn + sedorgn(ihru) * fracwet
      wet(ihru)%sedp = wet(ihru)%sedp + sedorgp(ihru) * fracwet
      wet(ihru)%no3 = wet(ihru)%no3 + surqno3(ihru) * fracwet
      wet(ihru)%nh3 = wet(ihru)%nh3 + 0.  !add ammonium 
      wet(ihru)%no2 = wet(ihru)%no2 + 0.  !add no2
      wet(ihru)%solp = wet(ihru)%solp + (sedminps(ihru) + sedminpa(ihru)) * fracwet

        !calc release from decision table
        d_tbl => dtbl_res(id)
        call conditions (ihru)
        do iac = 1, d_tbl%acts
          action = "n"
          do ial = 1, dtbl_res(id)%alts
            if (d_tbl%act_hit(ial) == "y" .and. d_tbl%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == "y") then
            select case (d_tbl%act(iac)%option)
            case ("rate")
              resflwo = d_tbl%act(iac)%const * 86400.
            case ("days")
              select case (d_tbl%act(iac)%file_pointer)
                case ("null")
                  b_lo = 0.
                case ("pvol")
                  b_lo = wet_ob(ihru)%pvol
                case ("evol")
                  b_lo = wet_ob(ihru)%evol
              end select
              resflwo = (wet(ihru)%flo - b_lo) / d_tbl%act(iac)%const
            case ("weir")
              resflwo = res_weir(ihyd)%c * res_weir(ihyd)%k * res_weir(ihyd)%w * (res_h ** 1.5)
            case ("meas")
              irel = int(d_tbl%act(iac)%const)
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
      if (wet(ihru)%flo > 0.) then
          x1 = wet_hyd(ihyd)%bcoef ** 2 + 4. * wet_hyd(ihyd)%ccoef * (1. - wet(ihru)%flo / wet_ob(ihru)%pvol)
          if (x1 < 1.e-6) then
            wet_h = 0.
          else
            wet_h1 = (-wet_hyd(ihyd)%bcoef - sqrt(x1)) / (2. * wet_hyd(ihyd)%ccoef)
            wet_h = wet_h1 + wet_hyd(ihyd)%bcoef
          end if
          wet_fr = (1. + wet_hyd(ihyd)%acoef * wet_h)
          wet_fr = min(wet_fr,1.)
          wet_hru_area = hru(ihru)%area_ha * wet_hyd(ihyd)%psa * wet_fr
                
         hru(ihru)%water_fr =  wet_hru_area / hru(ihru)%area_ha
         wet_om_d(ihru)%area_ha = wet_hru_area
         
         !! subtract outflow from storage
         wet(ihru)%flo =  wet(ihru)%flo - resflwo
         
          !! compute evaporation and seepage
          wet_evapt = pet_day * wet_hyd(ihyd)%evrsv * wet_hru_area * 10. 
          wet_evap = min(wet_evapt, wet(ihru)%flo)
          wet(ihru)%flo =  wet(ihru)%flo - wet_evap
          hru(ihru)%water_seep = wet_hru_area * wet_hyd(ihyd)%k * 10.* 24.  
          hru(ihru)%water_seep = min(wet(ihru)%flo, hru(ihru)%water_seep)
          wet(ihru)%flo = wet(ihru)%flo - hru(ihru)%water_seep 
      end if 
 
      !! perform reservoir nutrient balance
      inut = wet_dat(ires)%nut
      !call res_nutrient (ires, inut)

      !! perform reservoir pesticide transformations
      ipst = wet_dat(ires)%pst
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
          resorgnc = wet(ihru)%orgn / (wet(ihru)%flo + .1) * 1000.
          resno3c = wet(ihru)%no3 / (wet(ihru)%flo + .1) * 1000.
          resno2c = wet(ihru)%no2 / (wet(ihru)%flo + .1) * 1000.
          resnh3c = wet(ihru)%nh3 / (wet(ihru)%flo + .1) * 1000.
          resorgpc = wet(ihru)%sedp / (wet(ihru)%flo + .1) * 1000.
          ressolpc = wet(ihru)%solp / (wet(ihru)%flo + .1) * 1000.
          sedcon = wet(ihru)%sed * 1.e6
          
          wet_in_d(ihru)%flo = wet(ihru)%flo / 10000.  !m^3 -> ha-m
          wet_out_d(ihru)%flo = wet(ihru)%flo / 10000.  !m^3 -> ha-m
          wet_in_d(ihru)%sed = ressedi 
          wet_out_d(ihru)%sed = ressedo
          wet_in_d(ihru)%orgn = orgni
          wet_out_d(ihru)%orgn = orgno
          wet_in_d(ihru)%sedp = orgpi
          wet_out_d(ihru)%sedp = orgpo
          wet_in_d(ihru)%no3 = no3i
          wet_out_d(ihru)%no3 = no3o
          wet_in_d(ihru)%no2 = no2i
          wet_out_d(ihru)%no2 = no2o
          wet_in_d(ihru)%nh3 = nh3i
          wet_out_d(ihru)%nh3 = nh3o
          wet_in_d(ihru)%solp = solpi
          wet_out_d(ihru)%solp = solpo
          wet_in_d(ihru)%chla = chlai
          wet_out_d(ihru)%chla = chlao
          wet_in_d(ihru)%cbod = cbodi
          wet_out_d(ihru)%cbod = cbodo

        end if             
        
      return
      end subroutine stor_surf