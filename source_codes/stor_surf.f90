      subroutine stor_surf
    
      use reservoir_data_module
      use reservoir_module
      use hru_module, only : hru, sedyld, sanyld, silyld, clayld, sagyld, lagyld, grayld, sedminps, sedminpa,   &
        surqno3, sedorgn, sedorgp, qdr, ihru, pet_day, qday
      use conditional_module
      use climate_module
      use hydrograph_module, only : wet, res, recall, ob, icmd
      use time_module
      use basin_module
      use channel_module
      
      implicit none
     
      real :: bypass                  !              | 
      real :: fracwet                 !              | 
      integer :: iwst                 !none          |counter
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
!!!!may need separate initialization for wetlands see sim_initday.f90
                  !!add overbank flooding to storage
 
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
        do iac = 1, d_tbl(id)%acts
          action = "n"
          do ial = 1, d_tbl(id)%alts
            if (d_tbl(id)%act_hit(ial) == "y" .and. d_tbl(id)%act_outcomes(iac,ial) == "y") then
              action = "y"
              exit
            end if
          end do
          
          !condition is met - set the release rate
          if (action == "y") then
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
              resflwo = (res(ires)%flo - b_lo) / d_tbl(id)%act(iac)%const
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
         wet_ob(ires)%area_ha = wet_ob(ires)%area_ha + wet_hru_area
         
    !!   compute evaporation

          call water_hru
          wet_evapt = pet_day*wet_hyd(ihyd)%evrsv * wet_hru_area * 10. 
          wet_evap = min(wet_evapt, wet(ires)%flo)
          wet(ires)%flo =  wet(ires)%flo - wet_evap
          hru(ihru)%water_seep = wet_hru_area * wet_hyd(ihyd)%k * 10.* 24.  
          hru(ihru)%water_seep = min(wet(ires)%flo, hru(ihru)%water_seep)
          wet(ires)%flo = wet(ires)%flo-hru(ihru)%water_seep 
      end if 
 
      !! perform reservoir nutrient balance
      inut = res_dat(idat)%nut
      !call res_nutrient (ires, inut)

      !! perform reservoir pesticide transformations
      ipst = res_dat(idat)%pst
      !call res_pest (ires, ipst)

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
          wet_d(ires)%vol = wet(ires)%flo  / 10000.     !m^3 -> ha-m
          wet_d(ires)%area_ha = wet_d(ires)%area_ha + wet_hru_area
          wet_d(ires)%flowi =wet_d(ires)%flowi + flowi / 10000.             !m^3 -> ha-m
 
          wet_d(ires)%flowo = wet_d(ires)%flowo + resflwo  /10000.  !m^3 -> ha-m
          wet_d(ires)%ev = wet_d(ires)%ev + wet_evap  / 10000.               !m^3 -> ha-m
          wet_d(ires)%sep = hru(ihru)%water_seep/ 10000.        !m^3 -> ha-m
          wet_d(ires)%pcp = respcp / 10000.             !m^3 -> ha-m
          wet_d(ires)%sedi = sedi 
          wet_d(ires)%sedo = sedo
          wet_d(ires)%sedcon = sedcon
          wet_d(ires)%pesti = pesti
          wet_d(ires)%reactw = reactw
          wet_d(ires)%volatpst = volatpst
          wet_d(ires)%setlpst = setlpst
          wet_d(ires)%resuspst = resuspst
          wet_d(ires)%difus = difus
          wet_d(ires)%reactb = reactb
          wet_d(ires)%pesto = pesto
          wet_d(ires)%pstcon = pstcon
          wet_d(ires)%spstcon = spstcon
          wet_d(ires)%orgni = orgni
          wet_d(ires)%orgno = orgno
          wet_d(ires)%orgpi = orgpi
          wet_d(ires)%orgpo = orgpo
          wet_d(ires)%no3i = no3i
          wet_d(ires)%no3o = no3o
          wet_d(ires)%no2i = no2i
          wet_d(ires)%no2o = no2o
          wet_d(ires)%nh3i = nh3i
          wet_d(ires)%nh3o = nh3o
          wet_d(ires)%solpi = solpi
          wet_d(ires)%solpo = solpo
          wet_d(ires)%chlai = chlai
          wet_d(ires)%chlao = chlao
          wet_d(ires)%orgpc = orgpc
          wet_d(ires)%solpc = solpc
          wet_d(ires)%orgnc = orgnc
          wet_d(ires)%no3c = no3c
          wet_d(ires)%no2c = no2c
          wet_d(ires)%nh3c = nh3c
        end if             
        
      return
      end subroutine stor_surf