      subroutine pl_leaf_senes
      
      use plant_data_module
      use basin_module
      use hru_module, only : hru, uapd, uno3d, lai_yrmx, par, bioday, ep_day, es_day,              &
         ihru, ipl, pet_day, rto_no3, rto_solp, sum_no3, sum_solp, uapd_tot, uno3d_tot, vpd
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      use climate_module
      use hydrograph_module
      
      implicit none 
      
      integer :: j              !none               |HRU number
      integer :: idp            !                   |
      integer :: iob            !                   |
      real :: rto               !none               |ratio of current years of growth:years to maturity of perennial
      real :: biomxyr
               
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
      !! lai decline for annuals - if dlai < phuacc < 1
      if (pldb(idp)%typ == "warm_annual" .or. pldb(idp)%typ == "cold_annual") then
        if (pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .and. pcom(j)%plcur(ipl)%phuacc < 1.) then
          rto = (1. - pcom(j)%plcur(ipl)%phuacc) / (1. - pldb(idp)%dlai)
          pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%olai * rto ** pldb(idp)%dlai_rate
        end if
      end if
      
      !! lai decline for temperture based perennials - use annual base zero phu's
      if (pldb(idp)%typ == "perennial" .and. pldb(idp)%trig == "temp_gro") then
        if (wst(iwst)%weat%phubase0 > pldb(idp)%dlai .and. wst(iwst)%weat%phubase0 < 1.) then
          iob = hru(j)%obj_no
          iwst = ob(iob)%wst
          !! logistic decline rate - Strauch and Volk
          rto = (1. - wst(iwst)%weat%phubase0) / (1. - pldb(idp)%dlai)
          pcom(j)%plg(ipl)%lai = (pcom(j)%plg(ipl)%olai - pldb(idp)%alai_min) /   &
                (1. + Exp((rto - .5) * -12)) + pldb(idp)%alai_min
        end if
      end if
      
      !! lai decline for moisture based perennials - use f(P/PET) to estimate drought stress
      if (pldb(idp)%typ == "perennial" .and. pldb(idp)%trig == "moisture_gro") then
        if (pcom(j)%plcur(ipl)%phuacc > pldb(idp)%dlai .and. pcom(j)%plcur(ipl)%phuacc < 1.) then
          iob = hru(j)%obj_no
          iwst = ob(iob)%wst
          !! logistic decline rate - Strauch and Volk
          rto = (1. - wst(iwst)%weat%phubase0) / (1. - pldb(idp)%dlai)
          pcom(j)%plg(ipl)%lai = (pcom(j)%plg(ipl)%olai - pldb(idp)%alai_min) /   &
                (1. + Exp((rto - .5) * -12)) + pldb(idp)%alai_min
        end if
      end if
          
      return
      end subroutine pl_leaf_senes