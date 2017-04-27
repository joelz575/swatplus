      subroutine pl_grow
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adjusts plant biomass, leaf area index, and canopy height
!!    taking into account the effect of water, temperature and nutrient stresses
!!    on the plant

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blai(:)     |none             |maximum (potential) leaf area index
!!    auto_nstrs(:) |none           |nitrogen stress factor which triggers
!!                                  |auto fertilization
!!    bio_e(:)    |(kg/ha)/(MJ/m**2)|biomass-energy ratio
!!                                  |The potential (unstressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    bio_targ(:,:,:)|kg/ha          |biomass target
!!    chtmx(:)    |m                |maximum canopy height
!!    curyr       |none             |current year of simulation
!!    dlai(:)     |none             |fraction of growing season when leaf
!!                                  |area declines
!!    ep_day      |mm H2O           |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day      |mm H2O           |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    hru_dafr(:) |km**2/km**2      |fraction of watershed area in HRU
!!    hru_ra(:)   |MJ/m^2           |solar radiation for the day in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)  |harvest index: crop yield/aboveground
!!                                  |biomass
!!    icr(:)      |none             |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)      |none             |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    ihru        |none             |HRU number
!!    lai_yrmx(:) |none             |maximum leaf area index for the year in the
!!                                  |HRU
!!    leaf1(:)    |none             |1st shape parameter for leaf area
!!                                  |development equation.
!!    leaf2(:)    |none             |2nd shape parameter for leaf area
!!                                  |development equation.
!!    pet_day     |mm H2O           |potential evapotranspiration on current day
!!                                  |in HRU
!!    t_base(:)   |deg C            |minimum temperature for plant growth
!!    vpd         |kPa              |vapor pressure deficit
!!    ruc1(:)    |none             |1st shape parameter for radiation use
!!                                  |efficiency equation.
!!    ruc2(:)    |none             |2nd shape parameter for radiation use
!!                                  |efficiency equation.
!!    wavp(:)     |none             |Rate of decline in radiation use efficiency
!!                                  |as a function of vapor pressure deficit
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bioday      |kg            |biomass generated on current day in HRU
!!    cht(:)      |m             |canopy height
!!    lai_yrmx(:) |none          |maximum leaf area index for the year in the
!!                               |HRU
!!    rsr1c(:)    |              |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |              |root to shoot ratio at end of growing season
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    beadj       |(kg/ha)/(MJ/m**2)|radiation-use efficiency for a given CO2
!!                                  |concentration
!!    delg        |
!!    deltalai    |
!!    f           |none             |fraction of plant's maximum leaf area index
!!                                  |corresponding to a given fraction of
!!                                  |potential heat units for plant
!!    ff          |
!!    j           |none             |HRU number
!!    laimax      |none             |maximum leaf area index
!!    par         |MJ/m^2           |photosynthetically active radiation
!!    reg         |none             |stress factor that most limits plant growth
!!                                  |on current day
!!    ruedecl     |none             |decline in radiation use efficiency for the
!!                                  |plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min, Sqrt
!!    SWAT: tstr, nup, npup, anfert

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      
      integer :: j
      real :: delg, ruedecl, beadj, reg, f, ff, deltalai
      real :: laimax, lai_exp

      j = ihru
      rto = 1.

      do ipl = 1, pcom(j)%npl
        !! plant will not undergo stress if dormant
        if (pcom(j)%plcur(ipl)%idorm == 0 .and. pcom(j)%plcur(ipl)%gro == 1) then
        idp = pcom(j)%plcur(ipl)%idplt
 
        !! if plant hasn't reached maturity
        if (pcom(j)%plcur(ipl)%phuacc <= 1.) then
          if (uno3d_tot > sum_no3) then
            rto_no3 = uno3d(ipl) / uno3d_tot
          else 
            rto_no3 = 1.
          end if
          if (uapd_tot > sum_solp) then
            rto_solp = uapd(ipl) / uapd_tot
          else
            rto_solp = 1.
          end if
       
          !! compute temperature stress    
          call pl_tstr

          !! calculate optimal biomass
          !! adjust radiation-use efficiency for CO2
          if (hru(j)%parms%co2 > 330.) then
            beadj = 100. * hru(j)%parms%co2 / (hru(j)%parms%co2 +        &
                Exp(plcp(idp)%ruc1 - hru(j)%parms%co2 * plcp(idp)%ruc2))
          else
            beadj = pldb(idp)%bio_e
          end if

          !! adjust radiation-use efficiency for vapor pressure deficit
          !!assumes vapor pressure threshold of 1.0 kPa
          if (vpd > 1.0) then
            ruedecl = vpd - 1.0
            beadj = beadj - pldb(idp)%wavp * ruedecl
            beadj = Max(beadj, 0.27 * pldb(idp)%bio_e)
          end if

          beadj = pldb(idp)%bio_e
          bioday = beadj * par(ipl)
          if (bioday < 0.) bioday = 0.

          !! auto fertilization-nitrogen demand (non-legumes only)
          select case (pldb(idp)%idc)
            case (4, 5, 6, 7)    !non-legumes
            if (auto_nstrs(j) > 0.) then
              call pl_anfert
            end if
          end select

          uno3d(ipl) = Min(4. * pldb(idp)%pltnfr3 * bioday, uno3d(ipl))
          if (uapd(ipl) > 10.) then
            uapd(ipl) = Min(4. * pldb(idp)%pltpfr3 * bioday, uapd(ipl))
          end if
          !! uno3d(ipl) = uno3d(ipl) * rto_no3
          !! uapd(ipl) = uapd(ipl) * rto_solp
          call pl_nup
          call pl_pup

          !! new code to turn off nutrient stress
          if (bsn_cc%nostress == 1) then
            pcom(j)%plstr(ipl)%strsn = 1.
            pcom(j)%plstr(ipl)%strsp = 1.
            pcom(j)%plstr(ipl)%strsa = 1.
          end if
          
          !! reduce predicted biomass due to stress on plant
          reg = Min(pcom(j)%plstr(ipl)%strsw, pcom(j)%plstr(ipl)%strst,                     &
            pcom(j)%plstr(ipl)%strsn, pcom(j)%plstr(ipl)%strsp, pcom(j)%plstr(ipl)%strsa)
          if (reg < 0.) reg = 0.
          if (reg > 1.) reg = 1.

          if (bio_targ(j) > 1.e-2) then
            bioday = bioday * (bio_targ(j) - pcom(j)%plm(ipl)%mass) / bio_targ(j)
            reg = 1.
          end if
 
          pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass + bioday * reg

          !!maximum lai and bioimass for perrenials
          select case (pldb(idp)%idc)
            case (3, 6, 7)  ! all perenials
            if (pldb(idp)%mat_yrs > 0) then
              rto = float(pcom(j)%plcur(ipl)%curyr_mat + 1) / float(pldb(idp)%mat_yrs)
              rto = Min(rto, 1.)
            else
              rto = 1.
            end if
            biomxyr = rto * pldb(idp)%bmx_peren * 1000.  !t/ha -> kg/ha
            if (biomxyr > 1.e-6 .and. pcom(j)%plm(ipl)%mass > biomxyr) then
              pcom(j)%plm(ipl)%mass = biomxyr
            end if
          end select

          pcom(j)%plm(ipl)%mass = Max(pcom(j)%plm(ipl)%mass,0.)

          !!add by zhang
          !!============
          if (bsn_cc%cswat == 2) then
            NPPC_d(j) = NPPC_d(j) + bioday * reg* 0.42
          end if
          !!add by zhang
          !!============          
          
          !! calculate fraction of total biomass that is in the roots
          pcom(j)%plg(ipl)%rwt = .4 - .2 * pcom(j)%plcur(ipl)%phuacc

          f = pcom(j)%plcur(ipl)%phuacc/(pcom(j)%plcur(ipl)%phuacc +     &
              Exp(plcp(idp)%leaf1 - plcp(idp)%leaf2 * pcom(j)%plcur(ipl)%phuacc))
          ff = f - pcom(j)%plg(ipl)%laimxfr
          pcom(j)%plg(ipl)%laimxfr = f

          !! calculate new canopy height
          if (pldb(idp)%idc == 7) then
            pcom(j)%plg(ipl)%cht = rto * pldb(idp)%chtmx
          else
            pcom(j)%plg(ipl)%cht = pldb(idp)%chtmx * Sqrt(f)
          end if

          !! calculate new leaf area index
          if (pcom(j)%plcur(ipl)%phuacc <= pldb(idp)%dlai) then
            laimax = 0.
            deltalai = 0.
            if (pldb(idp)%idc == 7) then
              if (pcom(j)%plcur(ipl)%curyr_mat < 1) pcom(j)%plcur(ipl)%curyr_mat = 1
              rto = float(pcom(j)%plcur(ipl)%curyr_mat) / float(pldb(idp)%mat_yrs)
              rto = alog10 (rto)
              lai_exp = rto * pldb(idp)%laixco_tree
              laimax = pcom(j)%plg(ipl)%laimx_pop * 10. ** lai_exp
            else
              laimax = pcom(j)%plg(ipl)%laimx_pop
            end if
            
            !! calculate fraction of above ground tree biomass that is leaf
            if (pldb(idp)%idc == 7) then
              pcom(j)%plg(ipl)%bio_leaf = .4 - (.3 * float(pcom(j)%plcur(ipl)%curyr_mat) - 1.) /  &
                    (float(pldb(idp)%mat_yrs) - 1.)
            end if
            
            if (pcom(j)%plg(ipl)%lai > laimax) pcom(j)%plg(ipl)%lai = laimax
            deltalai = ff * laimax * (1.0 - Exp(5.0 * (pcom(j)%plg(ipl)%lai - laimax))) * Sqrt(reg)
            !! adjust lai increment for plant competition
            sumlaiht = 0.
            do jpl = 1, pcom(j)%npl
              sumlaiht=sumlaiht+pcom(j)%plg(ipl)%lai * pcom(j)%plg(jpl)%cht
            end do
            if (sumlaiht > 1.e-6) then
              rto = (pcom(j)%plg(ipl)%lai * pcom(j)%plg(ipl)%cht) / sumlaiht
            else
              rto = 1.
            end if
            deltalai = deltalai * rto
            pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai + deltalai
            
            if (pcom(j)%plg(ipl)%lai > laimax) pcom(j)%plg(ipl)%lai = laimax
            pcom(j)%plg(ipl)%olai = pcom(j)%plg(ipl)%lai
            if (sumlai > lai_yrmx(j)) lai_yrmx(j) = sumlai
          else
            !! logistic decline rate - Michael Strauch
            rto = (1. - pcom(j)%plcur(ipl)%phuacc)/(1. - pldb(idp)%dlai)
            !pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%olai * rto
            pcom(j)%plg(ipl)%lai = (pcom(j)%plg(ipl)%olai - pldb(idp)%alai_min) /   &
                (1. + Exp((rto - .5) * -12)) + pldb(idp)%alai_min
          end if
          if (pcom(j)%plg(ipl)%lai < pldb(idp)%alai_min) then   !Sue White dormancy
            pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
          end if
          
          !! calculate plant ET values
          if (pcom(j)%plcur(ipl)%phuacc > 0.5 .and. pcom(j)%plcur(ipl)%phuacc < pldb(idp)%dlai) then 
            pcom(j)%plg(ipl)%plet = pcom(j)%plg(ipl)%plet + ep_day + es_day
            pcom(j)%plg(ipl)%plpet = pcom(j)%plg(ipl)%plpet + pet_day
          end if

          pcom(j)%plg(ipl)%hvstiadj = pldb(idp)%hvsti * 100. * pcom(j)%plcur(ipl)%phuacc /          &
                (100. * pcom(j)%plcur(ipl)%phuacc + Exp(11.1 - 10. * pcom(j)%plcur(ipl)%phuacc))
          

!!  added per JGA for Srini by gsm 9/8/2011
          strsw_sum(j) = strsw_sum(j) + (1. - pcom(j)%plstr(ipl)%strsw)
          strstmp_sum(j) = strstmp_sum(j)+(1.-pcom(j)%plstr(ipl)%strst)
          strsn_sum(j) = strsn_sum(j) + (1. - pcom(j)%plstr(ipl)%strsn)
          strsp_sum(j) = strsp_sum(j) + (1. - pcom(j)%plstr(ipl)%strsp) 
          strsa_sum(j) = strsa_sum(j) + (1. - pcom(j)%plstr(ipl)%strsa)
        end if
        end if  ! if plant is growing
      end do    ! loop for number of plants
      return
      end subroutine pl_grow