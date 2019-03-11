      subroutine pl_partition
      
      use plant_data_module
      use basin_module
      use hru_module, only : hru, uapd, uno3d, lai_yrmx, par, bioday, ep_day, es_day,              &
         ihru, ipl, pet_day, rto_no3, rto_solp, sum_no3, sum_solp,uapd_tot, uno3d_tot, vpd
      use plant_module
      use carbon_module
      use organic_mineral_mass_module
      
      implicit none 
      
      integer :: j              !none               |HRU number
      integer :: idp            !                   |
      real :: root_frac         !none               |root mass fraction
      real :: ab_gr_frac        !none               |above ground mass fraction
      real :: leaf_mass_frac    !none               |leaf mass fraction of above ground biomass
      real :: stem_mass_frac    !none               |stem mass fraction of above ground biomass
      real :: seed_mass_frac    !none               |stem mass fraction of above ground biomass
      real :: n_left            !none               |n left after seed is removed
      real :: n_frac            !none               |n fraction in remainder of plant
      real :: p_left            !none               |p left after seed is removed
      real :: p_frac            !none               |p fraction in remainder of plant
           
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt
      
          !! partition root and above ground biomass
          pcom(j)%plm(ipl)%mass = Max(pcom(j)%plm(ipl)%mass,0.)
          root_frac = pcom(j)%plg(ipl)%root_frac
          ab_gr_frac = 1. - root_frac
          pcom(j)%ab_gr(ipl)%mass = ab_gr_frac * pcom(j)%plm(ipl)%mass
          pcom(j)%root(ipl)%mass = root_frac * pcom(j)%plm(ipl)%mass

          !! partition leaf and stem (wood/stalk) and seed (grain) mass
          leaf_mass_frac = 0.03 * pcom(j)%plg(ipl)%lai / pldb(idp)%blai     !**** 0.03 for forest - this needs to be a plant parm ****
          seed_mass_frac = pcom(j)%plg(ipl)%hvstiadj
          stem_mass_frac = 1. - leaf_mass_frac - seed_mass_frac
          stem_mass_frac = amax1 (0., stem_mass_frac)
          pcom(j)%leaf(ipl)%mass = leaf_mass_frac * pcom(j)%ab_gr(ipl)%mass
          pcom(j)%seed(ipl)%mass = seed_mass_frac * pcom(j)%ab_gr(ipl)%mass
          pcom(j)%stem(ipl)%mass = stem_mass_frac * pcom(j)%ab_gr(ipl)%mass
          
          !! partition carbon with constant fractions
          pcom(j)%leaf(ipl)%cmass = c_frac%leaf * pcom(j)%leaf(ipl)%mass
          pcom(j)%stem(ipl)%cmass = c_frac%stem * pcom(j)%stem(ipl)%mass
          pcom(j)%seed(ipl)%cmass = c_frac%seed * pcom(j)%seed(ipl)%mass
          pcom(j)%root(ipl)%cmass = c_frac%root * pcom(j)%root(ipl)%mass
          pcom(j)%ab_gr(ipl)%cmass = pcom(j)%leaf(ipl)%cmass + pcom(j)%stem(ipl)%cmass + pcom(j)%seed(ipl)%cmass
          pcom(j)%plm(ipl)%cmass = pcom(j)%ab_gr(ipl)%cmass + pcom(j)%root(ipl)%cmass
          
          !! partition n and p
          if (pldb(idp)%typ == "perennial") then
            !! woody biomass - partition leaves and seed
            pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%n_fr * pcom(j)%plm(ipl)%mass
            pcom(j)%seed(ipl)%nmass = pldb(idp)%cnyld * pcom(j)%seed(ipl)%mass
            n_left = pcom(j)%plm(ipl)%nmass - pcom(j)%seed(ipl)%nmass
            if (pcom(j)%leaf(ipl)%mass + pcom(j)%seed(ipl)%mass > 1.e-6) then
              n_frac = n_left / (10. * pcom(j)%leaf(ipl)%mass + pcom(j)%seed(ipl)%mass)
            else
              n_frac = 0.
            end if
            pcom(j)%leaf(ipl)%nmass = 10. * n_frac * pcom(j)%leaf(ipl)%mass
            pcom(j)%root(ipl)%nmass = n_frac * pcom(j)%root(ipl)%mass
            pcom(j)%stem(ipl)%nmass = n_frac * pcom(j)%stem(ipl)%mass
            pcom(j)%ab_gr(ipl)%nmass = pcom(j)%seed(ipl)%nmass + pcom(j)%leaf(ipl)%nmass + pcom(j)%stem(ipl)%nmass
            
            pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%p_fr * pcom(j)%plm(ipl)%mass
            pcom(j)%seed(ipl)%pmass = pldb(idp)%cnyld * pcom(j)%seed(ipl)%mass
            p_left = pcom(j)%plm(ipl)%pmass - pcom(j)%seed(ipl)%pmass
            if (pcom(j)%leaf(ipl)%mass + pcom(j)%seed(ipl)%mass > 1.e-6) then
              p_frac = p_left / (10. * pcom(j)%leaf(ipl)%mass + pcom(j)%seed(ipl)%mass)
            else
              p_frac = 0.
            end if
            pcom(j)%leaf(ipl)%pmass = 10. * n_frac * pcom(j)%leaf(ipl)%mass
            pcom(j)%root(ipl)%pmass = n_frac * pcom(j)%root(ipl)%mass
            pcom(j)%stem(ipl)%pmass = n_frac * pcom(j)%stem(ipl)%mass
            pcom(j)%ab_gr(ipl)%pmass = pcom(j)%seed(ipl)%pmass + pcom(j)%leaf(ipl)%pmass + pcom(j)%stem(ipl)%pmass
          else
            !! annual or grass (no woody vegetation) - partition seed (grain)
            pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%n_fr * pcom(j)%plm(ipl)%mass
            pcom(j)%seed(ipl)%nmass = pldb(idp)%cnyld * pcom(j)%seed(ipl)%mass
            !! assume same concentration in rest of plant
            n_left = pcom(j)%plm(ipl)%nmass - pcom(j)%seed(ipl)%nmass
            if (pcom(j)%plm(ipl)%mass - pcom(j)%seed(ipl)%mass > 1.e-6) then
              n_frac = n_left / (pcom(j)%plm(ipl)%mass - pcom(j)%seed(ipl)%mass)
            else
              n_frac = 0.
            end if
            pcom(j)%leaf(ipl)%nmass = n_frac * pcom(j)%leaf(ipl)%mass
            pcom(j)%stem(ipl)%nmass = n_frac * pcom(j)%stem(ipl)%mass
            pcom(j)%root(ipl)%nmass = n_frac * pcom(j)%root(ipl)%mass
            pcom(j)%ab_gr(ipl)%nmass = pcom(j)%seed(ipl)%nmass + pcom(j)%leaf(ipl)%nmass + pcom(j)%stem(ipl)%nmass
            
            p_left = pcom(j)%plm(ipl)%pmass - pcom(j)%seed(ipl)%pmass
            if (pcom(j)%plm(ipl)%mass - pcom(j)%seed(ipl)%mass > 1.e-6) then
              p_frac = p_left / (pcom(j)%plm(ipl)%mass - pcom(j)%seed(ipl)%mass)
            else
              p_frac = 0.
            end if
            pcom(j)%leaf(ipl)%pmass = p_frac * pcom(j)%leaf(ipl)%mass
            pcom(j)%stem(ipl)%pmass = p_frac * pcom(j)%stem(ipl)%mass
            pcom(j)%root(ipl)%pmass = p_frac * pcom(j)%root(ipl)%mass
            pcom(j)%ab_gr(ipl)%pmass = pcom(j)%seed(ipl)%pmass + pcom(j)%leaf(ipl)%pmass + pcom(j)%stem(ipl)%pmass
          end if
           
      return
      end subroutine pl_partition