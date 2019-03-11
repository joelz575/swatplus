      subroutine pl_graze      
    
      use mgt_operations_module
      use fertilizer_data_module
      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : igrz, ndeat, grz_days, ihru, grazn, grazp 
      use soil_module
      use plant_module
      use carbon_module
      
      implicit none 

      integer :: j         !none        |HRU number
      integer :: l         !none        |number of soil layer that manure applied
      integer :: it        !none        |manure/fertilizer id from fertilizer.frt
      real :: gc           !            |
      real :: gc1          !            |
      real :: swf          !            |
      real :: frt_t        !            |
      real :: xx           !none        |variable to hold intermediate calculation
                           !            |result
      real :: dmi          !kg/ha       |biomass in HRU prior to grazing
      real :: dmii         !kg/ha       |biomass prior to trampling
      real :: zz           !none        |variable to hold intermediate calculation
                           !            |result
      real :: yz
      real :: yy           !none        |variable to hold intermediate calculation
                           !            |result
      real :: xz           !            |the amount of organic carbon allocated to structural litter C pool  
      real :: xxx          !            |the amount of organic carbon allocated to metabolic litter C pool
      real :: x8           !            |organic carbon applied (kg C/ha)   
      real :: x10          !frac        |the fraction of carbon in fertilizer that is allocated to metabolic litter C pool
      real :: x1           !            |fertlizer applied to layer (kg/ha)
      real :: sol_min_n    !            |
      real :: sf           !frac        |fraction of mineral n sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter 
      real :: rlr          !frac        |fraction of lignin in the added residue
      real :: rln          !            |
      real :: resnew_ne    !            |
      real :: resnew_n     !            |
      real :: resnew       !            |  
      real :: orgc_f       !frac        |fraction of organic carbon in fertilizer
      real :: lsf          !frac        |fraction of the litter that is structural
      real :: lmf          !frac        |fraction of the litter that is metabolic 
      integer :: ipl       !none        |counter
      real :: clg          !            |
      real :: manure_kg

      j = ihru

        do ipl = 1, pcom(j)%npl
        !! determine new biomass in HRU
        dmi = pcom(j)%plm(ipl)%mass
        !! for now the amount eaten is evenly divided by the number of plants
        !! later we can add preferences - by animal type or simply by n and p content
        pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - graze%eat / pcom(j)%npl
 
        !!add by zhang
        !!=================
        if (bsn_cc%cswat == 2) then
            cbn_loss(j)%emitc_d = cbn_loss(j)%emitc_d + dmi - pcom(j)%plm(ipl)%mass
        end if
        !!add by zhang
        !!=================        
        
        !! adjust nutrient content of biomass
        pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - (dmi -         &     
              pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%n_fr
        pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - (dmi -   &
              pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%p_fr
        if (pcom(j)%plm(ipl)%nmass < 0.) pcom(j)%plm(ipl)%nmass = 0.
        if (pcom(ihru)%plm(ipl)%pmass < 0.) pcom(ihru)%plm(ipl)%pmass = 0.

        !! remove trampled biomass and add to residue
        dmii = pcom(j)%plm(ipl)%mass
        pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass - graze%tramp / pcom(j)%npl
        if (pcom(j)%plm(ipl)%mass < 0.)  then
          rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m + dmii
          pcom(j)%plm(ipl)%mass = 0.
          !!add by zhang
          if (bsn_cc%cswat == 2) then
            cbn_loss(j)%rsdc_d = cbn_loss(j)%rsdc_d + dmii - pcom(j)%plm(ipl)%mass
          end if         
        else
          rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m + graze%tramp   
          !!add by zhang
          if (bsn_cc%cswat == 2) then
            cbn_loss(j)%rsdc_d = cbn_loss(j)%rsdc_d + graze%tramp
          end if                           
        endif
        rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
        pcom(j)%plm(ipl)%mass = Max(pcom(j)%plm(ipl)%mass, 0.)

        !! adjust nutrient content of residue and biomass for trampling
        pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - (dmii -        & 
               pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%n_fr
        pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - (dmii -  &
             pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%p_fr
        if (pcom(j)%plm(ipl)%nmass < 0.) pcom(j)%plm(ipl)%nmass = 0.
        if (pcom(ihru)%plm(ipl)%pmass < 0.) pcom(ihru)%plm(ipl)%pmass=0.
        if (dmii - pcom(j)%plm(ipl)%mass > 0.) then
          rsd1(j)%tot(1)%n = (dmii - pcom(j)%plm(ipl)%mass) *          &
            pcom(j)%plm(ipl)%n_fr + rsd1(j)%tot(1)%n

          resnew = (dmii - pcom(j)%plm(ipl)%mass) 
          resnew_n = (dmii - pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%n_fr
          call pl_leaf_drop (resnew, resnew_n)

          rsd1(j)%tot(1)%p = (dmii - pcom(j)%plm(ipl)%mass) *            &
             pcom(j)%plm(ipl)%p_fr + rsd1(j)%tot(1)%p 
        end if

        !! reset leaf area index and fraction of growing season
        if (dmi > 1.) then
          pcom(j)%plg(ipl)%lai = pcom(j)%plg(ipl)%lai *                  &
             pcom(j)%plm(ipl)%mass / dmi
          pcom(j)%plcur(ipl)%phuacc = pcom(j)%plcur(ipl)%phuacc *        &
             pcom(j)%plm(ipl)%mass / dmi
        else
          pcom(j)%plg(ipl)%lai = 0.05
          pcom(j)%plcur(ipl)%phuacc = 0.
        endif

        end do    !! plant loop
        
        
        !! apply manure
        it = graze%manure_id
        manure_kg = graze%eat * graze%manure
        if (manure_kg > 0.) then 
          l = 1
          
          if (bsn_cc%cswat == 0) then

          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg *      &
                  (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + manure_kg *      &
                                                   fertdb(it)%forgn 
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg *      &
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg *      &
                       fertdb(it)%fminp
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg *      &
                       fertdb(it)%forgp
          end if
          if (bsn_cc%cswat == 1) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg *       &
                  (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil1(j)%man(l)%n = soil1(j)%man(l)%n + manure_kg *         &
                       fertdb(it)%forgn
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg *       &
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg *       &
                       fertdb(it)%fminp
          soil1(j)%man(l)%p = soil1(j)%man(l)%p + manure_kg *         &
                       fertdb(it)%forgp         
          soil1(j)%man(l)%c = soil1(j)%man(l)%c + manure_kg *         &
                       fertdb(it)%forgn * 10.
          end if
          
          !!By Zhang for C/N cycling
          !!===============================  
          if (bsn_cc%cswat == 2) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg *        &   
                       (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          !sol_fon(l,j) = sol_fon(l,j) + manure_kg(j) * forgn(it)
          orgc_f = 0.35  
          X1 = manure_kg
          X8 = X1 * orgc_f          
          RLN = .175 *(orgc_f)/(fertdb(it)%fminp + fertdb(it)%forgn + 1.e-5)
          X10 = .85-.018*RLN
          if (X10<0.01) then
            X10 = 0.01
          else
            if (X10 > .7) then
                X10 = .7
            end if
          end if
          XX = X8 * X10
          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + XX
          YY = manure_kg * X10
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + YY
          ZZ = manure_kg * fertdb(it)%forgn * X10
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + ZZ
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + manure_kg * fertdb(it)%forgn - ZZ
          XZ = manure_kg * orgc_f-XX
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + XZ
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + XZ * .175
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + XZ * (1.-.175) 
          YZ = manure_kg - YY
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + YZ
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + YZ *.175
          soil1(j)%tot(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg *       &   
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg *       & 
                       fertdb(it)%fminp
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg *       &   
                       fertdb(it)%forgp  
          
          end if

        end if

      return
      end subroutine pl_graze