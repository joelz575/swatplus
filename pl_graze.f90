      subroutine pl_graze
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates biomass lost to grazing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_min(:)   |kg/ha         |minimum plant biomass for grazing
!!    bio_eat(:)   |(kg/ha)/day   |dry weight of biomass removed by grazing
!!                                |daily
!!    bio_trmp(:)  |(kg/ha)/day   |dry weight of biomass removed by
!!                                |trampling daily
!!    fminn(:)     |kg minN/kg frt|fraction of mineral N (NO3 + NH3) in 
!!                                |fertilizer/manure
!!    fminp(:)     |kg minP/kg frt|fraction of mineral P in fertilizer/manure
!!    fnh3n(:)     |kg NH3-N/kg minN|fraction of NH3-N in mineral N in 
!!                                |fertilizer/manure
!!    forgn(:)     |kg orgN/kg frt|fraction of organic N in fertilizer/manure
!!    forgp(:)     |kg orgP/kg frt|fraction of organic P in fertilizer/manure
!!    manure_id(:) |none          |manure (fertilizer) identification
!!                                |number from fert.dat
!!    igrz(:)      |none          |grazing flag for HRU:
!!                                |0 HRU currently not grazed
!!                                |1 HRU currently grazed
!!    ihru         |none          |HRU number
!!    grz_days(:)  |none          |number of days grazing will be simulated
!!    ngr(:)       |none          |sequence number of grazing operation
!!                                |within the year
!!    manure_kg(:) |(kg/ha)/day   |dry weight of manure deposited on HRU
!!                                |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    igrz(:)     |none          |grazing flag for HRU:
!!                               |0 HRU currently not grazed
!!                               |1 HRU currently grazed
!!    ndeat(:)    |days          |number of days HRU has been grazed
!!    ngr(:)      |none          |sequence number of grazing operation
!!                               |within the year
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dmi         |kg/ha         |biomass in HRU prior to grazing
!!    dmii        |kg/ha         |biomass prior to trampling
!!    frt_t       |
!!    gc          |
!!    gc1         |
!!    it          |none          |manure/fertilizer id number from fert.dat
!!    j           |none          |HRU number
!!    l           |none          |number of soil layer that manure is applied
!!    swf         |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module, only : mgt, fertdb
      use basin_module
      use organic_mineral_mass_module
      use parm, only : pcom, soil, emitc_d, rsdc_d, tgrazn, tgrazp, igrz, ndeat, ngr, bio_min, bio_eat,  &
        bio_trmp, manure_id, manure_kg, grz_days, ihru, grazn, grazp  

      integer :: j, l, it
      real :: dmi, dmii, gc, gc1, swf, frt_t, xx
      j = 0
      j = ihru
      
      bioms_tot = 0.
      do ipl = 1, pcom(j)%npl
        bioms_tot = bioms_tot + pcom(j)%plm(ipl)%mass
      end do
!! graze only if adequate biomass in HRU
      if (bioms_tot > bio_min(j)) then

        do ipl = 1, pcom(j)%npl
        !! determine new biomass in HRU
        dmi = 0.
        dmi = pcom(j)%plm(ipl)%mass
        !! for now the amount eaten is evenly divided by the number of plants
        !! later we can add preferences - by animal type or simply by n and p content
        pcom(j)%plm(ipl)%mass =pcom(j)%plm(ipl)%mass - bio_eat(j)/pcom(j)%npl
        if (bioms_tot < bio_min(j)) pcom(j)%plm(ipl)%mass = bio_min(j)

        !!add by zhang
        !!=================
        if (bsn_cc%cswat == 2) then
            emitc_d(j) = emitc_d(j) + dmi - pcom(j)%plm(ipl)%mass
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
        dmii = 0.
        dmii = pcom(j)%plm(ipl)%mass
        pcom(j)%plm(ipl)%mass=pcom(j)%plm(ipl)%mass - bio_trmp(j)/pcom(j)%npl
        if (pcom(j)%plm(ipl)%mass < bio_min(j))  then
          soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd + dmii - bio_min(j)
          pcom(j)%plm(ipl)%mass = bio_min(j)
            !!add by zhang
            !!=================
            if (bsn_cc%cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + dmii - pcom(j)%plm(ipl)%mass
            end if
            !!add by zhang
            !!=================          
        else
          soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd + bio_trmp(j)   
            !!add by zhang
            !!=================
            if (bsn_cc%cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + bio_trmp(j)
            end if
            !!add by zhang
            !!=================                           
        endif
        soil(j)%ly(1)%rsd = Max(soil(j)%ly(1)%rsd,0.)
        pcom(j)%plm(ipl)%mass = Max(pcom(j)%plm(ipl)%mass,0.)

        !! adjust nutrient content of residue and biomass for
        !! trampling
        pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - (dmii -        & 
               pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%n_fr
        pcom(ihru)%plm(ipl)%pmass = pcom(ihru)%plm(ipl)%pmass - (dmii -  &
             pcom(j)%plm(ipl)%mass) * pcom(j)%plm(ipl)%p_fr
        if (pcom(j)%plm(ipl)%nmass < 0.) pcom(j)%plm(ipl)%nmass = 0.
        if (pcom(ihru)%plm(ipl)%pmass < 0.) pcom(ihru)%plm(ipl)%pmass=0.
        if (dmii - pcom(j)%plm(ipl)%mass > 0.) then
          rsd1(j)%tot(1)%n = (dmii - pcom(j)%plm(ipl)%mass) *          &
            pcom(j)%plm(ipl)%n_fr + rsd1(j)%tot(1)%n

            !!insert new biomss by zhang
            !!===========================
            if (bsn_cc%cswat == 2) then
                  !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU               

                BLG1 = 0.01/0.10
                BLG2 = 0.99
                BLG3 = 0.10
                XXX = log(0.5/BLG1-0.5)
                BLG2 = (XXX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XXX + 0.5*BLG2
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/                      &
                  (pcom(j)%plcur(ipl)%phuacc +                           &
                  EXP(BLG1-BLG2*pcom(j)%plcur(ipl)%phuacc))

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (rsd1(j)%mn%no3 + rsd1(j)%mn%nh4)
	          	          
	          resnew = (dmii - pcom(j)%plm(ipl)%mass) 
	          resnew_n = (dmii - pcom(j)%plm(ipl)%mass) *                   &
                    pcom(j)%plm(ipl)%n_fr   	    
        	    resnew_ne = resnew_n + sf * sol_min_n
        	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          rsd1(j)%meta%m = rsd1(j)%meta%m + LMF * resnew
	          rsd1(j)%str%m = rsd1(j)%str%m + LSF * resnew
        	  
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          rsd1(j)%lig%m = rsd1(j)%lig%m + RLR*resnew
	          rsd1(j)%str%c = rsd1(j)%str%c + 0.42*LSF * resnew  
	          
	          rsd1(j)%lig%c = rsd1(j)%lig%c + RLR*0.42* resnew
	          rsd1(j)%lig%n = rsd1(j)%str%c - rsd1(j)%lig%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         rsd1(j)%str%n = rsd1(j)%str%n + 0.42*LSF*resnew/150.
		         rsd1(j)%meta%n = rsd1(j)%meta%n + resnew_ne -         & 
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         rsd1(j)%str%n = rsd1(j)%str%n + resnew_ne
		         rsd1(j)%meta%n = rsd1(j)%meta%n + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          rsd1(j)%meta%c = rsd1(j)%meta%c + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                rsd1(j)%mn%no3 = rsd1(j)%mn%no3 * (1-sf)
                rsd1(j)%mn%nh4 = rsd1(j)%mn%nh4 * (1-sf)
            end if
            !!insert new biomss by zhang
            !!===========================


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
        it = 0
        it = manure_id(j)
        it = mgt%op1
        if (manure_kg(j) > 0.) then 
          l = 1
          
          if (bsn_cc%cswat == 0) then

          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg(j) *      &
                  (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + manure_kg(j) *      &
                                                   fertdb(it)%forgn 
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg(j) *      &
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg(j) *      &
                       fertdb(it)%fminp
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg(j) *      &
                       fertdb(it)%forgp
          end if
          if (bsn_cc%cswat == 1) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg(j) *       &
                  (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          soil1(j)%man(l)%n = soil1(j)%man(l)%n + manure_kg(j) *         &
                       fertdb(it)%forgn
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg(j) *       &
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg(j) *       &
                       fertdb(it)%fminp
          soil1(j)%man(l)%p = soil1(j)%man(l)%p + manure_kg(j) *         &
                       fertdb(it)%forgp         
          soil1(j)%man(l)%c = soil1(j)%man(l)%c + manure_kg(j) *         &
                       fertdb(it)%forgn * 10.
          end if
          
          !!By Zhang for C/N cycling
          !!===============================  
          if (bsn_cc%cswat == 2) then
          soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 + manure_kg(j) *        &   
                       (1. - fertdb(it)%fnh3n) * fertdb(it)%fminn
          !sol_fon(l,j) = sol_fon(l,j) + manure_kg(j) *   
     !! &    !             forgn(it)
          orgc_f = 0.35  
          X1 = manure_kg(j)
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
          YY = manure_kg(j) * X10
          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + YY
          ZZ = manure_kg(j) * fertdb(it)%forgn * X10
          soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + ZZ
          soil1(j)%str(l)%n = soil1(j)%str(l)%n + manure_kg(j) * fertdb(it)%forgn - ZZ
          XZ = manure_kg(j) * orgc_f-XX
          soil1(j)%str(l)%c = soil1(j)%str(l)%c + XZ
          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + XZ * .175
          soil1(j)%lig(l)%n = soil1(j)%lig(l)%n + XZ * (1.-.175) 
          YZ = manure_kg(j) - YY
          soil1(j)%str(l)%m = soil1(j)%str(l)%m + YZ
          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + YZ *.175
          soil1(j)%tot(l)%n = soil1(j)%meta(l)%n + soil1(j)%str(l)%n
          soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 + manure_kg(j) *       &   
                       fertdb(it)%fnh3n * fertdb(it)%fminn
          soil1(j)%mp(l)%lab = soil1(j)%mp(l)%lab + manure_kg(j) *       & 
                       fertdb(it)%fminp
          soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + manure_kg(j) *       &   
                       fertdb(it)%forgp  
          
          end if
          !!By Zhang for C/N cycling
          !!=============================== 

        end if

        !! summary calculations
        !! I do not understand these summary calculations Armen March 2009
        grazn = grazn + manure_kg(j) *                                  &                                  
                     (fertdb(it)%fminn + fertdb(it)%forgn)
        grazp = grazp + manure_kg(j) *                                  &                                  
                     (fertdb(it)%fminp + fertdb(it)%forgp)
        tgrazn(j) = tgrazn(j) + grazn
        tgrazp(j) = tgrazp(j) + grazp
      end if

!! check to set if grazing period is over
      if (ndeat(j) == grz_days(j)) then
        igrz(j) = 0
        ndeat(j) = 0
        ngr(j) = ngr(j) + 1
      end if

      return
      end subroutine pl_graze