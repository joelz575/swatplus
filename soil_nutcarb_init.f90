     subroutine soil_nutcarb_init (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    skoc(:)       |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                                 |for soil organic carbon content
!!    sol_pst(:,:,1)|kg/ha         |initial amount of pesticide in first layer
!!                                 |read in from .chm file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer NOTE UNIT
!!                                 |CHANGE!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    soldepth    |mm            |depth from bottom of 1st soil layer to
!!                               |the bottom of the layer of interest
!!    solpst      |mg/kg         |concentration of pesticide in soil
!!    summinp     |kg P/ha       |amount of phosphorus stored in the mineral P
!!                               |pool in the profile
!!    sumno3      |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the soil profile
!!    sumorgn     |kg N/ha       |amount of nitrogen stored in the organic N
!!                               |pools in the profile
!!    sumorgp     |kg P/ha       |amount of phosphorus stored in the organic P
!!                               |pools in the profile
!!    wt1         |none          |converts mg/kg (ppm) to kg/ha
!!    xx          |none          |variable to hold value
!!    zdst        |none          |variable to hold value
!!    labfrac     |none          |fraction of total soil mineral P which is labile
!!    soil_TP	  |kg/ha         |Total Soil Mineral P
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : sol, rsdin, nactfr
      use basin_module
      use organic_mineral_mass_module

      integer :: nly, j, jj, n
      real :: xx, wt1, zdst, soldepth, sumno3, sumorgn, summinp
      real :: sumorgp, solpst, soil_TP, labfrac,solp

      solpst = 0.
      sumno3 = 0.
      sumorgn = 0.
      summinp = 0.
      sumorgp = 0.
      nly = sol(isol)%s%nly

!      if (rsdin(j) > 0.) soil(j)%ly(1)%rsd = rsdin(j)
!      sol(isol)%ly(j)%n = sol(isol)%cbn(j)%cbn / 11.0
      
!!    calculate percent carbon for lower layers if only have upper layer
      if (nly >= 3) then
        if (sol1(isol)%cbn(3) <= 0) then
          do j = 3, nly
            if (sol1(isol)%cbn(j) == 0.) then
              soldepth = sol(isol)%phys(j)%d - sol(isol)%phys(2)%d
              sol1(isol)%cbn(j) = sol1(isol)%cbn(j-1) * Exp(-.001 * soldepth)
            end if
          end do
        end if
      end if

      do j = 1, nly
        !! soil total organic mass and carbon mass (kg/ha = mm * t/m3 * 10000 m2/ha * m/1000 mm * 1000 kg/t = 10000.
        soil_mass = 10000. * sol(isol)%phys(j)%thick * sol(isol)%phys(j)%bd * (1 - sol(isol)%phys(j)%rock / 100.)
        sol1(isol)%tot(j)%c = soil_mass * (sol1(isol)%cbn(j) / 100.)
        sol1(isol)%tot(j)%m = 1.72 * sol1(isol)%tot(j)%c
      end do

!!    calculate initial nutrient contents of layers, profile and
!!    average in soil for the entire watershed
!!    convert mg/kg (ppm) to kg/ha

      do j = 1, nly
        wt1 = sol(isol)%phys(j)%bd * sol(isol)%phys(j)%thick / 100.          !! mg/kg => kg/ha
        sol(isol)%phys(j)%conv_wt = 1.e6 * wt1          !! kg/kg => kg/ha

        if (sol1(isol)%mn(j)%no3 <= 0.) then
          zdst = Exp(-sol(isol)%phys(j)%d / 1000.)
          sol1(isol)%mn(j)%no3 = 10. * zdst * .7
        end if
        sol1(isol)%mn(j)%no3 =  sol1(isol)%mn(j)%no3 * wt1      !! mg/kg => kg/ha
        sumno3 = sumno3 +  sol1(isol)%mn(j)%no3

        if (sol1(isol)%hp(j)%n > 0.0001) then
          sol1(isol)%hp(j)%n = sol1(isol)%hp(j)%n * wt1   !! mg/kg => kg/ha
        else
          !! assume C:N ratio of 10:1
          sol1(isol)%hp(j)%n = 10000. * (sol1(isol)%tot(j)%c / 11.)*      &
                                  wt1  !! CN ratio was 14 before 01-22-09 Armen
        end if
        sol1(isol)%hs(j)%n = sol1(isol)%hs(j)%n * nactfr
        sol1(isol)%hp(j)%n = sol1(isol)%hp(j)%n * (1. - nactfr)
        sumorgn = sumorgn + sol1(isol)%hs(j)%n +                          &
            sol1(isol)%hp(j)%n + sol1(isol)%tot(j)%n

        if (sol1(isol)%hp(j)%p > 0.0001) then
          sol1(isol)%hp(j)%p = sol1(isol)%hp(j)%p * wt1   !! mg/kg => kg/ha
        else
	!! assume N:P ratio of 8:1 
          sol1(isol)%hp(j)%p = .125 * sol1(isol)%hp(j)%n   
        end if

        if (sol1(isol)%mp(j)%lab > 0.0001) then
          sol1(isol)%mp(j)%lab = sol1(isol)%mp(j)%lab * wt1   !! mg/kg => kg/ha
        else
          !! assume initial concentration of 5 mg/kg
          sol1(isol)%mp(j)%lab = 5. * wt1
        end if

        !! Set active pool based on dynamic PSP MJW
		
	    if (bsn_cc%sol_P_model == 0) then 
	      !! Allow Dynamic PSP Ratio
            !! convert to concentration
            solp = sol1(isol)%mp(j)%lab / sol(isol)%phys(j)%conv_wt * 1000000.
	      !! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
	      if (sol(isol)%phys(j)%clay > 0.) then
              bsn_prm%psp = -0.045 * log(sol(isol)%phys(j)%clay) + (0.001 * solp) 
              bsn_prm%psp = bsn_prm%psp - (0.035 * sol1(isol)%tot(j)%c) + 0.43 
            else
              bsn_prm%psp = 0.4
            endif   		
            !! Limit PSP range
            
            if (bsn_prm%psp < .05) then
              bsn_prm%psp = 0.05
	      else if (bsn_prm%psp > 0.9) then
              bsn_prm%psp = 0.9
            end if
          end if
	    
        sol1(isol)%mp(j)%act = sol1(isol)%mp(j)%lab * (1. - bsn_prm%psp) / bsn_prm%psp

        !! Set Stable pool based on dynamic coefficient
	    if (bsn_cc%sol_P_model == 0) then  !! From White et al 2009 
            !! convert to concentration for ssp calculation
	        actp = sol1(isol)%mp(j)%act / sol(isol)%phys(j)%conv_wt*1000000.
		    solp = sol1(isol)%mp(j)%lab / sol(isol)%phys(j)%conv_wt *1000000.
            !! estimate Total Mineral P in this soil based on data from sharpley 2004
		    ssp = 25.044 * (actp + solp)** -0.3833
		    !!limit SSP Range
		    if (SSP > 7.) SSP = 7.
		    if (SSP < 1.) SSP = 1.	      	  
		    sol1(isol)%mp(j)%sta = SSP * (sol1(isol)%mp(j)%act + sol1(isol)%mp(j)%lab)
         else
	      !! The original code
		  sol1(isol)%mp(j)%sta = 4. * sol1(isol)%mp(j)%act
	   end if

        xx = sol(isol)%phys(j)%d
        summinp = summinp + sol1(isol)%mp(j)%lab + sol1(isol)%mp(j)%act          &
                 + sol1(isol)%mp(j)%sta
        sumorgn = sumorgn + sol1(isol)%hs(j)%n +                              &
         sol1(isol)%hp(j)%n + sol1(isol)%tot(j)%n + sol1(isol)%microb(j)%n
        sumorgp = sumorgp + sol1(isol)%hp(j)%p + sol1(isol)%tot(j)%p
      end do

      !! By Zhang for C/N cycling
      !!=============================== 
      if (bsn_cc%cswat == 2) then
      if (rsdin(i) > 0.) sol(isol)%ly(1)%rsd = rsdin(i)		
	do j = 1, nly
		!!kg/ha sol mass in each layer
		if (j == 1) then
		    sol_mass = (sol(isol)%phys(j)%d) / 1000.
            sol_mass = sol_mass * 10000. * sol(isol)%phys(j)%bd* 1000.
            sol_mass = sol_mass * (1- sol(isol)%phys(j)%rock / 100.) 	
            
		else
            sol_mass = (sol(isol)%phys(j)%d-sol(isol)%phys(j-1)%d)/1000.
            sol_mass = sol_mass * 10000. * sol(isol)%phys(j)%bd* 1000.
            sol_mass = sol_mass * (1- sol(isol)%phys(j)%rock / 100.) 			
		end if
		!!kg/ha mineral nitrogen
		sol_min_n =  sol1(isol)%mn(j)%no3 + sol1(isol)%mn(j)%nh4	     
 
        !XCB = 0.2
        !mm

        !if(sol_WOC(j,ihru)<1.E-5) sol_WOC(j,ihru)=XCB*exp(-.001*sol(isol)%phys(j)%thick)  
                                             
        !XCB=sol_WOC(j,ihru)                                                                     
        !XZ=sol_WOC(j,ihru) *.0172                                                                
        !ZZ=1.-XZ                                                                       
        !sol_BDM(j,ihru)=ZZ/(1./sol_BD(j,ihru)-XZ/.224)                                                   
	  !if(sol_BDM(j,ihru)<1.)then                                                                  
	  !    sol_BDM(j,ihru)=1.                                                                         
	  !    sol_BD(j,ihru)=1./(ZZ+XZ/.224)                                                             
	  !end if                                                                             
        
        !ton/ha
        !WT = sol_mass/1000.
        
        !WT1 = WT/1000.
        !X1 = 10. * sol_cbn(j,ihru) * WT
        !WT(J)=BD(J)*sol(isol)%phys(j)%thick*10.                                                             
        !WT1=WT(J)/1000.                                                                
        !X1=10.*WOC(J)*WT(J) 
        !WOC(J)=X1  
        !kg/ha                                                           
        !sol_WOC(j,ihru)=X1
        sol1(isol)%water(j)%c = sol_mass * sol1(isol)%tot(j)%c / 100.  
        !if(sol_WON(j,ihru)>0.)then                                                             
        !      sol_WON(j,ihru)=WT1*sol_WON(j,ihru)  
        !      KK=0 
        !else
        sol1(isol)%water(j)%c = sol1(isol)%hs(j)%n + sol1(isol)%hp(j)%n  !0.1 * sol_WOC(j,i)
        !      KK=1 
        !end if     

        !Frction of Mirobial Biomass, Humus Passive C pools
        FBM = 0.0
        FHP = 0.0
        IF(FBM<1.E-10)FBM=.04   
        RTN0 = 100.                   
        IF(FHP<1.E-10)FHP=.7-.4*EXP(-.0277*100) 
        FHS = 1 - FBM - FHP
        !From DSSAT
        FBM = 0.02
        FHS = 0.54
        FHP = 0.44
				
		!NCC = 0
        !IF(NCC==0)THEN
            !sol_WBM(j,ihru)=FBM*X1
            sol1(isol)%microb(j)%m = FBM * sol1(isol)%water(j)%c
            sol1(isol)%microb(j)%c = sol1(isol)%microb(j)%m                                 !needs a ratio??                                                    
            RTO = sol1(isol)%water(j)%n / sol1(isol)%water(j)%c                                                                              
            sol1(isol)%microb(j)%n = RTO * sol1(isol)%microb(j)%c     
            sol1(isol)%hp(j)%m = FHP * (sol1(isol)%water(j)%c - sol1(isol)%microb(j)%m)    
            sol1(isol)%hs(j)%m = sol1(isol)%water(j)%c - sol1(isol)%microb(j)%m - sol1(isol)%hp(j)%m                                                                
            sol1(isol)%hs(j)%c = sol1(isol)%hs(j)%m                                         !needs a ratio??  
            sol1(isol)%hs(j)%n = RTO * sol1(isol)%hs(j)%c
            sol1(isol)%hp(j)%c = sol1(isol)%hp(j)%m                                          !needs a ratio?? 
            sol1(isol)%hp(j)%n = RTO * sol1(isol)%hp(j)%c
                                                                    
            X1=sol(isol)%ly(j)%rsd /1000.                                              
            sol1(isol)%meta(j)%m = 500. * X1                             
            sol1(isol)%str(j)%m = sol1(isol)%meta(j)%m
            sol1(isol)%lig(j)%m = .8 * sol1(isol)%str(j)%m
            sol1(isol)%meta(j)%c = .42 * sol1(isol)%meta(j)%m
            sol1(isol)%meta(j)%n = .1 * sol1(isol)%meta(j)%c
            sol1(isol)%str(j)%c = .42* sol1(isol)%str(j)%m
            sol1(isol)%lig(j)%c = .8 * sol1(isol)%lig(j)%c
            sol1(isol)%lig(j)%n = .2 * sol1(isol)%lig(j)%c
            sol1(isol)%str(j)%n = sol1(isol)%lig(j)%c / 150.
            sol1(isol)%water(j)%c = sol1(isol)%water(j)%c + sol1(isol)%str(j)%c + sol1(isol)%meta(j)%c
            sol1(isol)%water(j)%n = sol1(isol)%water(j)%n + sol1(isol)%str(j)%n + sol1(isol)%meta(j)%n
            !END IF 		
            
            !if (sol_orgn(j,i) > 0.0001) then
            !  sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
            !else
              !! assume C:N ratio of 10:1
            !  sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 11.) * wt1  !! CN ratio was 14 before 01-22-09 Armen
            !end if
            sol1(isol)%hp(j)%n = sol1(isol)%hp(j)%n
            sol1(isol)%hs(j)%n = sol1(isol)%hs(j)%n
            sol1(isol)%tot(j)%n = sol1(isol)%meta(j)%n + sol1(isol)%str(j)%n
            !sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
            !sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)

	end do	
	
	end if
      !! By Zhang for C/N cycling
      !!=============================== 

      return
      end subroutine soil_nutcarb_init