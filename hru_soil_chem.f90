      subroutine hru_soil_chem (isol)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    i             |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    skoc(:)       |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                                 |for soil organic carbon content
!!    sol_pst(:,:,1)|kg/ha         |initial amount of pesticide in first layer
!!                                 |read in from .chm file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_cov(:)    |kg/ha         |amount of residue on soil surface
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer NOTE UNIT
!!                                 |CHANGE!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
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

      use basin_module

      integer :: nly, j, jj, n
      real :: xx, dg, wt1, zdst, soldepth, sumno3, sumorgn, summinp
      real :: sumorgp, solpst, soil_TP, labfrac,solp
      
      !!by zhang
      !!=============
          real :: sol_mass
          real :: FBM, FHP, RTNO, FHS, X1, RTO, sol_min_n
          sol_mass = 0.
          DG = 0.
          FBM = 0.
          FHP = 0.
          RTNO = 0.
          FHS = 0.
          X1 = 0.
          RTO = 0.
      !!by zhang
      !!=============
      
      
      nly = 0
      solpst = 0.
      sumno3 = 0.
      sumorgn = 0.
      summinp = 0.
      sumorgp = 0.
      nly = sol(isol)%s%nly

!!    calculate sol_cbn for lower layers if only have upper layer
      if (nly >= 3) then
      if (sol(isol)%cbn(3)%cbn <= 0) then
        do j = 3, nly
          if (sol(isol)%cbn(j)%cbn == 0.) then
            soldepth = 0
            soldepth = sol(isol)%phys(j)%d - sol(isol)%phys(2)%d
            sol(isol)%cbn(j)%cbn = sol(isol)%cbn(j-1)%cbn *                   &
                                                Exp(-.001 *soldepth)
          end if
        end do
      end if
      end if

 !!     cmup_kgh = 0.                              !!! jga 
 !!     cmtot_kgh = 0.                             !!! jga 
      do j = 1, nly
        if (j == 1) then
          sol_thick = sol(isol)%phys(j)%d
        else
          sol_thick = sol(isol)%phys(j)%d - sol(isol)%phys(j-1)%d
        end if
 
!! soil carbon and nitrogen
        sol_mass = (sol_thick / 1000.) * 10000. * sol(isol)%phys(j)%bd      &
             * 1000. * (1 - sol(isol)%phys(j)%rock / 100.)
        sol_cmass = sol_mass * (sol(isol)%cbn(j)%cbn / 100.)

        if (j == 1) sol(isol)%s%cmup_kgh = sol_cmass
        sol(isol)%s%cmtot_kgh  = sol(isol)%s%cmtot_kgh + sol_cmass
      end do

!!    calculate initial nutrient contents of layers, profile and
!!    average in soil for the entire watershed
!!    convert mg/kg (ppm) to kg/ha

!      do ipl = 1, pcom(j)%npl  !! needs to be fixed when updating residue by plant type**
!      sol(isol)%rsd_flt(ipl)%pmass = sol(isol)%rsd_flt(ipl)%mass * .0010 !! was 0.0003 Armen January 2009
!      sol(isol)%rsd_flt(ipl)%nmass = sol(isol)%rsd_flt(ipl)%mass * .0055 !! was 0.0015 Armen January 2009
!      end do
      
      xx = 0.
      do j = 1, nly
        dg = (sol(isol)%phys(j)%d - xx)
        wt1 = sol(isol)%phys(j)%bd * dg / 100.          !! mg/kg => kg/ha
        sol(isol)%phys(j)%conv_wt = 1.e6 * wt1          !! kg/kg => kg/ha

        if (sol(isol)%nut(j)%no3 <= 0.) then
          zdst = Exp(-sol(isol)%phys(j)%d / 1000.)
          sol(isol)%nut(j)%no3 = 10. * zdst * .7
        end if
        sol(isol)%nut(j)%no3 =  sol(isol)%nut(j)%no3 * wt1      !! mg/kg => kg/ha
        sumno3 = sumno3 +  sol(isol)%nut(j)%no3

        if (sol(isol)%nut(j)%orgn > 0.0001) then
          sol(isol)%nut(j)%orgn = sol(isol)%nut(j)%orgn * wt1   !! mg/kg => kg/ha
        else
          !! assume C:N ratio of 10:1
          sol(isol)%nut(j)%orgn = 10000. * (sol(isol)%cbn(j)%cbn / 11.)*      &
                                  wt1  !! CN ratio was 14 before 01-22-09 Armen
        end if
        sol(isol)%nut(j)%aorgn = sol(isol)%nut(j)%orgn * nactfr
        sol(isol)%nut(j)%orgn = sol(isol)%nut(j)%orgn * (1. - nactfr)
        sumorgn = sumorgn + sol(isol)%nut(j)%aorgn +                          &
            sol(isol)%nut(j)%orgn + sol(isol)%nut(j)%fon

        if (sol(isol)%nut(j)%orgp > 0.0001) then
          sol(isol)%nut(j)%orgp = sol(isol)%nut(j)%orgp * wt1   !! mg/kg => kg/ha
        else
	!! assume N:P ratio of 8:1 
          sol(isol)%nut(j)%orgp = .125 * sol(isol)%nut(j)%orgn   
        end if

        if (sol(isol)%nut(j)%solp > 0.0001) then
          sol(isol)%nut(j)%solp = sol(isol)%nut(j)%solp * wt1   !! mg/kg => kg/ha
        else
          !! assume initial concentration of 5 mg/kg
          sol(isol)%nut(j)%solp = 5. * wt1
        end if

        !! Set active pool based on dynamic PSP MJW
		
	    if (bsn_cc%sol_P_model == 0) then 
	      !! Allow Dynamic PSP Ratio
            !! convert to concentration
            solp = sol(isol)%nut(j)%solp / sol(isol)%phys(j)%conv_wt *          &
                                                             1000000.
	      !! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
	      if (sol(isol)%phys(j)%clay > 0.) then
              bsn_prm%psp = -0.045 * log(sol(isol)%phys(j)%clay) +              &
                    (0.001 * solp) 
              bsn_prm%psp = bsn_prm%psp - (0.035 *                              &
                  sol(isol)%cbn(j)%cbn) + 0.43 
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
	    
        sol(isol)%nut(j)%actp = sol(isol)%nut(j)%solp *                  &
                     (1. - bsn_prm%psp) / bsn_prm%psp

          !! Set Stable pool based on dynamic coefficant
	    if (bsn_cc%sol_P_model == 0) then  !! From White et al 2009 
            !! convert to concentration for ssp calculation
	        actp = sol(isol)%nut(j)%actp/sol(isol)%phys(j)%conv_wt*1000000.
		    solp = sol(isol)%nut(j)%solp / sol(isol)%phys(j)%conv_wt *1000000.
            !! estimate Total Mineral P in this soil based on data from sharpley 2004
		    ssp = 25.044 * (actp + solp)** -0.3833
		    !!limit SSP Range
		    if (SSP > 7.) SSP = 7.
		    if (SSP < 1.) SSP = 1.	      	  
		    sol(isol)%nut(j)%stap = SSP * (sol(isol)%nut(j)%actp +               &
                                                 sol(isol)%nut(j)%solp)
         else
	!! The original code
		  sol(isol)%nut(j)%stap = 4. * sol(isol)%nut(j)%actp
	   end if

        sol(isol)%ly(j)%hum = sol(isol)%cbn(j)%cbn * wt1 * 17200.
        xx = sol(isol)%phys(j)%d
        summinp = summinp + sol(isol)%nut(j)%solp+sol(isol)%nut(j)%actp           &
                 + sol(isol)%nut(j)%stap
        sumorgn = sumorgn + sol(isol)%nut(j)%aorgn +                              &
         sol(isol)%nut(j)%orgn+sol(isol)%nut(j)%fon+sol(isol)%cbn(j)%bmn
        sumorgp = sumorgp + sol(isol)%nut(j)%orgp + sol(isol)%nut(j)%fop
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
		sol_min_n =  sol(isol)%nut(j)%no3 + sol(isol)%nut(j)%nh3	     
 
        !XCB = 0.2
        !mm
        if (j == 1) then
            !DG = 10
            DG = sol(isol)%phys(j)%d
        else
            DG = (sol(isol)%phys(j)%d - sol(isol)%phys(j-1)%d)
        end if        
                
        !if(sol_WOC(j,ihru)<1.E-5) sol_WOC(j,ihru)=XCB*exp(-.001*DG)  
                                             
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
        !WT(J)=BD(J)*DG*10.                                                             
        !DG1=DG                                                                         
        !WT1=WT(J)/1000.                                                                
        !X1=10.*WOC(J)*WT(J) 
        !WOC(J)=X1  
        !kg/ha                                                           
        !sol_WOC(j,ihru)=X1
        sol(isol)%cbn(j)%woc = sol_mass * sol(isol)%cbn(j)%cbn / 100.  
        !if(sol_WON(j,ihru)>0.)then                                                             
        !      sol_WON(j,ihru)=WT1*sol_WON(j,ihru)  
        !      KK=0 
        !else
        sol(isol)%cbn(j)%woc = sol(isol)%nut(j)%aorgn +                          &
                               sol(isol)%nut(j)%orgn  !0.1 * sol_WOC(j,i)
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
        !FBM = 0.02
        !FHS = 0.54
        !FHP = 0.44
				
		!NCC = 0
        !IF(NCC==0)THEN
            !sol_WBM(j,ihru)=FBM*X1
            sol(isol)%ly(j)%bm=FBM*sol(isol)%cbn(j)%woc
            sol(isol)%cbn(j)%bmc=sol(isol)%ly(j)%bm
            !IF(KK==0)THEN                                                                  
	            RTO=sol(isol)%cbn(j)%won/sol(isol)%cbn(j)%woc   
	      !ELSE                                                                                
	      !      RTO=.1                                                                            
	      !END IF                                                                              
            sol(isol)%cbn(j)%bmn=RTO*sol(isol)%cbn(j)%bmc     
            !sol_HP(j,ihru)=FHP*(X1-sol_BM(j,ihru))  
            sol(isol)%ly(j)%hp=FHP*(sol(isol)%cbn(j)%woc-                    &     
                                                sol(isol)%ly(j)%bm)    
            sol(isol)%ly(j)%hs=sol(isol)%cbn(j)%woc-sol(isol)%ly(j)%bm-      &
                   sol(isol)%ly(j)%hp
            !sol_HP(j,i)=sol_WOC(j,i)-sol_BM(j,i)-sol_HP(j,i)                                                                
            sol(isol)%cbn(j)%hsc = sol(isol)%ly(j)%hs     
            sol(isol)%cbn(j)%hsn= RTO*sol(isol)%cbn(j)%hsc  !sol_aorgn(j,i)
            sol(isol)%cbn(j)%hpc=sol(isol)%ly(j)%hp          
            sol(isol)%cbn(j)%hpn= RTO*sol(isol)%cbn(j)%hpc  !sol_orgn(j,i)
            
                                                                    
            X1=sol(isol)%ly(j)%rsd /1000.  
            !!skip std in SWAT                                                                   
            !IF(j==1)X1=X1+STD(j)/1000.
                                                                  
            sol(isol)%cbn(j)%lm = 500.*X1                             
            sol(isol)%cbn(j)%ls = sol(isol)%cbn(j)%lm
            sol(isol)%cbn(j)%lsl = .8 * sol(isol)%cbn(j)%ls
            sol(isol)%cbn(j)%lmc = .42*sol(isol)%cbn(j)%lm
                                                                        
            sol(isol)%cbn(j)%lmn=.1*sol(isol)%cbn(j)%lmc
            sol(isol)%cbn(j)%lsc=.42* sol(isol)%cbn(j)%ls
            sol(isol)%cbn(j)%lslc=.8*sol(isol)%cbn(j)%lsc
            sol(isol)%cbn(j)%lslnc=.2*sol(isol)%cbn(j)%lsc
            sol(isol)%cbn(j)%lsn=sol(isol)%cbn(j)%lsc/150.
            !sol_WOC(j,ihru)=sol_WOC(j,ihru)+sol_LSC(j,ihru)+sol_WLMC(j,ihru)
            sol(isol)%cbn(j)%woc=sol(isol)%cbn(j)%woc +                               &
                sol(isol)%cbn(j)%lsc + sol(isol)%cbn(j)%lmc
            !sol_WON(j,ihru)=sol_WON(j,ihru)+sol_LSN(j,ihru)+sol_WLMN(j,ihru)
            sol(isol)%cbn(j)%won=sol(isol)%cbn(j)%won +                               &
                sol(isol)%cbn(j)%lsn + sol(isol)%cbn(j)%lmn
            !END IF 		
            
            !if (sol_orgn(j,i) > 0.0001) then
            !  sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
            !else
              !! assume C:N ratio of 10:1
            !  sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 11.) * wt1  !! CN ratio was 14 before 01-22-09 Armen
            !end if
            sol(isol)%nut(j)%orgn = sol(isol)%cbn(j)%hpn
            sol(isol)%nut(j)%aorgn = sol(isol)%cbn(j)%hsn
            sol(isol)%nut(1)%fon = sol(isol)%cbn(j)%lmn +                         &
                                             sol(isol)%cbn(j)%lsn
            !sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
            !sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)

	end do	
	
	end if
      !! By Zhang for C/N cycling
      !!=============================== 

      return
      end subroutine hru_soil_chem