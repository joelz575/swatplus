      subroutine mgt_killop (jj, iplant)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    ihru         |none          |HRU number
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:) |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module
      use hru_module, only : pcom, soil, hru, plgz, plmz, plstrz, hrupest, ihru, ipl, npmx
      use constituent_mass_module
  
      integer :: j, k
      integer, intent (in) :: jj, iplant
      real :: resnew
      
      !!by zhang
      !!====================
      real :: BLG1, BLG2, BLG3,  CLG, sf
      real :: sol_min_n, resnew_n, resnew_ne
      real :: LMF, LSF, LSLF, LSNF,LMNF 
      orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sf = 0.
      sol_min_n = 0.
      resnew = 0.
      resnew_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.
      !!by zhang
      !!====================      
 
      j = jj
      ipl = iplant

	  !! 22 January 2008	
      resnew = pcom(j)%plm(ipl)%mass * (1. - pcom(j)%plg(ipl)%rwt)
	  rtresnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%rwt
	  call pl_rootfr

	  !! update residue, N, P on soil surface
      ff1 = (1 - hiad1) / (1 - hiad1 + pcom(j)%plg(ipl)%rwt)
      rsd1(j)%tot(ipl)%m = resnew + rsd1(j)%tot(ipl)%m
      
      rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n + ff1 *     &
                                  (pcom(j)%plm(ipl)%nmass - yieldn)
      rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p + ff1 *     & 
                                  (pcom(j)%plm(ipl)%pmass - yieldp)
      rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
	  rsd1(j)%tot(ipl)%n = Max(rsd1(j)%tot(ipl)%n, 0.)
	  rsd1(j)%tot(ipl)%p = Max(rsd1(j)%tot(ipl)%p, 0.)

            !!insert new biomss by zhang
            !!=================================
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
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/                       &
                          (pcom(j)%plcur(ipl)%phuacc +                    &
                          EXP(BLG1 - BLG2 * pcom(j)%plcur(ipl)%phuacc))
    
	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (rsd1(j)%mn%no3 + rsd1(j)%mn%nh4)
	          
	          resnew = resnew
	          resnew_n = ff1 * (pcom(j)%plm(ipl)%nmass- yieldn)    	    
        	      resnew_ne = resnew_n + sf * sol_min_n
        	    
       	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/(resnew+1.E-5))
        	    
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
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          rsd1(j)%lig%m = rsd1(j)%lig%m + RLR * LSF * resnew
	          rsd1(j)%str%c = rsd1(j)%str%c + 0.42 * LSF * resnew  
	          
	          rsd1(j)%lig%c = rsd1(j)%lig%c + RLR * 0.42 * LSF * resnew
	          rsd1(j)%lig%n = rsd1(j)%str%c - rsd1(j)%lig%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_n >= (0.42 * LSF * resnew /150)) then
		         rsd1(j)%str%n = rsd1(j)%str%n + 0.42*LSF*resnew/150.
		         rsd1(j)%meta%n = rsd1(j)%meta%n + resnew_n -          & 
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         rsd1(j)%str%n = rsd1(j)%str%n + resnew_n
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
            !!===============================

	!! allocate dead roots, N, P to soil layers
	do l = 1, soil(j)%nly
	 soil(j)%ly(l)%rsd = soil(j)%ly(l)%rsd + soil(j)%ly(l)%rtfr * rtresnew
	 soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + soil(j)%ly(l)%rtfr *            &
          pcom(j)%plm(ipl)%nmass * pcom(j)%plg(ipl)%rwt
	 soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + soil(j)%ly(l)%rtfr *          &
          pcom(ihru)%plm(ipl)%pmass * pcom(j)%plg(ipl)%rwt

              !!insert new biomss by zhang
              !!==============================
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
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/                      &
                      (pcom(j)%plcur(ipl)%phuacc +                       &
                      EXP(BLG1 - BLG2 * pcom(j)%plcur(ipl)%phuacc))
         
	          if (l == 1) then
		        sf = 0.05
	          else
		        sf = 0.1
	          end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (soil1(j)%mn(l)%no3 + soil1(j)%mn(l)%nh4)
	          	          
	          resnew = soil(j)%ly(l)%rtfr * rtresnew 
           resnew_n = soil(j)%ly(l)%rtfr*ff2*                             &
                        (pcom(j)%plm(ipl)%nmass-yieldn)
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
        	  
	          soil1(j)%meta(l)%m = soil1(j)%meta(l)%m + LMF * resnew
	          soil1(j)%str(l)%m = soil1(j)%str(l)%m + LSF * resnew
        	  
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + RLR * resnew
	          soil1(j)%str(l)%c = soil1(j)%str(l)%c + 0.42*LSF * resnew  
	          
	          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + RLR*0.42*resnew
	          soil1(j)%lig(l)%n = soil1(j)%str(l)%c - soil1(j)%lig(l)%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         soil1(j)%str(l)%n = soil1(j)%str(l)%n + 0.42*LSF*resnew/150.
		         soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + resnew_ne -        & 
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil1(j)%str(l)%n = soil1(j)%str(l)%n + resnew_ne
		         soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(l,j)/(sol_LS(l,j)+1.E-5)	
        	  
	          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(l,j)/(sol_LM(l,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                rsd1(j)%mn%no3 = rsd1(j)%mn%no3 * (1-sf)
                rsd1(j)%mn%nh4 = rsd1(j)%mn%nh4 * (1-sf)
            end if
            !!insert new biomss by zhang    
            !!=============================== 


	end do

      if (hrupest(j) == 1) then
        npmx = obcs(icmd)%num_pests
        do k = 1, npmx
           soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + hru(j)%pst(k)%plt
           hru(j)%pst(k)%plt = 0.
        end do
      end if
      
	!! reset variables
      pcom(j)%plg(ipl) = plgz
      pcom(j)%plm(ipl) = plmz
      
      !! can't reset entire plcur - harv_num can't be zero'd
      pcom(j)%plcur(ipl)%gro = 0
      pcom(j)%plcur(ipl)%idorm = 0
      pcom(j)%plcur(ipl)%phuacc = 0.
      pcom(j)%plcur(ipl)%curyr_mat = 1

	  rtfr = 0. ! Resetting roots fraction per layer array
	 
      return
      end subroutine mgt_killop