      subroutine mgt_killop

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
  
      integer :: j, k
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
 
      j = ihru

	  !! 22 January 2008	
      resnew = pcom(j)%plm(ipl)%mass * (1. - pcom(j)%plg(ipl)%rwt)
	  rtresnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%rwt
	  call pl_rootfr

	  !! update residue, N, P on soil surface
      ff1 = (1 - hiad1) / (1 - hiad1 + pcom(j)%plg(ipl)%rwt)
      hru(j)%rsd_flt(ipl)%mass = resnew + hru(j)%rsd_flt(ipl)%mass
      
      hru(j)%rsd_flt(ipl)%nmass = hru(j)%rsd_flt(ipl)%nmass + ff1 *     &
                                  (pcom(j)%plm(ipl)%nmass - yieldn)
      hru(j)%rsd_flt(ipl)%pmass = hru(j)%rsd_flt(ipl)%pmass + ff1 *     & 
                                  (pcom(j)%plm(ipl)%pmass - yieldp)
      hru(j)%rsd_flt(ipl)%mass = Max(hru(j)%rsd_flt(ipl)%mass, 0.)
	  hru(j)%rsd_flt(ipl)%nmass = Max(hru(j)%rsd_flt(ipl)%nmass, 0.)
	  hru(j)%rsd_flt(ipl)%pmass = Max(hru(j)%rsd_flt(ipl)%pmass, 0.)

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
	          sol_min_n = (soil(j)%nut(1)%no3  +soil(j)%nut(1)%nh3)
	          
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
        	  
	          soil(j)%cbn(1)%lm = soil(j)%cbn(1)%lm + LMF * resnew
	          soil(j)%cbn(1)%ls = soil(j)%cbn(1)%ls + LSF * resnew
        	  

                
	          !here a simplified assumption of 0.5 LSL
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          soil(j)%cbn(1)%lsl = soil(j)%cbn(1)%lsl + RLR* LSF * resnew
	          soil(j)%cbn(1)%lsc = soil(j)%cbn(1)%lsc + 0.42*LSF * resnew  
	          
	          soil(j)%cbn(1)%lslc = soil(j)%cbn(1)%lslc+RLR*0.42*LSF*resnew
	          soil(j)%cbn(1)%lslnc=soil(j)%cbn(1)%lsc-soil(j)%cbn(1)%lslc
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_n >= (0.42 * LSF * resnew /150)) then
		         soil(j)%cbn(1)%lsn = soil(j)%cbn(1)%lsn+0.42*LSF*resnew/150.
		         soil(j)%cbn(1)%lmn = soil(j)%cbn(1)%lmn + resnew_n -          & 
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil(j)%cbn(1)%lsn = soil(j)%cbn(1)%lsn + resnew_n
		         soil(j)%cbn(1)%lmn = soil(j)%cbn(1)%lmn + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          soil(j)%cbn(1)%lmc = soil(j)%cbn(1)%lmc + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                soil(j)%nut(1)%no3 = soil(j)%nut(1)%no3 * (1-sf)
                soil(j)%nut(1)%nh3 = soil(j)%nut(1)%nh3 * (1-sf)
            end if
            !!insert new biomss by zhang
            !!===============================

	!! allocate dead roots, N, P to soil layers
	do l = 1, hru(j)%sol%nly
	 soil(j)%ly(l)%rsd = soil(j)%ly(l)%rsd + soil(j)%ly(l)%rtfr * rtresnew
	 soil(j)%nut(l)%fon = soil(j)%nut(l)%fon+soil(j)%ly(l)%rtfr *            &
          pcom(j)%plm(ipl)%nmass * pcom(j)%plg(ipl)%rwt
	 soil(j)%nut(l)%fop = soil(j)%nut(l)%fop + soil(j)%ly(l)%rtfr *          &
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
	          sol_min_n = (soil(j)%nut(l)%no3 + soil(j)%nut(l)%nh3)
	          	          
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
        	  
	          soil(j)%cbn(l)%lm = soil(j)%cbn(l)%lm + LMF * resnew
	          soil(j)%cbn(l)%ls = soil(j)%cbn(l)%ls + LSF * resnew
        	  
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          soil(j)%cbn(l)%lsl = soil(j)%cbn(l)%lsl + RLR*resnew
	          soil(j)%cbn(l)%lsc = soil(j)%cbn(l)%lsc + 0.42*LSF * resnew  
	          
	          soil(j)%cbn(l)%lslc = soil(j)%cbn(l)%lslc + RLR*0.42*resnew
	          soil(j)%cbn(l)%lslnc = soil(j)%cbn(l)%lsc-soil(j)%cbn(l)%lslc
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn+0.42*LSF*resnew/150.
		         soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + resnew_ne -        & 
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil(j)%cbn(l)%lsn = soil(j)%cbn(l)%lsn + resnew_ne
		         soil(j)%cbn(l)%lmn = soil(j)%cbn(l)%lmn + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(l,j)/(sol_LS(l,j)+1.E-5)	
        	  
	          soil(j)%cbn(l)%lmc = soil(j)%cbn(l)%lmc + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(l,j)/(sol_LM(l,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                soil(j)%nut(1)%no3 = soil(j)%nut(1)%no3 * (1-sf)
                soil(j)%nut(l)%nh3 = soil(j)%nut(l)%nh3 * (1-sf)
            end if
            !!insert new biomss by zhang    
            !!=============================== 


	end do

      if (hrupest(j) == 1) then
        npmx = obcs(icmd)%num_pests
        do k = 1, npmx
           hru(j)%ly(1)%pst(k) = hru(j)%ly(1)%pst(k) + hru(j)%pst(k)%plt
           hru(j)%pst(k)%plt = 0.
        end do
      end if
      
	!! reset variables
      pcom(j)%plcur(ipl)%gro = 0
      pcom(j)%plcur(ipl)%idorm = 0
      pcom(j)%plm(ipl)%mass = 0.
      pcom(j)%plg(ipl)%rwt = 0.
	pcom(j)%plm(ipl)%nmass = 0.
      pcom(ihru)%plm(ipl)%pmass = 0.
      pcom(j)%plstr(ipl)%strsw = 1.
      pcom(j)%plg(ipl)%lai = 0.
      pcom(j)%plg(ipl)%hvstiadj = 0.
      pcom(j)%plcur(ipl)%phuacc = 0.
!      phubase(j) = 0.
	rtfr = 0. ! Resetting roots fraction per layer array
	 
      return
      end subroutine mgt_killop