      subroutine mgt_harvkillop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest and kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    cnop        |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                                |planting
!!    hru_dafr(:) |km2/km2        |fraction of watershed in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    ihru        |none           |HRU number
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!                                |organic (residue) pool0
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hiad1       |
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    wur         |
!!    yield       |kg            |yield (dry weight)
!!    yieldn      |
!!    yieldp      |
!!    yldpst      |kg pst/ha     |pesticide removed in yield
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module
  
      integer :: j, k
      
!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!      real :: wur, hiad1, yield, yieldn, yieldp, yldpst
      real :: wur, hiad1, yieldn, yieldp, yldpst
	real :: resnew, rtresnew 

	!!By Zhang
	!!=============
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
	!!By Zhang
	!!==================
	

      j = 0
      j = ihru
      idp = pcom(j)%plcur(ipl)%idplt

      ! pl_tot => hru(j)%pl_tot(ipl)
      ! veg_ag => hru(j)%veg_ag(ipl)
      ! grain => hru(j)%grain(ipl)
      ! root => hru(j)%root(ipl)
      ! rsd_flt => hru(j)%rsd_flt(ipl)
      
	!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = (1. - pcom(j)%plg(ipl)%rwt) * pcom(j)%plm(ipl)%mass *     &
         pcom(j)%plm(ipl)%n_fr * auto_eff(j)

      !select case (harvop%typ)
      
      hiad1 = 0.
      if (hi_targ(j) > 0.) then
        hiad1 = hi_targ(j)
      else
        if (pcom(j)%plg(ipl)%plpet < 10.) then
          wur = 100.
        else
          wur = 0.
          if (pcom(j)%plg(ipl)%plpet > 1.e-6) then
            wur = 100. * pcom(j)%plg(ipl)%plet / pcom(j)%plg(ipl)%plpet
          end if
        endif

        hiad1 = (pcom(j)%plg(ipl)%hvstiadj - pldb(idp)%wsyf) *          &
            (wur / (wur + Exp(6.13 - .0883 * wur))) + pldb(idp)%wsyf

        if (hiad1 > pldb(idp)%hvsti) then 
          hiad1 = pldb(idp)%hvsti
        end if
      end if

!! check if yield is from above or below ground
      yield = 0.
      resnew = 0.
      rtresnew = 0.

      frac_harvk = 1. !hkop%frac_harvk
      hi_ovr = 0.  !hkop%hi_ovr
!! stover fraction during harvkillop
      xx = frac_harvk
      if (xx .lt. 1.e-6) then
        xx = hi_ovr                      
      endif
!! stover fraction during harvkillop
      if (hi_ovr > 1.e-6) then
        yield = pcom(j)%plm(ipl)%mass * hi_ovr
        resnew = pcom(j)%plm(ipl)%mass - yield
      else
      if (pldb(idp)%idc == 7) then
        yield = pcom(j)%plm(ipl)%mass * (1. - pcom(j)%plg(ipl)%bio_leaf)
        resnew = pcom(j)%plm(ipl)%mass - yield
      else
        if (pldb(idp)%hvsti > 1.001) then
          yield = pcom(j)%plm(ipl)%mass * (1. - 1. / (1. + hiad1))
          resnew = pcom(j)%plm(ipl)%mass / (1. + hiad1)
          resnew = resnew * (1. - xx)
        else
          yield = (1. - pcom(j)%plg(ipl)%rwt) * pcom(j)%plm(ipl)%mass *    &
             hiad1
          resnew = (1. - pcom(j)%plg(ipl)%rwt)*(1. - hiad1) *              &
             pcom(j)%plm(ipl)%mass
          !! remove stover during harvkillop
          resnew = resnew * (1. - xx)
          rtresnew = pcom(j)%plg(ipl)%rwt * pcom(j)%plm(ipl)%mass	
        endif
      endif
      end if 
      
      if (yield < 0.) yield = 0.
      if (resnew < 0.) resnew = 0.
	if (rtresnew < 0.) rtresnew = 0.	! Armen 19 May 2008
										! I would avoid this check, it is
										! safer to know if variable is negative

      !!add by zhang
      !!=================

      if (bsn_cc%cswat == 2) then
          grainc_d(j) = grainc_d(j) + yield * 0.42
          stoverc_d(j) = stoverc_d(j) + (pcom(j)%plm(ipl)%mass - yield -   &
                                                rtresnew)* .42 * xx
          rsdc_d(j) = rsdc_d(j) + resnew * 0.42
          rsdc_d(j) = rsdc_d(j) + rtresnew * 0.42
      end if
      !!add by zhang
      !!=================

	!! calculate nutrients removed with yield
      yieldn = 0.
      yieldp = 0.
      yieldn = yield * pldb(idp)%cnyld
      yieldp = yield * pldb(idp)%cpyld
      yieldn = Min(yieldn, 0.80 * pcom(j)%plm(ipl)%nmass)
      yieldp = Min(yieldp, 0.80 * pcom(ihru)%plm(ipl)%pmass)

	!! Armen 19 May 2008 / 21 January 2008	
	!! fraction of roots in each layer
	call pl_rootfr

	!! fraction of N, P in residue (ff1) or roots (ff2)
	ff1 = (1 - hiad1) / (1 - hiad1 + pcom(j)%plg(ipl)%rwt)
	ff2 = 1 - ff1

	!! update residue, N, P on soil surface
      rsd1(j)%tot(ipl)%m = resnew + rsd1(j)%tot(ipl)%m
      rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n + FF1 *      &
                                   (pcom(j)%plm(ipl)%nmass - yieldn)
      rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p + FF1 *      & 
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
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/                      &
                     (pcom(j)%plcur(ipl)%phuacc +                        &
                     EXP(BLG1-BLG2*pcom(j)%plcur(ipl)%phuacc))
    

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (rsd1(j)%mn%no3 + rsd1(j)%mn%nh4)
	          
	          resnew = resnew
	          resnew_n = ff1 * (pcom(j)%plm(ipl)%nmass - yieldn)    	    
        	      resnew_ne = resnew_n + sf * sol_min_n
        	    
       	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    !RLN is the ratio of lignin to nitrogen in the newly added residue
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
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          rsd1(j)%lig%m = rsd1(j)%lig%m + RLR * resnew
	          rsd1(j)%str%c = rsd1(j)%str%c + 0.42*LSF * resnew  
	          
	          rsd1(j)%lig%c = rsd1(j)%lig%c + RLR*0.42*resnew
	          rsd1(j)%lig%n = rsd1(j)%str%c - rsd1(j)%lig%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_n >= (0.42 * LSF * resnew /150)) then
		         rsd1(j)%str%n = rsd1(j)%str%n + 0.42*LSF*resnew/150.
		         rsd1(j)%meta%n = rsd1(j)%meta%n + resnew_n -           &
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
            !!insert new biomass by zhang
            !!===============================


	!! allocate dead roots, N, P to soil layers
	do l = 1, soil(j)%nly
	 soil(j)%ly(l)%rsd=soil(j)%ly(l)%rsd + soil(j)%ly(l)%rtfr * rtresnew
       soil1(j)%tot(l)%n = soil1(j)%tot(l)%n + soil(j)%ly(l)%rtfr*ff2*     &
           (pcom(j)%plm(ipl)%nmass - yieldn)
       soil1(j)%tot(l)%p = soil1(j)%tot(l)%p + soil(j)%ly(l)%rtfr*ff2*     &
           (pcom(ihru)%plm(ipl)%pmass - yieldp)

              !!insert new biomass by zhang
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
                CLG=BLG3*pcom(j)%plcur(ipl)%phuacc/                     &
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
	          	          
                resnew = soil(j)%ly(l)%rtfr *rtresnew 
                resnew_n = soil(j)%ly(l)%rtfr*ff2*                       &
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
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          soil1(j)%lig(l)%m = soil1(j)%lig(l)%m + RLR* LSF * resnew
	          soil1(j)%str(l)%c = soil1(j)%str(l)%c + 0.42*LSF * resnew  
	          
	          soil1(j)%lig(l)%c = soil1(j)%lig(l)%c + RLR*0.42 * LSF*resnew
	          soil1(j)%lig(l)%n = soil1(j)%str(l)%c - soil1(j)%lig(l)%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         soil1(j)%str(l)%n = soil1(j)%str(l)%n + 0.42*LSF*resnew/150.
		         soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + resnew_ne -          &
                               (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         soil1(j)%str(l)%n = soil1(j)%str(l)%n + resnew_ne
		         soil1(j)%meta(l)%n = soil1(j)%meta(l)%n + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(l,j)/(sol_LS(l,j)+1.E-5)	
        	  
	          soil1(j)%meta(l)%c = soil1(j)%meta(l)%c + 0.42 * LMF * resnew
	          !LMNF = sol_LMN(l,j)/(sol_LM(l,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                soil1(j)%mn(l)%no3 = soil1(j)%mn(l)%no3 * (1-sf)
                soil1(j)%mn(l)%nh4 = soil1(j)%mn(l)%nh4 * (1-sf)
            end if
            !!insert new biomss by zhang    
            !!=============================== 

	end do
   
	!! adjust foliar pesticide for plant removal
      if (hrupest(j) == 1) then
        npmx = obcs(icmd)%num_pests
        do k = 1, npmx
          !! calculate amount of pesticide removed with yield
          yldpst = 0.
          if (pldb(idp)%hvsti > 1.001) then
            yldpst = hru(j)%pst(k)%plt
            hru(j)%pst(k)%plt = 0.
          else
            yldpst = hiad1 * hru(j)%pst(k)%plt
            hru(j)%pst(k)%plt = hru(j)%pst(k)%plt - yldpst
            if (hru(j)%pst(k)%plt < 0.) hru(j)%pst(k)%plt = 0.
          endif
          !! add pesticide in residue to soil surface
          soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + hru(j)%pst(k)%plt
          
          hru(j)%pst(k)%plt = 0.
        end do
      end if

	!! update curve number
      if (cnop > 0.) call curno(cnop,j)

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
	rtfr = 0. ! resetting root fraction per layer array to 0

      return
      end subroutine mgt_harvkillop