      subroutine mgt_dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)    |m**2/m**2     |minimum LAI during winter dormant period
!!    bio_leaf(:)    |none          |fraction of biomass that drops during
!!                                  |dormancy (for trees only)
!!    dayl(:)        |hours         |day length for current day
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    dormhr(:)      |hour          |time threshold used to define dormant
!!                                  |period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum
!!                                  |daylength for the area, the plant will go
!!                                  |dormant)
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    ihru           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use basin_module
      use hydrograph_module
      use jrw_datalib_module
      use organic_mineral_mass_module

      real :: resnew
      integer :: j

      !!by zhang
      !!====================

      real :: BLG1, BLG2, BLG3,  CLG, sf
      real :: sol_min_n,  resnew_n, resnew_ne
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
      idp = pcom(j)%plcur(ipl)%idplt
      iob = hru(j)%obj_no
      iwst = ob(iob)%wst
      iwgn = wst(iwst)%wco%wgn

!! check for beginning of dormant season
      if (pldb(idp)%idc == 1 .or. pldb(idp)%idc == 4) return
      if (pcom(j)%plcur(ipl)%idorm == 0 .and. dayl(iwgn)-dormhr(j)<wgn_pms(iwgn)%daylmn) then
          
        select case (pldb(idp)%idc)
        
          !! make sure all operations are scheduled during growing season of warm season annual
          case (1,4)
            dorm_flag = 1
            call mgt_operatn
            dorm_flag = 0

          !! beginning of forest dormant period
          case (7)
            pcom(j)%plcur(ipl)%idorm = 1
            resnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%bio_leaf
            
            !!add by zhang
            !!===================
            if (bsn_cc%cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + resnew*0.42
            end if
            !!add by zhang
            !!===================

            !!insert new biomss by zhang
            !!=============================
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
	          	          
	          resnew = pcom(j)%plm(ipl)%mass * pcom(j)%plg(ipl)%bio_leaf
	          resnew_n = resnew * pcom(j)%plm(ipl)%n_fr   	    
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
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          rsd1(j)%lig%m = rsd1(j)%lig%m + RLR* LSF * resnew
	          rsd1(j)%str%c = rsd1(j)%str%c + 0.42*LSF * resnew  
	          
	          rsd1(j)%lig%c = rsd1(j)%lig%c + RLR*0.42*LSF*resnew
	          rsd1(j)%lig%n = rsd1(j)%str%c - rsd1(j)%lig%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         rsd1(j)%str%n = rsd1(j)%str%n + 0.42*LSF*resnew / 150
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

            
            rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m + resnew
            rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
            rsd1(j)%tot(ipl)%n = resnew * pcom(j)%plm(ipl)%n_fr +   &
                                              rsd1(j)%tot(ipl)%n
            rsd1(j)%tot(ipl)%p = resnew * pcom(j)%plm(ipl)%p_fr +   &
                                              rsd1(j)%tot(ipl)%p
            pcom(j)%plm(ipl)%mass = pcom(j)%plm(ipl)%mass *                &   
                (1. - pcom(j)%plg(ipl)%bio_leaf)
            pcom(j)%plm(ipl)%nmass = pcom(j)%plm(ipl)%nmass - resnew *     &
              pcom(j)%plm(ipl)%n_fr
            pcom(ihru)%plm(ipl)%pmass =                                    &
             pcom(ihru)%plm(ipl)%pmass - resnew * pcom(j)%plm(ipl)%p_fr
            pcom(j)%plstr(ipl)%strsw = 1.
            pcom(j)%plg(ipl)%lai = pldb(idp)%alai_min
            pcom(j)%plcur(ipl)%phuacc = 0.
            pcom(j)%plg(ipl)%laimxfr = 0.        !Sue White - dormancy

          !! beginning of perennial (pasture/alfalfa) dormant period
          case (3, 6)
            pcom(j)%plcur(ipl)%idorm = 1
            resnew = 0.
            resnew = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%mass

            !!add by zhang
            !!===================
            if (bsn_cc%cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + resnew*0.42
            end if
            !!add by zhang
            !!===================

            !!insert new biomss by zhang
            !!=============================
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
	          	          
	          resnew = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%mass 
	          resnew_n = pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%nmass
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
	          
	          rsd1(j)%lig%c= rsd1(j)%lig%c + RLR*0.42*resnew
	          rsd1(j)%lig%n = rsd1(j)%str%c - rsd1(j)%lig%c
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         rsd1(j)%str%n = rsd1(j)%str%n + 0.42*LSF*resnew/150
		         rsd1(j)%meta%n = rsd1(j)%meta%n + resnew_ne -          &
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


            rsd1(j)%tot(ipl)%m = rsd1(j)%tot(ipl)%m + resnew
            rsd1(j)%tot(ipl)%m = Max(rsd1(j)%tot(ipl)%m, 0.)
            rsd1(j)%tot(ipl)%n = rsd1(j)%tot(ipl)%n +      &
              pldb(idp)%bm_dieoff * pcom(j)%plm(ipl)%nmass
            rsd1(j)%tot(ipl)%p = rsd1(j)%tot(ipl)%p +      &
              pldb(idp)%bm_dieoff * pcom(ihru)%plm(ipl)%pmass
            pcom(j)%plm(ipl)%mass = (1. - pldb(idp)%bm_dieoff) *         &
             pcom(j)%plm(ipl)%mass
            pcom(j)%plm(ipl)%nmass = (1. - pldb(idp)%bm_dieoff) *        &
              pcom(j)%plm(ipl)%nmass
            pcom(ihru)%plm(ipl)%pmass = (1. - pldb(idp)%bm_dieoff) *     &
              pcom(ihru)%plm(ipl)%pmass
            pcom(j)%plstr(ipl)%strsw = 1.

          !! beginning of cool season annual dormant period
          case (2, 5)
            if (pcom(j)%plcur(ipl)%phuacc < 0.75) then
              pcom(j)%plcur(ipl)%idorm = 1
              pcom(j)%plstr(ipl)%strsw = 1.
            end if 
          end select
           if (pco%mgtout ==  'year') then
            write (143, 1000) j, time%yrc, i_mo, iida,                   &
              pldb(idp)%plantnm, "START-DORM", phubase(j),               & 
              pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,                  &
              pcom(j)%plm(ipl)%mass,soil(j)%ly(1)%rsd,                   &
              sol_sumno3(j), sol_sumsolp(j)
           end if
           
          end if

!! check if end of dormant period
        if (pcom(j)%plcur(ipl)%idorm == 1.and.dayl(iwgn) - dormhr(j) >=   &
            wgn_pms(iwgn)%daylmn) then

          select case (pldb(idp)%idc)
          
            !! end of perennial dormant period
            case (3, 6, 7)
              pcom(j)%plcur(ipl)%idorm = 0

            !! end of cool season annual dormant period
            case (2, 5)
              pcom(j)%plcur(ipl)%idorm = 0
              pcom(j)%plcur(ipl)%phuacc = 0.

            end select
            
          if (pco%mgtout == 'year') then
            write (143,1000) j, time%yrc, i_mo, iida,                   & 
              pldb(idp)%plantnm, "END-DORM", phubase(j),                &               
              pcom(j)%plcur(ipl)%phuacc, soil(j)%sw,                 &
              pcom(j)%plm(ipl)%mass,soil(j)%ly(1)%rsd,                  &
              sol_sumno3(j), sol_sumsolp(j)
          end if

        end if

1000  format (4i6,2a15,7f10.2)
      return
      end subroutine mgt_dormant