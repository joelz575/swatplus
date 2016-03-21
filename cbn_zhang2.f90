  subroutine cbn_zhang2
        use parm
        use nutrient_module
        use basin_module

        !!Transput variables;
        !!  STD(:)          : standing dead (kg ha-1)                                               (Not used)
        !!  STDL(:)         : mass of lignin in standing dead (kg ha-1)                             (Not used)
        !!  STDN(:)         : mass of N in standing dead (dead plants + sorbed from soil; kg ha-1)  (Not used)
        !!  STDNEl(:)       : standing dead N after enrichment with sorbed N in a soil layer (kg ha-1)

        !!==============================================
        !! local variables
       !ABCO2   : allocation from biomass to CO2; 0.6 (surface litter), 0.85–0.68*(CLAF + SILF) (all other layers) (Parton et al., 1993, 1994)
       !ABL     : carbon allocation from biomass to leaching; ABL = (1-exp(-f/(0.01* SW+ 0.1*(KdBM)*DB)) (Williams, 1995)
       !ABP     : allocation from biomass to passive humus; 0 (surface litter), 0.003 + 0.032*CLAF (all other layers) (Parton et al., 1993, 1994)
       !ALMCO2  : allocation from metabolic litter to CO2; 0.6 (surface litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
       !ALSLCO2 : allocation from lignin of structural litter to CO2; 0.3 (Parton et al., 1993, 1994)
       !ALSLNCO2: allocation from non-lignin of structural litter to CO2; 0.6 (surface litter), 0.55 (all other layers) (Parton et al., 1993, 1994)
       !APCO2   : allocation from passive humus to CO2; 0.55 (Parton et al., 1993, 1994)
       !ASCO2   : allocation from slow humus to CO2; 0.55 (Parton et al., 1993, 1994)
       !ASP     : allocation from slow humus to passive; 0 (surface litter), 0.003-0.009*CLAF (all other layers) (Parton et al., 1993, 1994)
       !BMC     : mass of C in soil microbial biomass and associated products (kg ha-1)
       !BMCTP   : potential transformation of C in microbial biomass (kg ha-1 day-1)
       !BMN     : mass of N in soil microbial biomass and associated products (kg ha-1)
       !BMNTP   : potential transformation of N in microbial biomass (kg ha-1 day-1)
       !BMR     : rate of transformation of microbial biomass and associated products under optimal
       !            conditions (surface = 0.0164 day-1; all other layers = 0.02 day-1) (Parton et al., 1993, 1994)       
       !Cf      : carbon fraction of organic materials 0.42; from data of Pinck et al., 1950)
       !CDG     : soil temperature control on biological processes
       !CNR     : C/N ratio of standing dead
       !CS      : combined factor controlling biological processes [CS = sqrt(CDG×SUT)* 0.8*OX*X1), CS < 10; CS = 10, CS>=10 (Williams, 1995)]
       !DBp     : soil bulk density of plow layer (Mg m-3) (Not used)
       !HSCTP   : potential transformation of C in slow humus (kg ha-1 day-1)
       !HSNTP   : potential transformation of N in slow humus (kg ha-1 day-1)
       !HPCTP   : potential transformation of C in passive humus (kg ha-1 day-1)
       !HPNTP   : potential transformation of N in passive humus (kg ha-1 day-1)
       !HPR     : rate of transformation of passive humus under optimal conditions (subsurface
                !layers = 0.000012 day-1) (Parton et al.,1993, 1994)
       !HSR     : rate of transformation of slow humus under optimal conditions (all layers
                != 0.0005 day-1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
       !KOC     : liquid–solid partition coefficient for microbial biomass (10^3 m^3 Mg-1)     
       !LMF     : fraction of the litter that is metabolic    
       !LMNF    : fraction of metabolic litter that is N (kg kg-1)  
       !LMR     : rate of transformation of metabolic litter under optimal conditions (surface =
                !0.0405 day-1; all other layers = 0.0507 day-1) (Parton et al., 1994)
       !LMCTP   : potential transformation of C in metabolic litter (kg ha-1 day-1)
       !LMNTP   : potential transformation of N in metabolic litter (kg ha-1 day-1)
       !LSCTP   : potential transformation of C in structural litter (kg ha-1 day-1)
       !LSF     : fraction of the litter that is structural
       !LSLF    : fraction of structural litter that is lignin (kg kg-1)
       !LSNF    : fraction of structural litter that is N (kg kg-1)
       !LSLCTP  : potential transformation of C in lignin of structural litter (kg ha-1 day-1)
       !LSLNCTP : potential transformation of C in nonlignin structural litter (kg ha-1 day-1)  
       !LSNTP   : potential transformation of N in structural litter (kg ha-1 day-1)
       !LSR     : rate of potential transformation of structural litter under optimal conditions
                !(surface = 0.0107 day-1; all other layers= 0.0132 day-1) (Parton et al., 1994)
       !NCBM    : N/C ratio of biomass
       !NCHP    : N/C ratio passive humus
       !NCHS    : N/C ratio of the slow humus
       !OX      : oxygen control on biological processes with soil depth
       !Sf      : fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter     
       !SUT     : soil water control on biological processes 
       !X1      : tillage control on residue decomposition (Not used)
       !XBMT    : control on transformation of microbial biomass by soil texture and structure.
                !Its values: surface litter layer = 1; all other layers = 1-0.75*(SILF + CLAF) (Parton et al., 1993, 1994)
       !XLSLF   : control on potential transformation of structural litter by lignin fraction
                !of structural litter [XLSLF = exp(-3* LSLF) (Parton et al., 1993, 1994)]
       !PRMT_51 !COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),     
       integer :: j, k, kk
       real :: sol_mass, sol_min_n  
       real :: fc, wc, sat, void, sut, cdg, OX, CS
       real :: X1,X3, XX
       real :: LMF, LSF, LSLF, XLSLF, LSR, BMR, XBMT, HSR, HPR       
       real :: LSCTA, LSLCTA, LSLNCTA,LSNTA, LMCTA, LMNTA, BMCTA, BMNTA, HSCTA, HSNTA, HPCTA, HPNTA
       real :: LSCTP, LSLCTP, LSLNCTP, LSNTP, LMR, LMCTP, LMNTP, BMCTP,HSCTP, HSNTP, HPCTP, HPNTP
       real :: NCHP, Nf, NCBM, NCHS, ALSLCO2, ALSLNCO2,ALMCO2,ABCO2, A1CO2, APCO2, ASCO2, ABP, ASP, A1, ASX, APX
       real :: PRMT_51
       real :: DF1, DF2, SNMN,  DF3, DF4, DF5, DF6, ADD, ADF1, ADF2, ADF3, ADF4, ADF5
       real :: TOT
       real :: PN1, PN2, PN3, PN4, PN5, PN6, PN7, PN8, PN9
       real :: SUM, CPN1, CPN2, CPN3, CPN4, CPN5
       real :: WMIN,DMDN, wdn, Delta_BMC, DeltaWN
       !! initilize local variables
       DeltaWN = 0.
       DeltaBMC = 0.   
       wdn = 0.   
       X1 = 0.
       X3 = 0.
       XX = 0.
       fc = 0.
       wc = 0.
       sat = 0.
       void = 0.
       sut = 0.
       cdg = 0.
       OX = 0.
       CS = 0.
       LMF = 0.
       LSF = 0.
       LSLF = 0.
       XLSLF = 0.
       LSR = 0.
       LSCTP = 0.
       LSCTA = 0.
       LSLCTA = 0.
       LSLNCTA = 0.
       SNTA = 0.
       LMCTA = 0.
       LMNTA = 0.
       BMCTA = 0.
       BMNTA = 0.
       HSCTA = 0.
       HSNTA= 0. 
       HPCTA= 0.
       HPNTA= 0.
       LSLCTP= 0.
       LSLNCTP= 0.
       LSNTP= 0.
       LMR= 0.
       LMCTP= 0.
       LMNTP= 0.
       BMR= 0.
       XBMT= 0.
       BMCTP= 0.
       HSR= 0.
       HSCTP= 0.
       HSNTP= 0.
       HPR= 0.
       HPCTP= 0.
       HPNTP= 0.
       NCHP= 0.
       Nf= 0.
       NCBM= 0.
       NCHS= 0.
       ALSLCO2= 0.
       ALSLNCO2= 0.
       ALMCO2= 0.
       ABCO2= 0.
       A1CO2= 0.
       APCO2= 0.
       ASCO2= 0.
       ABP= 0.
       ASP= 0.
       A1= 0.
       ASX= 0.
       APX= 0.
       DF1= 0.
       DF2= 0.
       SNMN= 0.
       DF3= 0.
       DF4= 0.
       DF5= 0.
       DF6= 0.
       ADD= 0.
       ADF1= 0.
       ADF2= 0.
       ADF3= 0.
       ADF4= 0.
       ADF5= 0.
       PN1= 0.
       PN2= 0.
       PN3= 0.
       PN4= 0.
       PN5= 0.
       PN6= 0.
       PN7= 0.
       PN8= 0.
       PN9= 0.
       TOT= 0.
       SUM= 0.
       CPN1= 0.
       CPN2= 0.
       CPN3= 0.
       CPN4= 0.
       CPN5= 0.
       WMIN= 0.
       DMDN= 0.
        
       j=0
       j=ihru
        
       do k = 1, hru(j)%sol%nly
          ! a simple equation to calculate Bulk Density from DSSAT (Not Used)  
          !XZ = sol_cbn(k,j)  
          !sol_BDM(k)=ZZ/(1./BD(J)-XZ/.224)  
       end do 

        !!for debug purpose by zhang
       if (time%yrc == 1941 .and. i==134) then
        !write(*,*) 'stop'
       end if    

      !calculate tillage factor using DSSAT
      if (tillage_switch(j) .eq. 1 .and. tillage_days(j) .le. 30) then
         tillage_factor(j) = 1.6
      else
         tillage_factor(j) = 1.0
      end if	
      !calculate tillage factor using DSSAT

      
      !!calculate C/N dynamics for each soil layer
      !!===========================================
       do k = 1, hru(j)%sol%nly
          if (k == 1) then
            !10 cm / 1000 = 0.01m; 1 ha = 10000 m2; ton/m3; * 1000 --> final unit is kg/ha; rock fraction is considered
 		    sol_mass = (10) / 1000.* 10000. * soil(j)%phys(k)%bd* 1000. *(1- soil(j)%phys(k)%rock / 100.)            
          else
		    sol_mass = (soil(j)%phys(k)%d - soil(j)%phys(k-1)%d) / 1000.* 10000. * soil(j)%phys(k)%bd* 1000. *(1- soil(j)%phys(k)%rock / 100.)
	      end if        
         
          !!If k = 1, then using temperature, soil moisture in layer 2 to calculate decomposition factor
          !!Not 
	      kk =0 
	      if (k == 1) then
	        kk = 2
	      else
	        kk = k
	      end if	  	
          !! mineralization can occur only if temp above 0 deg
          !check sol_st soil water content in each soil ayer mm H2O
          if (soil(j)%phys(k)%tmp > 0. .AND. soil(j)%phys(k)%st > 0.) then
              !!from Armen
              !!compute soil water factor - sut
              fc = soil(j)%phys(k)%fc + soil(j)%phys(k)%wpmm        ! units mm
              wc = soil(j)%phys(k)%st + soil(j)%phys(k)%wpmm        ! units mm
              sat = soil(j)%phys(k)%ul + soil(j)%phys(k)%wpmm       ! units mm
              void = soil(j)%phys(k)%por * (1. - wc / sat)   ! fraction
              !!from Armen              
              
              sut = 0.
              
              X1=wc-soil(j)%phys(k)%wpmm
	          IF(X1<0.)THEN
	              SUT=.1*(soil(j)%phys(kk)%st /soil(j)%phys(k)%wpmm)**2
	          ELSE
	              SUT = .1 + .9 * Sqrt(soil(j)%phys(k)%st / soil(j)%phys(k)%fc)
              END IF             
              sut = Min(1., sut)
              sut = amax1(.05, sut)
              !check X1, FC, S15
 
              !from Armen 
              !wf = fwf(fc,wc,sol_wpmm(kk,j))
              !of = fof(void,sol_por(kk,j))
              !sut = wf * of
              !from Armen
              
 		      !!compute tillage factor (X1)
		      !use the tillfactor module from Armen
		      !X1 = ftilf(tillagef(kk,j), wc, sat) 
              X1 = 1.0

		      !calculate tillage factor using DSSAT
		      if (tillage_switch(j) .eq. 1 .and. tillage_days(j) .le. 30) then
		         if (k == 1) then
                     X1 = 1.6
		         else
		             if (soil(j)%phys(k)%d .le. tillage_depth(j)) then
		                X1 = 1.6
		             elseif (soil(j)%phys(k-1)%d .lt. tillage_depth(j)) then
		                X1 = 1.0 + 0.6*(tillage_depth(j) - soil(j)%phys(k-1)%d)/(soil(j)%phys(k)%d- soil(j)%phys(k-1)%d)
		             end if		         
		         end if
		      else
		         X1 = 1.0
		      end if	
		      !calculate tillage factor using DSSAT

              !!compute soil temperature factor
              !!When sol_tep is larger than 35, cdg is negative?
		      cdg = 0.
		      !if (sol_tmp(kk,j) <= 35.0) then
			     !cdg = sol_tmp(kk,j) / (sol_tmp(kk,j) + exp(5.058 - 0.2504 * sol_tmp(kk,j)))
			     cdg = soil(j)%phys(k)%tmp / (soil(j)%phys(k)%tmp + exp(5.058459 - 0.2503591 * soil(j)%phys(k)%tmp))

		      !end if

    		  !!from Armen
    		  cdg = fcgd(soil(j)%phys(k)%tmp)
    		  !!from Armen
    		  
		      !!compute oxygen (OX)
		      OX = 0.
		      !OX = 1 - (0.9* sol_z(k,j)/1000.) / (sol_z(k,j)/1000.+ exp(1.50-3.99*sol_z(k,j)/1000.))
		      !OX = 1 - (0.8* sol_z(k,j)) / (sol_z(k,j)+ exp(1.50-3.99*sol_z(k,j)))  
    	      OX=1.-0.8*((soil(j)%phys(kk)%d+soil(j)%phys(kk-1)%d)/2)/(((soil(j)%phys(kk)%d+soil(j)%phys(kk-1)%d)/2)+EXP(18.40961-0.023683632*((soil(j)%phys(kk)%d+soil(j)%phys(kk-1)%d)/2)))  	 			
              !! compute combined factor
		      CS = 0.
		      CS=MIN(10.,SQRT(cdg*sut)*0.9*OX*X1)              
               !! call denitrification (to use void and cdg factor)
                wdn = 0.
                cdg = fcgd(soil(j)%phys(k)%tmp)
                if (cdg > 0. .and. void <= 0.1) then
                    call nut_denit(k,j,cdg,wdn,void)
                end if
                wdntl = wdntl + wdn

              sol_min_n = soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3
              
              
              !lignin content in structural litter (fraction)          
              RLR = 0.
              RLR = min(0.8,soil(j)%cbn(k)%lsl/(soil(j)%cbn(k)%ls + 1.E-5))  

	          !HSR=PRMT(47) !CENTURY SLOW HUMUS TRANSFORMATION RATE D^-1(0.00041_0.00068) ORIGINAL VALUE = 0.000548,
	          HSR = 5.4799998E-04
              !HPR=PRMT(48) !CENTURY PASSIVE HUMUS TRANSFORMATION RATE D^-1(0.0000082_0.000015) ORIGINAL VALUE = 0.000012 
              HPR = 1.2000000E-05

              APCO2=.55
              ASCO2=.60
              PRMT_51 =0.   !COEF ADJUSTS MICROBIAL ACTIVITY FUNCTION IN TOP SOIL LAYER (0.1_1.),
              PRMT_51 = 1.
              IF(k==1)THEN
                  CS=CS*PRMT_51
                  ABCO2=.55
                  A1CO2=.55
                  BMR=.0164
                  LMR=.0405
                  LSR=.0107
                  NCHP=.1
                  XBM=1.
            !     COMPUTE N/C RATIOS
                  !X1=.1*(WLMN(LD1)+WLSN(LD1))/(RSD(LD1)+1.E-5)
                  X1 = 0.1*(soil(j)%cbn(k)%lsn+soil(j)%cbn(k)%lmn)/(soil(j)%ly(k)%rsd/1000+1.E-5) !relative notrogen content in residue
                  IF(X1>2.)THEN
                      NCBM=.1
                      GO TO 6
                  END IF
                  IF(X1>.01)THEN
                      NCBM=1./(20.05-5.0251*X1)
                  ELSE
                      NCBM=.05
                  END IF    
                6 NCHS=NCBM/(5.*NCBM+1.)
                  GO TO 2
              END IF
              !ABCO2=.17+.0068*SAN(ISL)
              ABCO2 = 0.17 + 0.0068 * soil(j)%phys(k)%sand
              A1CO2=.55
              BMR=.02
              LMR=.0507
              LSR=.0132
              XBM=.25+.0075*soil(j)%phys(k)%sand
              !X1=1000.*(WNH3(ISL)+WNO3(ISL))/WT(ISL)
              X1 = 1000. * sol_min_n/(sol_mass/1000)
              IF(X1>7.15)THEN
                  NCBM=.33
                  NCHS=.083
                  NCHP=.143       
              ELSE
                  NCBM=1./(15.-1.678*X1)
                  NCHS=1./(20.-1.119*X1)
                  NCHP=1./(10.-.42*X1)
              END IF
            2 ABP=.003+.00032*soil(j)%phys(k)%clay
              !SMS(3,ISL)=SMS(3,ISL)+CS
              PRMT_45 = 0.  !COEF IN CENTURY EQ ALLOCATING SLOW TO PASSIVE HUMUS(0.001_0.05) ORIGINAL VALUE = 0.003,
              PRMT_45 = 5.0000001E-02
              ASP=MAX(.001,PRMT_45-.00009*soil(j)%phys(k)%clay)
        !     POTENTIAL TRANSFORMATIONS STRUCTURAL LITTER
              X1=LSR*CS*EXP(-3.*RLR)
              LSCTP=X1*soil(j)%cbn(k)%lsc
              LSLCTP=LSCTP*RLR
              LSLNCTP=LSCTP*(1.-RLR)
              LSNTP=X1 * soil(j)%cbn(k)%lsn
        !     POTENTIAL TRANSFORMATIONS METABOLIC LITTER
              X1=LMR*CS
              LMCTP=soil(j)%cbn(k)%lmc*X1
              LMNTP=soil(j)%cbn(k)%lmn*X1
        !     POTENTIAL TRANSFORMATIONS MICROBIAL BIOMASS
              X1=BMR*CS*XBM
              BMCTP = soil(j)%cbn(k)%bmc * X1
              BMNTP=soil(j)%cbn(k)%bmn*X1
        !     POTENTIAL TRANSFORMATIONS SLOW HUMUS
              X1=HSR*CS
              HSCTP=soil(j)%cbn(k)%hsc*X1
              HSNTP=soil(j)%cbn(k)%hsn*X1
        !     POTENTIAL TRANSFORMATIONS PASSIVE HUMUS
              X1=CS*HPR
              HPCTP=soil(j)%cbn(k)%hpc*X1
              HPNTP=soil(j)%cbn(k)%hpn*X1
        !     ESTIMATE N DEMAND
              A1=1.-A1CO2
              ASX=1.-ASCO2-ASP
              APX=1.-APCO2      
                
              PN1=LSLNCTP*A1*NCBM               !Structural Litter to Biomass
              PN2=.7*LSLCTP*NCHS               !Structural Litter to Slow
              PN3=LMCTP*A1*NCBM                 !Metabolic Litter to Biomass
              !PN4=BMCTP*ABL*NCBM                !Biomass to Leaching (calculated in NCsed_leach)
              PN5=BMCTP*ABP*NCHP                !Biomass to Passive
              PN6=BMCTP*(1.-ABP-ABCO2)*NCHS     !Biomass to Slow
              PN7=HSCTP*ASX*NCBM                !Slow to Biomass
              PN8=HSCTP*ASP*NCHP                !Slow to Passive
              PN9=HPCTP*APX*NCBM                !Passive to Biomass
 
              !PN1=LSLNCTP*A1*NCBM
              !PN2=.7*LSLCTP*NCHS
              !PN3=LMCTP*A1*NCBM
              !PN5=BMCTP*ABP*NCHP
              !PN6=BMCTP*(1.-ABP-ABCO2)*NCHS
              !PN7=HSCTP*ASX*NCBM
              !PN8=HSCTP*ASP*NCHP
              !PN9=HPCTP*APX*NCBM
        !     COMPARE SUPPLY AND DEMAND FOR N
              SUM=0.
              CPN1=0.
              CPN2=0.
              CPN3=0.
              CPN4=0.
              CPN5=0.
              X1=PN1+PN2
              IF(LSNTP<X1)THEN
                  CPN1=X1-LSNTP
              ELSE
                  SUM=SUM+LSNTP-X1
              END IF
              IF(LMNTP<PN3)THEN
                  CPN2=PN3-LMNTP
              ELSE
                  SUM=SUM+LMNTP-PN3
              END IF
              X1=PN5+PN6
              IF(BMNTP<X1)THEN
                  CPN3=X1-BMNTP
              ELSE
                  SUM=SUM+BMNTP-X1
              END IF      
              X1=PN7+PN8
              IF(HSNTP<X1)THEN
                  CPN4=X1-HSNTP
              ELSE
                  SUM=SUM+HSNTP-X1
              END IF
              IF(HPNTP<PN9)THEN
                  CPN5=PN9-HPNTP
              ELSE
                  SUM=SUM+HPNTP-PN9
              END IF
        !     WNH3(ISL)=WNH3(ISL)+SUM
              !total available N
              WMIN=MAX(1.E-5,soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3+SUM)
              !total demand for potential tranformaiton of SOM
              DMDN=CPN1+CPN2+CPN3+CPN4+CPN5
              
              X3=1.
        !     REDUCE DEMAND IF SUPPLY LIMITS
              IF(WMIN<DMDN) then
                  X3=WMIN/DMDN
              end if
              !SMS(5,ISL)=SMS(5,ISL)+X3
        !     ACTUAL TRANSFORMATIONS
              IF(CPN1>0.)THEN
                  LSCTA=LSCTP*X3
                  LSNTA=LSNTP*X3
                  LSLCTA=LSLCTP*X3
                  LSLNCTA=LSLNCTP*X3
              ELSE
                  LSCTA=LSCTP
                  LSNTA=LSNTP
                  LSLCTA=LSLCTP
                  LSLNCAT=LSLNCTP
              END IF
              IF(CPN2>0.)THEN
                  LMCTA=LMCTP*X3
                  LMNTA=LMNTP*X3
              ELSE
                  LMCTA=LMCTP
                  LMNTA=LMNTP
              END IF
              IF(CPN3>0.)THEN
                  BMCTA=BMCTP*X3
                  BMNTA=BMNTP*X3
              ELSE
                  BMCTA=BMCTP
                  BMNTA=BMNTP
              END IF
              IF(CPN4>0.)THEN
                  HSCTA=HSCTP*X3
                  HSNTA=HSNTP*X3
              ELSE
                  HSCTA=HSCTP
                  HSNTA=HSNTP
              END IF
              IF(CPN5>0.)THEN
                  HPCTA=HPCTP*X3
                  HPNTA=HPNTP*X3
              ELSE
                  HPCTA=HPCTP
                  HPNTA=HPNTP
              END IF        
              
              !Recalculate demand
              !revised from EPIC code by Zhang
                  PN1=LSLNCTA*A1*NCBM               !Structural Litter to Biomass
                  PN2=.7*LSLCTA*NCHS               !Structural Litter to Slow
                  PN3=LMCTA*A1*NCBM                 !Metabolic Litter to Biomass
                  !PN4=BMCTP*ABL*NCBM                !Biomass to Leaching (calculated to NCsed_leach)
                  PN5=BMCTA*ABP*NCHP                !Biomass to Passive
                  PN6=BMCTA*(1.-ABP-ABCO2)*NCHS     !Biomass to Slow
                  PN7=HSCTA*ASX*NCBM                !Slow to Biomass
                  PN8=HSCTA*ASP*NCHP                !Slow to Passive
                  PN9=HPCTA*APX*NCBM                !Passive to Biomass              
            !     COMPARE SUPPLY AND DEMAND FOR N
                  SUM=0.
                  CPN1=0.
                  CPN2=0.
                  CPN3=0.
                  CPN4=0.
                  CPN5=0.
                  X1=PN1+PN2
                  IF(LSNTA<X1)THEN
                      CPN1=X1-LSNTA
                  ELSE
                      SUM=SUM+LSNTA-X1
                  END IF
                  IF(LMNTA<PN3)THEN
                      CPN2=PN3-LMNTA
                  ELSE
                      SUM=SUM+LMNTA-PN3
                  END IF
                  X1=PN5+PN6
                  IF(BMNTA<X1)THEN
                      CPN3=X1-BMNTA
                  ELSE
                      SUM=SUM+BMNTA-X1
                  END IF      
                  X1=PN7+PN8
                  IF(HSNTA<X1)THEN
                      CPN4=X1-HSNTA
                  ELSE
                      SUM=SUM+HSNTA-X1
                  END IF
                  IF(HPNTA<PN9)THEN
                      CPN5=PN9-HPNTA
                  ELSE
                      SUM=SUM+HPNTA-PN9
                  END IF
            !     WNH3(ISL)=WNH3(ISL)+SUM
                  !total available N
                  WMIN=MAX(1.E-5,soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3 + SUM)
                  !total demand for potential tranformaiton of SOM
                  DMDN=CPN1+CPN2+CPN3+CPN4+CPN5              
                              

              !DMDN=DMDN*X3
              !SGMN=SGMN+SUM
              !supply - demand
              soil(j)%cbn(k)%rnmn=SUM-DMDN
        !     UPDATE
              IF(soil(j)%cbn(k)%rnmn>0.)THEN
                  soil(j)%nut(k)%nh3=soil(j)%nut(k)%nh3+soil(j)%cbn(k)%rnmn
        !  	      WNO3(ISL)=WNO3(ISL)+RNMN(ISL)
                  GO TO 21
              END IF      
	          X1=soil(j)%nut(k)%no3+soil(j)%cbn(k)%rnmn
	          IF(X1<0.)THEN
	              soil(j)%cbn(k)%rnmn=-soil(j)%nut(k)%no3
	              soil(j)%nut(k)%no3=1.E-10
	          ELSE
	              soil(j)%nut(k)%no3=X1
              END IF   
           21 DF1=LSNTA
            
              DF2=LMNTA
	          !!Demand represents Demand from   
	          !SNMN=SNMN+sol_RNMN(k,j)

	          !calculate P flows
              !! compute humus mineralization on active organic p
              hmp = 0.
              hmp_rate = 0.
              hmp_rate = 1.4* (HSNTA + HPNTA)/(soil(j)%cbn(k)%hsn + soil(j)%cbn(k)%hpn + 1.e-6)
              !hmp_rate = 1.4* (HSNTA )/(sol_HSN(k,j) + sol_HPN(k,j) + 1.e-6)
              hmp = hmp_rate*soil(j)%nut(k)%orgp
              hmp = Min(hmp, soil(j)%nut(k)%orgp)
              soil(j)%nut(k)%orgp = soil(j)%nut(k)%orgp - hmp
              soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + hmp	          
	
	          !! compute residue decomp and mineralization of 
              !! fresh organic n and p (upper two layers only)  
                rmp = 0.             
                decr = 0.
                decr = (LSCTA + LMCTA)/(soil(j)%cbn(k)%lsc + soil(j)%cbn(k)%lmc + 1.e-6)
                decr = min(1., decr)
                rmp = decr * soil(j)%nut(k)%fop

                soil(j)%nut(k)%fop = soil(j)%nut(k)%fop - rmp
                soil(j)%nut(k)%solp= soil(j)%nut(k)%solp + .8 * rmp
                soil(j)%nut(k)%orgp = soil(j)%nut(k)%orgp + .2 * rmp	          
	          !calculate P flows
       
	          
	          !SMS(9,ISL)=SMS(9,ISL)+RNMN(ISL)
	          LSCTA = Min(soil(j)%cbn(k)%lsc,LSCTA)
              soil(j)%cbn(k)%lsc=MAX(1.E-10,soil(j)%cbn(k)%lsc-LSCTA)
              LSLCTA = min(soil(j)%cbn(k)%lslc,LSLCTA)
              soil(j)%cbn(k)%lslc=MAX(1.E-10,soil(j)%cbn(k)%lslc-LSLCTA)
              soil(j)%cbn(k)%lslnc=MAX(1.E-10,soil(j)%cbn(k)%lslnc-LSLNCTA)
              LMCTA=MIN(soil(j)%cbn(k)%lmc,LMCTA)
              IF (soil(j)%cbn(k)%lm > 0.) THEN
                RTO = MAX(0.42,soil(j)%cbn(k)%lmc/soil(j)%cbn(k)%lm)
                soil(j)%cbn(k)%lm = soil(j)%cbn(k)%lm - LMCTA/RTO
                soil(j)%cbn(k)%lmc = soil(j)%cbn(k)%lmc - LMCTA
              END IF
              !sol_LMC(k,j)=MAX(1.E-10,sol_LMC(k,j)-LMCTA)
              !sol_LM(k,j)=MAX(1.E-10,sol_LM(k,j)-LMCTA/.42)
              soil(j)%cbn(k)%lsl=MAX(1.E-10,soil(j)%cbn(k)%lsl-LSLCTA/.42)
              soil(j)%cbn(k)%ls=MAX(1.E-10,soil(j)%cbn(k)%ls-LSCTA/.42)
              
              X3=APX*HPCTA+ASX*HSCTA+A1*(LMCTA+LSLNCTA)
              soil(j)%cbn(k)%bmc=soil(j)%cbn(k)%bmc-BMCTA+X3
              !DeltaBMC = DeltaBMC -BMCTA+X3
              DF3=BMNTA-NCBM*X3
              !!DF3 is the supply of BMNTA - demand of N to meet the Passive, Slow, Metabolic, and Non-lignin Structural 
              !! C pools transformaitons into microbiomass pool
              X1=.7*LSLCTA+BMCTA*(1.-ABP-ABCO2)
              soil(j)%cbn(k)%hsc=soil(j)%cbn(k)%hsc-HSCTA+X1
              DF4=HSNTA-NCHS*X1
              !!DF4 Slow pool supply of N - N demand for microbiomass C transformed into slow pool
              X1=HSCTA*ASP+BMCTA*ABP
              soil(j)%cbn(k)%hpc=soil(j)%cbn(k)%hpc-HPCTA+X1
              DF5=HPNTA-NCHP*X1
              !!DF5 Passive pool demand of N - N demand for microbiomass C transformed into passive pool
              DF6=sol_min_n-soil(j)%nut(k)%no3-soil(j)%nut(k)%nh3
              !!DF6 Supply of mineral N - available mineral N = N demanded from mineral pool
              !SMS(10,ISL)=SMS(10,ISL)-DF6
              ADD=DF1+DF2+DF3+DF4+DF5+DF6
              ADF1=ABS(DF1)
              ADF2=ABS(DF2)
              ADF3=ABS(DF3)
              ADF4=ABS(DF4)
              ADF5=ABS(DF5)
              TOT=ADF1+ADF2+ADF3+ADF4+ADF5
              XX=ADD/(TOT+1.E-10)
              soil(j)%cbn(k)%lsn=MAX(.001,soil(j)%cbn(k)%lsn-DF1+XX*ADF1)
              soil(j)%cbn(k)%lmn=MAX(.001,soil(j)%cbn(k)%lmn-DF2+XX*ADF2)
              soil(j)%cbn(k)%bmn=soil(j)%cbn(k)%bmn-DF3+XX*ADF3
              soil(j)%cbn(k)%hsn=soil(j)%cbn(k)%hsn-DF4+XX*ADF4
              soil(j)%cbn(k)%hpn=soil(j)%cbn(k)%hpn-DF5+XX*ADF5
              soil(j)%cbn(k)%rspc=.3*LSLCTA+A1CO2*(LSLNCTA+LMCTA)+ABCO2*BMCTA+ASCO2*HSCTA+APCO2*HPCTA
              rspc_d(j) = rspc_d(j) +  soil(j)%cbn(k)%rspc 
              !SMM(74,MO)=SMM(74,MO)+RSPC(ISL)
              !SMS(8,ISL)=SMS(8,ISL)+RSPC(ISL)
              !TRSP=TRSP+RSPC(ISL)      
              !VAR(74)=VAR(74)+RSPC(ISL)
              !RSD(ISL)=.001*(WLS(ISL)+WLM(ISL)) 
              soil(j)%ly(k)%rsd = soil(j)%cbn(k)%ls+soil(j)%cbn(k)%lm            
              soil(j)%nut(k)%orgn = soil(j)%cbn(k)%hpn
              soil(j)%nut(k)%aorgn = soil(j)%cbn(k)%hsn
              soil(j)%nut(k)%fon = soil(j)%cbn(k)%lmn + soil(j)%cbn(k)%lsn 
              soil(j)%cbn(k)%cbn = 100*(soil(j)%cbn(k)%lsc+soil(j)%cbn(k)%lmc +soil(j)%cbn(k)%hsc + soil(j)%cbn(k)%hpc + soil(j)%cbn(k)%bmc)/sol_mass    
 
!! septic changes 1/28/09 gsm
!!  compute denitrification while simulating septic tank
      !wdn = 0.   
	  !if (i_sep(j) /= k .and. ipop_sep(j) > 0) then
          !! compute soil water factor
      !    sut = 0.
	        !! change for domain error 1/29/09 gsm check with Jeff !!!
 	  !    if (sol_st(kk,j) < 0.) sol_st(kk,j) = .0000001
      !    sut = .1 + .9 * Sqrt(sol_st(kk,j) / sol_fc(kk,j))
      !    sut = Min(1., sut)
      !    sut = Max(.05, sut)
      !    !!compute soil temperature factor
      !    xx = 0.
      !    cdg = 0.
      !    xx = sol_tmp(kk,j)
      !    cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
      !    cdg = Max(.1, cdg)
          
	  !  if (sut >= bsn_prm%sdnco) then
	  !    wdn = sol_no3(k,j) * (1. - Exp(-bsn_prm%cdn * cdg * sol_cbn(k,j)))
	  !  else
	  !    wdn = 0.
	  !  endif
	  !  sol_no3(k,j) = sol_no3(k,j) - wdn
	  !end if
! septic changes 1/28/09 gsm
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!			call nut_denit(k,j,cdg,wdn,0.05)
	!!	end if             	  
        end if

    end do

    !write (*,*) time%yrc,i, DeltaBMC
    !write (*,*) time%yrc,i, wdntl
    return
    end subroutine cbn_zhang2