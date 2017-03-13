      subroutine sep_biozone
	    
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine conducts biophysical processes occuring 
!!    in the biozone layer of a septic HRU.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_bd(:)        |kg/m^3        |density of biomass 
!!    bio_bod(:)       |kg/ha         |BOD concentration in biozone
!!    biom(:)          |kg/ha         |biomass of live bacteria in biozone       
!!    bz_thk(:)        |mm            |thickness of biozone                    
!!    coeff_bod_dc(:)  |m^3/day       |BOD decay rate coefficient
!!    coeff_bod_conv(:)|none          |BOD to live bacteria biomass conversion factor
!!    coeff_denitr(:)  |none          |Denitrification rate coefficient
!!    coeff_fc1(:)     |none          |field capacity calibration parameter 1
!!    coeff_fc2(:)     |none          |field capacity calibration parameter 2  
!!    coeff_fecal(:)   |m^3/day       |Fecal coliform bacteria decay rate coefficient  
!!    coeff_mrt(:)     |none          |mortality rate coefficient          
!!    coeff_nitr(:)    |none          |Nitrification rate coefficient
!!    coeff_plq(:)     |none          |Conversion factor for plaque from TDS           
!!    coeff_rsp(:)     |none          |respiration rate coefficient          
!!    coeff_slg1(:)    |none          |slough-off calibration parameter
!!    coeff_slg2(:)    |none          |slough-off calibration parameter
!!    fcoli(:)         |cfu/100ml     |concentration of the fecal coliform in the biozone 
!!                     |              |septic tank effluent
!!    ihru             |none          |HRU number
!!    iida             |day           |day being simulated
!!    i_sep(:)         |none          |soil layer where biozone exists           
!!    isep_opt(:)      |none          |Septic system operation flag (1=active,2=failing,0=not operated)                 
!!    plqm             |kg/ha         |plaque in biozone
!!    rbiom(:)         |kg/ha         |daily change in biomass of live bacteria
!!    rplqm            |kg/ha         |daily change in plaque
!!    sepday           |mm H2O        |percolation from soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                     |none          |                       

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bod_rt           |1/day         |BOD reaction rate
!!    bz_lyr           |none          |soil layer where biozone exists
!!    bz_vol           |m^3           |volume of biozone
!!    dentr_rt         |1/day         |denitrification reaction rate
!!    fcoli_rt         |1/day         |fecal coliform reaction rate
!!    isp              |none          |type of septic system for current hru
!!    ntr_rt           |1/day         |nitrification reaction rate
!!    rbod             |mg/l          |daily change in bod concentration
!!    rdenit           |kg/ha         |denitrification during the day
!!    rfcoli           |cfu/100ml     |daily change in fecal coliform
!!    rmort            |kg/ha         |daily mortality of bacteria
!!    rnit             |kg/ha         |nitrification during the day
!!    rrsp             |kg/ha         |daily resparation of bacteria
!!    rslg             |kg/ha         |daily slough-off bacteria
!!    rtof             |none          |weighting factor used to partition the 
!!                                    |organic N & P concentration of septic effluent
!!                                    |between the fresh organic and the stable 
!!                                    |organic pools

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    Coded by J.Jeong and C.Santhi. BRC, Temple TX
!!    Septic algorithm adapted from Siegrist et al., 2005

      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module
      
	implicit none
	      
!	real ntr_rt
      integer bz_lyr, isp, ii,j,nly
	real*8 bz_vol, rtrate,bodconc, qin, qout,qmm,qvol,pormm,rplqm
	real*8 ntr_rt,dentr_rt, bod_rt, fcoli_rt,rtof,xx,bodi,bode
	real*8 rnit, rdenit, rbio, rmort, rrsp, rslg, rbod, rfcoli
	real*8 nh3_begin, nh3_end, nh3_inflw_ste, no3_begin, no3_end 
	real*8 no3_inflow_ste, bio_steintobz,bio_outbz,bza,qi,nperc
	real*8 nh3_init, no3_init, hvol, solpconc, solpsorb, qlyr,qsrf
	real*8 n1,n2,n3,n4,n5,n6,n7,n8,p1,p2,p3,p4
	real*8 solp_init,solp_begin,solp_end,svolp,totalp,ctmp

	j = ihru
	nly = soil(j)%nly
      isep = iseptic(j)
	isp = sep(isep)%typ 	   !! J.Jeong 3/09/09
      bz_lyr = i_sep(j)    
	bza = hru(j)%area_ha
	bz_vol = sep(isep)%thk * bza * 10. !m^3
	qlyr = qstemm(j)
	qsrf = 0
	
	!temperature correctioin factor for bacteria growth/dieoff (Eppley, 1972)
	ctmp = thbact ** (soil(j)%phys(bz_lyr)%tmp- 20.) 

	! initial water volume
	qi = (soil(j)%phys(bz_lyr)%st + soil(j)%ly(bz_lyr-1)%prk + qstemm(j)) *   &
                                              bza * 10. !m3
    ! STE volume
	qin = qstemm(j) * bza * 10. ! m^3
	! leaching to sub layer
	qout = bz_perc(j) * bza * 10. !m3/d
	! final volume
	hvol = soil(j)%phys(bz_lyr)%st * bza * 10.
	rtof = 0.5

	nh3_init = soil1(j)%mn(bz_lyr)%nh4
	no3_init = soil1(j)%mn(bz_lyr)%no3
	solp_init = soil1(j)%mp(bz_lyr)%lab

	!! Failing system: STE saturates upper soil layers
	if (sep(isep)%opt == 2) then
	  
	  ! increment the number of failing days
	  if(sep_tsincefail(j)>0) sep_tsincefail(j) = sep_tsincefail(j) + 1

      ! convert the failing system into an active system if duration of failing ends
	  if (sep_tsincefail(j) >= sep(isep)%tfail) then
	     sep(isep)%opt  = 1
         soil(j)%phys(bz_lyr)%ul=sep(isep)%thk *                           &
           (soil(j)%phys(bz_lyr)%por - soil(j)%phys(bz_lyr)%wp) 
         soil(j)%phys(bz_lyr)%fc=sep(isep)%thk*(soil(j)%phys(bz_lyr)%up-   &
           soil(j)%phys(bz_lyr)%wp)
		 soil1(j)%mn(bz_lyr)%nh4 = 0
		 soil1(j)%mn(bz_lyr)%no3 = 0
		 soil1(j)%hp(bz_lyr)%n = 0
		 soil1(j)%hp(bz_lyr)%p = 0
		 soil1(j)%tot(bz_lyr)%p = 0    !! nbs ?
		 soil1(j)%mp(bz_lyr)%lab = 0
         soil1(j)%mp(bz_lyr)%act = 0
		 biom(j) = 0		
         plqm(j) = 0
		 bio_bod(j) = 0
		 fcoli(j) = 0
		 sep_tsincefail(j) = 0
	  end if

	  return
	endif

	!! Active system


   !! Water content(eqn 4-12), biozone hydraulic conductivity(eqn 4-9), 
	!! and percolation (eqn 4-8,10,11) are computed in percmain/percmicro


	! Add STE nutrients to appropriate soil pools in mass unit
	xx = qin / bza / 1000. ! used for unit conversion: mg/l -> kg/ha
      soil1(j)%mn(bz_lyr)%no3 = soil1(j)%mn(bz_lyr)%no3 + xx *            &
                    (sepdb(sep(isep)%typ)%no3concs +                      &                   
                     sepdb(sep(isep)%typ)%no2concs)  
      soil1(j)%mn(bz_lyr)%nh4 = soil1(j)%mn(bz_lyr)%nh4 + xx *            &
                                    sepdb(sep(isep)%typ)%nh4concs 
      soil1(j)%hp(bz_lyr)%n = soil1(j)%hp(bz_lyr)%n + xx *                & 
                                   sepdb(sep(isep)%typ)%orgnconcs*rtof
      soil1(j)%tot(bz_lyr)%n = soil1(j)%tot(bz_lyr)%n +                 &
               xx*sepdb(sep(isep)%typ)%orgnconcs*(1-rtof)
      soil1(j)%hp(bz_lyr)%p = soil1(j)%hp(bz_lyr)%p + xx *                &
                                    sepdb(sep(isep)%typ)%orgps*rtof
      soil1(j)%tot(bz_lyr)%p = soil1(j)%tot(bz_lyr)%p + xx *              &
                                    sepdb(sep(isep)%typ)%orgps*           &        
                                    (1-rtof)
      soil1(j)%mp(bz_lyr)%lab = soil1(j)%mp(bz_lyr)%lab + xx*             &
                     sepdb(sep(isep)%typ)%minps  
      bio_bod(j)=bio_bod(j)+xx*sepdb(sep(isep)%typ)%bodconcs   ! J.Jeong 4/03/09

      bodi = bio_bod(j) * bza / qi * 1000.  !mg/l

	!! Field capacity in the biozone Eq. 4-6  ! 
      soil(j)%phys(bz_lyr)%fc = soil(j)%phys(bz_lyr)%fc + sep(isep)%fc1   &
        * (soil(j)%phys(bz_lyr)%ul - soil(j)%phys(bz_lyr)%fc) **          &
        sep(isep)%fc2 * rbiom(j) / (sep(isep)%bd * 10)

	!! Saturated water content in the biozone - Eq. 4-7    
	! mm = mm - kg/ha / (kg/m^3 * 10)
      soil(j)%phys(bz_lyr)%ul = soil(j)%phys(bz_lyr)%por *                & 
                          sep(isep)%thk-plqm(j) /(sep(isep)%bd*10.)

	if(soil(j)%phys(bz_lyr)%ul.le.soil(j)%phys(bz_lyr)%fc) then
	  soil(j)%phys(bz_lyr)%ul = soil(j)%phys(bz_lyr)%fc
	  sep(isep)%opt  = 2
	endif
     

	!! Respiration rate(kg/ha)  Eq. 4-2   
	rrsp = ctmp * sep(isep)%rsp * biom(j) 

	!! Mortality rate(kg/ha) Eq. 4-3      
	rmort = ctmp * sep(isep)%mrt * biom(j) 

	!! Slough-off rate(kg/ha)      
	rslg = sep(isep)%slg1 * bz_perc(j) ** sep(isep)%slg2 * biom(j) 
			
	
	!! Build up of plqm(kg/ha) Eq.4-5
	! kg/ha (perday) = kg/ha + dimensionless * m^3/d * mg/l / (1000*ha)
      rplqm = (rmort - rslg) + sep(isep)%plq * qin *                    &                    
                       sepdb(sep(isep)%typ)%tssconcs / (1000. * bza)  
	rplqm = max(0.,rplqm)

	!! Add build up to plqm  ! kg/ha = kg/ha + kg/ha 
      plqm(j) = plqm(j) + rplqm
	
	nh3_inflw_ste = xx * sepdb(sep(isep)%typ)%nh4concs
	no3_inflow_ste = xx*(sepdb(sep(isep)%typ)%no3concs +                   &                  
           sepdb(sep(isep)%typ)%no2concs) 
	nh3_begin = soil1(j)%mn(bz_lyr)%nh4
	no3_begin = soil1(j)%mn(bz_lyr)%no3
	solp_begin = soil1(j)%mp(bz_lyr)%lab

	!! Add STE f.coli concentration by volumetric averaging
      xx = 10.* soil(j)%phys(bz_lyr)%st * bza / (qin                       &
           + 10.* soil(j)%phys(bz_lyr)%st * bza)
	fcoli(j) = fcoli(j) * xx + sepdb(sep(isep)%typ)%fcolis * (1.- xx)      ! J.Jeong 3/09/09
	
	!! nutrients reaction rate (Equation 4-13)
	rtrate =  biom(j) * bza / (bz_vol * soil(j)%phys(bz_lyr)%por)
		      
	!! BOD (kg/ha) 4-14 ! 
 	bod_rt = max(0.,sep(isep)%bod_dc * rtrate)		!bod
      if (bod_rt>4) bod_rt=4
	rbod = bodi * (1.- Exp(-bod_rt))
      bode = bodi - rbod					!mg/l
	bio_bod(j) = bode * (soil(j)%phys(bz_lyr)%st * 10)/1000. !kg/ha

	!! Fecal coliform(cfu/100ml) Eq 4-14, J.Jeong 3/09/09
	fcoli_rt = max(0.,sep(isep)%fecal * rtrate)		!fecal coliform
	rfcoli = fcoli(j) * (1.- exp(-fcoli_rt))
	fcoli(j) = fcoli(j) - rfcoli

	!! change in nh3 & no3 in soil pools due to nitrification(kg/ha) Eq.4-13, 4-14  
	ntr_rt = max(0.,sep(isep)%nitr * rtrate)			!nitrification
	rnit = soil1(j)%mn(bz_lyr)%nh4 * (1. - Exp(-ntr_rt)) !! J.Jeong 4/03/09
	soil1(j)%mn(bz_lyr)%nh4 = soil1(j)%mn(bz_lyr)%nh4 - rnit	!J.Jeong 3/09/09
	soil1(j)%mn(bz_lyr)%no3 = soil1(j)%mn(bz_lyr)%no3 + rnit	!J.Jeong 3/09/09
	
	!ammonium percolation
	nperc = 0.2 * qout / qi * soil1(j)%mn(bz_lyr)%nh4
	nperc = min(nperc,0.5 * soil1(j)%mn(bz_lyr)%nh4)
	soil1(j)%mn(bz_lyr)%nh4 = soil1(j)%mn(bz_lyr)%nh4 - nperc
	soil1(j)%mn(bz_lyr+1)%nh4 = soil1(j)%mn(bz_lyr+1)%nh4 + nperc

	!! denitrification,(kg/ha) Eq 4-14  
	dentr_rt = max(0.,sep(isep)%denitr * rtrate)		!denitrification
      rdenit = soil1(j)%mn(bz_lyr)%no3 * (1. - Exp(-dentr_rt))	!J.Jeong 3/09/09
	soil1(j)%mn(bz_lyr)%no3 = soil1(j)%mn(bz_lyr)%no3 - rdenit		!J.Jeong 3/09/09

 	!soil volume for sorption: soil thickness below biozone 
      svolp = (soil(j)%phys(nly)%d - sep(isep)%z) * bza * 10. !m3, 
   
   !max adsorption amnt: linear isotherm, McCray 2005
      solpconc = soil1(j)%mp(bz_lyr)%lab * bza / qi * 1000. !mg/l
	solpsorb = min(sep(isep)%pdistrb * solpconc,sep(isep)%psorpmax) !mgP/kgSoil
	solpsorb = 1.6 * 1.e-3 * solpsorb * svolp *                    &
            (1-soil(j)%phys(bz_lyr)%por) !kgP sorption potential	

  !check if max. P sorption is reached 
      if(soil1(j)%mp(bz_lyr)%lab * bza<solpsorb) then
       totalp = soil1(j)%mp(bz_lyr)%lab + soil1(j)%mp(bz_lyr)%act 
       solp_end = sep(isep)%solpslp * totalp  + sep(isep)%solpintc
        if (solp_end > soil1(j)%mp(bz_lyr)%lab) then
         solp_end = soil1(j)%mp(bz_lyr)%lab
        endif 
      soil1(j)%mp(bz_lyr)%act = soil1(j)%mp(bz_lyr)%act +          &
           soil1(j)%mp(bz_lyr)%lab - solp_end
      soil1(j)%mp(bz_lyr)%lab = solp_end
      endif	     
      solpconc = soil1(j)%mp(bz_lyr)%lab * bza / qi * 1000. !mg/l
	percp(j) = 0.01*solpconc * qout / bza * 1.e-3
	soil1(j)%mp(bz_lyr)%lab = soil1(j)%mp(bz_lyr)%lab - percp(j) !kg/ha
      soil1(j)%mp(bz_lyr+1)%lab = soil1(j)%mp(bz_lyr+1)%lab + percp(j) !kg/ha	     
      nh3_end = soil1(j)%mn(bz_lyr)%nh4
	no3_end = soil1(j)%mn(bz_lyr)%no3
      solp_end = soil1(j)%mp(bz_lyr)%lab  

	!! daily change in live bacteria biomass(kg/ha) Eq. 4-1 
	! kg/ha = m^3 * mg/L/(1000.*ha)6
      rbiom(j) = ctmp*sep(isep)%bod_conv*(qin*                          &                          
           sepdb(sep(isep)%typ)%bodconcs -                              &                                
           qout * bode) / (1000. * bza) - (rrsp + rmort + rslg)         
      rbiom(j) = max(1.e-06,rbiom(j))

	!! total live biomass in biozone(kg/ha)    
	biom(j) = biom(j) + rbiom(j)
	       
	!! print out time series results 
      if (time%yrs > pco%nyskip) then
         n1=nh3_init 
         n2=nh3_begin
         n3=nh3_end
         n4=no3_init
         n5=no3_begin
         n6=no3_end !*bza/hvol*1000
         n7=rnit
         n8=rdenit
         p1=solp_init
         p2=solp_begin
         p3=solp_end
         p4 = solpconc

	write(173,1000) ihru,time%yrc,iida,precipday,qout,             &
        soil(j)%phys(bz_lyr)%ul, soil(j)%phys(bz_lyr)%st,          &
        soil(j)%phys(bz_lyr)%fc,n1,n2,n3,n4,n5,n6,n7,n8,p1,p2,p3,p4
	endif 	
       
1000  format(3i5,50es15.4)
      return
      end subroutine sep_biozone