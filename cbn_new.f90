      subroutine cbn_new
      !! This code simulates organic C, N, and P cycling in the soil
      !! The code was developed by Armen R. Kemanian and Stefan Julich
      !! It has been adapted from Kemanian and Stockle (2010) (European Journal of Agronomy 32:22-29)
      !! and crafted to accomodate to SWAT conventions
      !! Plant residues and manure residues are decomposed separately
      !! For convenience, the denitrification subroutine is called from here
      !! March 2009: testing has been minimal and further adjustments are expected
      !! manuscript describing this subroutine to be submitted to Ecological Modelling (September, 2010)
      !! use with caution and report anomalous results to akemanian@psu.edu, and jeff.arnold@ars.usda.edu, stefan.julich@tudor.lu
      
    
!!!!!!!
      !! local variables
      !! cx = saturated soil carbon concentration (%) (Hassink and Whitmore, 1997)
      !! decf = decomposition factor  
      !! net_N = nitrogen net mineralization
      !! net_P = phosphorus net mineralization
      !! rnet_N, rnet_P, mnet_N, mnet_P for residue and manure respectively
      !! sol_cdec = carbon decomposition (CO2)
      !! tilf = tillage factor
      !! resc_hum = humified carbon residue
      !! manc_hum = humified carbon manure

      use parm
      use nutrient_module
    
    !! private variables
      real :: cx, decf, rhc, mhc, sol_cdec, tilf
      real :: resc_hum, manc_hum
      real :: xx, xx1, xx2, xx3, xx4, csf
      real :: rdc, mdc, wdn, cdg, sut
      real :: CNsoil, CPsoil, NPsoil
      real :: CNres, CPres, CNman, CPman, rCNnew, mCNnew
      real :: sol_thick, sol_mass, sol_cmass, sol_nmass
      real :: net_N, net_P, rnet_N, rnet_P, mnet_N, mnet_P
      real :: wc, fc, wf, of, void

    !! mass balance variables
      real :: sum_c_i, sum_n_i, sum_p_i
      real :: sum_c_f, sum_n_f, sum_p_f
      real :: bal_c, bal_n, bal_p

    !! output variables for soil profile
      real :: cmass_pro, nmass_pro, sol_orgp_pro
      real :: sol_rsd_pro, sol_fon_pro, sol_fop_pro
      real :: sol_mc_pro, sol_mn_pro, sol_mp_pro
      real :: sol_no3_pro, sol_solp_pro, sol_nh3_pro
      real :: sol_cdec_pro, wdn_pro, net_N_pro, solN_net_min
      real :: solN_net_min_pro 

      integer :: j, k, kk

    !! functions
      real ::fwf, fof, fcdg, ftilf,fcx, fCNnew, fhc, fnetmin


      j = 0; wdn = 0
      j = ihru
  
    !! initialize
      cmass_pro = 0.
      nmass_pro = 0.
      sol_orgp_pro = 0.
      sol_rsd_pro = 0.
      sol_fon_pro = 0.
      sol_fop_pro = 0.
      sol_mc_pro = 0.
      sol_mn_pro = 0.
      sol_mp_pro = 0.
      sol_no3_pro = 0.
      sol_nh3_pro = 0.
      sol_solp_pro = 0. 
      sol_cdec_pro = 0.
      wdn_pro = 0.
      net_N_pro = 0.
      solN_net_min = 0.
      solN_net_min_pro=0.
!!    zero new carbon variables for output.hru
!!      cmup_kgh = 0.                               !! jga
!!      cmtot_kgh = 0.                              !! jga
!!    zero new carbon variables for output.hru


      if (soil(j)%cbn(1)%cbn == 0.) return	
 
      do k = 1, soil(j)%nly

      sum_c_i = 0.
      sum_n_i = 0.
      sum_p_i = 0.    

      rdc = 0.
      mdc = 0.
      rhc  = 0.
      mhc = 0.
      sol_cdec = 0.    
      resc_hum = 0.
      manc_hum = 0.
      rnet_N = 0.
      rnet_P = 0.
      mnet_N = 0.
      mnet_P = 0.
      net_N = 0.
      net_P = 0.

      rCNnew = 0.
      mCNnew = 0.

      wc = 0.
      fc = 0.
      wf = 0.
      of = 0.
      void = 0.
      cdg = 0.
      sut = 0.
      csf = 0.

      ffres = 0.
      ffres1 = 0.
      ffres2 = 0.

      ffman = 0.
      ffman1 = 0.
      ffman2 = 0.

	if (k == 1) then
	    sol_thick = soil(j)%phys(k)%d
	else	
	    sol_thick = soil(j)%phys(k)%d - soil(j)%phys(k-1)%d
	end if

      !! soil carbon and nitrogen mass
      sol_mass = (sol_thick / 1000.) * 10000. * soil(j)%phys(k)%bd      &
            * 1000. * (1- soil(j)%phys(k)%rock  / 100.)
      sol_cmass = sol_mass * (soil(j)%cbn(k)%cbn / 100.) 
      sol_nmass = sol_mass * (soil(j)%ly(k)%n / 100.) 

	!! sum initial C,N,P for mass balance
	sum_c_i = sol_cmass + 0.43 * soil(j)%ly(k)%rsd + soil(j)%cbn(k)%mc
	sum_n_i = sol_nmass + soil(j)%nut(k)%no3+soil(j)%nut(k)%nh3 +     &
             soil(j)%nut(k)%fon + soil(j)%nut(k)%mn
	sum_p_i = soil(j)%nut(k)%orgp + soil(j)%nut(k)%solp+soil(j)%nut(k)%fop    &
             + soil(j)%nut(k)%mp

      kk = k 
      if (k == 1) kk = 2

	if (soil(j)%phys(kk)%tmp > 0. .and. soil(j)%phys(kk)%st > 0.) then
		!! microbial processes if temp > 0 C
		!! microbial processes if water > pwp
	
		!!compute soil water factor - sut
		fc = soil(j)%phys(kk)%fc + soil(j)%phys(kk)%wpmm  ! units mm
		wc = soil(j)%phys(kk)%st + soil(j)%phys(kk)%wpmm  ! units mm
		sat = soil(j)%phys(kk)%ul + soil(j)%phys(kk)%wpmm ! units mm
		void = soil(j)%phys(kk)%por * (1. - wc / sat)	

        wf = fwf(fc,wc,soil(j)%phys(kk)%wpmm)
        of = fof(void,soil(j)%phys(kk)%por)
        sut = wf * of

        !! compute soil temperature factor - cdg
        cdg = fcgd(soil(j)%phys(kk)%tmp)

		!! compute combined factor
		xx = 0.
!!		xx = sqrt(cdg * sut)
		xx = (cdg * sut) ** cf(j)
		if (xx < 0.) xx = 0.
		if (xx > 1.) xx = 1.
		csf = xx
	
		!! call denitrification (to use void and cdg factor)
		wdn = 0.
		if (cdg > 0. .and. void <= 0.1) then
			call nut_denit(k,j,cdg,wdn,void)
		end if
		wdntl = wdntl + wdn

		!! calculate soil carbon 'decomposition'
		!! tillage factor
		tilf = ftilf(soil(j)%ly(k)%tillagef, wc, sat)	
		!! saturated carbon concentration (%)
		cx = fcx(soil(j)%phys(k)%clay)	
		!! soil carbon decomposition
		sol_cdec=fsol_cdec(soil(j)%cbn(k)%cbn,cx,cfdec(j),            &
      		tilf,csf,sol_cmass)

        !! residue and manure decomposition and N and P mineralization    
        CNsoil = soil(j)%cbn(k)%cbn / soil(j)%ly(k)%n 
        NPsoil = (sol_mass*soil(j)%ly(k)%n / 100.) / soil(j)%nut(k)%orgp
        CPsoil = CNsoil * NPsoil
        
    
        if (soil(j)%ly(k)%rsd > 0.00001) then

            !! residue decomposition
            rdc = 0.05 * csf * soil(j)%ly(k)%rsd

			!! humification factor
			rhc = fhc(soil(j)%phys(k)%clay,soil(j)%cbn(k)%cbn,cx) * cfh(j)
            
            CNres = 0.43 * soil(j)%ly(k)%rsd / soil(j)%nut(k)%fon
            CPres = 0.43 * soil(j)%ly(k)%rsd / soil(j)%nut(k)%fop
            
            !! CN of new organic matter (humified residue)
            rCNnew = fCNnew(soil(j)%nut(k)%no3,sol_mass,CNres, 110.)

			!! N mineralization / immobilization			
			xx1 = soil(j)%ly(k)%rsd
			xx2 = soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3
			rnet_N = fnetmin(rdc,CNres,rCNnew,rhc,ffres1,xx1,xx2,0.43)

            !! P mineralization / immobilization
            xx2 = soil(j)%nut(k)%solp
            rnet_P = fnetmin(rdc,CPres,CPsoil,rhc,ffres2,xx1,xx2,0.43)
  
            !! check if P limits N mineralization and re-adjust
            if (ffres2 < ffres1) then
                  rnet_N = rdc * 0.43 * (1. / CNres - rhc / rCNnew)
                ffres = ffres2
            else
                ffres = ffres1
            end if
        end if

        ! manure decomposition and N and P mineralization
        if (soil(j)%cbn(k)%mc > 0.00001) then

            !! residue decomposition
            !! decomposition rate about 1/2 of residue
            mdc = 0.025 * csf * soil(j)%cbn(k)%mc

            !! humification factor
			mhc = 1.6*fhc(soil(j)%phys(k)%clay,soil(j)%cbn(k)%cbn,cx)
			
			CNman = soil(j)%cbn(k)%mc / soil(j)%nut(k)%mn
			CPman = soil(j)%cbn(k)%mc / soil(j)%nut(k)%mp
			
			!! CN of new organic matter (humified manure)
			mCNnew = fCNnew(soil(j)%nut(k)%no3,sol_mass,CNman, 55.)

			!! N mineralization / immobilization			
			xx1 = soil(j)%cbn(k)%mc
			xx2 = soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3
			mnet_N = fnetmin(mdc,CNman,mCNnew,mhc,ffman1,xx1,xx2,1.0)

            !! P mineralization / immobilization
            xx2 = soil(j)%nut(k)%solp
            mnet_P = fnetmin(mdc,CPman,CPsoil,mhc,ffman2,xx1,xx2,1.0)
  
            !! check if P or N limit mineralization and re-adjust
            if (ffman2 < ffman1) then
                  mnet_N = mdc * (1. / CNman - mhc / mCNnew)
                ffman = ffman1
            else
                ffman = ffman2
            end if
        end if

        !! check if sufficient mineral N for both residue and manure decomposition
		if ((soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3) > 0.) then
			xx = (rnet_N + mnet_N) / (soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3)
            if (xx < -1.) then
                rdc = -rdc / xx
                rnet_N = -rnet_N / xx
                rnet_P = -rnet_P / xx
                ffres = -ffres / xx

                mdc = -mdc / xx
                mnet_N = -mnet_N / xx
                mnet_P = -mnet_P / xx
                ffman = -ffman / xx
            end if
        end if
        
        !! check if sufficient mineral P for both residue and manure decomposition
        if (soil(j)%nut(k)%solp > 0.) then
            xx = (rnet_P + mnet_P) / soil(j)%nut(k)%solp
            if (xx < -1.) then
                rdc = -rdc / xx
                rnet_N = -rnet_N / xx
                rnet_P = -rnet_P / xx
                ffres = -ffres / xx

                mdc = -mdc / xx
                mnet_N = -mnet_N / xx
                mnet_P = -mnet_P / xx
                ffman = -ffman / xx
            end if
        end if

        resc_hum = rhc * rdc * 0.43
        manc_hum = mhc * mdc
        net_N = rnet_N + mnet_N
        net_P = rnet_P + mnet_P
        if (resc_hum == 0.) rCNnew = 1000.
        if (manc_hum == 0.) mCNnew = 1000.

		!! C N P pools update
		sol_cmass = sol_cmass + resc_hum + manc_hum - sol_cdec
 		soil(j)%cbn(k)%cbn = 100. * sol_cmass / sol_mass
		
		sol_nmass = sol_nmass - sol_cdec / CNsoil                     &
            	  + resc_hum / rCNnew + manc_hum / mCNnew

!! april 2010 output net_min
		solN_net_min=-(- sol_cdec / CNsoil                            &
                  + resc_hum / rCNnew + manc_hum / mCNnew)
 		soil(j)%ly(k)%n  = 100. * sol_nmass / sol_mass
		soil(j)%nut(k)%orgn = sol_nmass ! for output
        soil(j)%nut(k)%orgp = soil(j)%nut(k)%orgp - sol_cdec / CPsoil  &
              + (resc_hum + manc_hum) / CPsoil

                if (ffres > 1.) ffres = 1.

        soil(j)%ly(k)%rsd = soil(j)%ly(k)%rsd * (1. - ffres)
        soil(j)%nut(k)%fon = soil(j)%nut(k)%fon * (1. - ffres)
        soil(j)%nut(k)%fop = soil(j)%nut(k)%fop * (1. - ffres)
     
                if (ffman > 1.) ffman = 1.

        soil(j)%cbn(k)%mc = soil(j)%cbn(k)%mc * (1. - ffman)
        soil(j)%nut(k)%mn = soil(j)%nut(k)%mn * (1. - ffman)
        soil(j)%nut(k)%mp = soil(j)%nut(k)%mp * (1. - ffman)
		
		! add positive n-mineralization to ammonia pool in the layer
		if (rnet_N>0.) soil(j)%nut(k)%nh3 = soil(j)%nut(k)%nh3 + rnet_N 
		if (mnet_N>0.) soil(j)%nut(k)%nh3 = soil(j)%nut(k)%nh3 + mnet_N

		if (rnet_N<0.) then
			if (abs(rnet_N) < soil(j)%nut(k)%nh3) then
				soil(j)%nut(k)%nh3 = soil(j)%nut(k)%nh3 + rnet_N
			else
				xx4 = soil(j)%nut(k)%nh3 + rnet_N
				soil(j)%nut(k)%nh3 = 0.
				soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 + xx4
			end if
		end if
	
		if (mnet_N<0.) then
			if (abs(mnet_N) < soil(j)%nut(k)%nh3) then
				soil(j)%nut(k)%nh3 = soil(j)%nut(k)%nh3 + mnet_N
			else
				xx4 = soil(j)%nut(k)%nh3 + mnet_N
				soil(j)%nut(k)%nh3 = 0.
				soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 + xx4
			end if
		end if
	
		soil(j)%nut(k)%nh3 = soil(j)%nut(k)%nh3 + sol_cdec * (1. / CNsoil)
		
		soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + net_P +              &
       		sol_cdec * (1. / CPsoil)
	 
		If (soil(j)%ly(k)%rsd < 1e-10) soil(j)%ly(k)%rsd = 1e-10
		If (soil(j)%nut(k)%fon < 1e-11) soil(j)%nut(k)%fon = 1e-11
		If (soil(j)%nut(k)%fop < 1e-12) soil(j)%nut(k)%fop = 1e-12
		If (soil(j)%cbn(k)%mc < 1e-10) soil(j)%cbn(k)%mc = 1e-10
		If (soil(j)%nut(k)%mn < 1e-11) soil(j)%nut(k)%mn = 1e-11
		If (soil(j)%nut(k)%mp < 1e-12) soil(j)%nut(k)%mp = 1e-12
		If (soil(j)%nut(k)%no3 < 1e-12) soil(j)%nut(k)%no3 = 1e-12

	end if

	!! balance file cswat_balance
	sum_c_f = sol_cmass + 0.43 * soil(j)%ly(k)%rsd + sol_cdec            &
        	+ (1. - rhc) * rdc * 0.43 + soil(j)%cbn(k)%mc + (1. - mhc) * mdc
	sum_n_f = sol_nmass + soil(j)%nut(k)%no3 + soil(j)%nut(k)%nh3 +      &
          soil(j)%nut(k)%fon + soil(j)%nut(k)%mn + wdn
	sum_p_f=soil(j)%nut(k)%orgp + soil(j)%nut(k)%orgp+soil(j)%nut(k)%fop &
        	+ soil(j)%nut(k)%mp
		  
	bal_c = sum_c_i - sum_c_f
	bal_n = sum_n_i - sum_n_f
	bal_p = sum_p_i - sum_p_f

!!    carbon outputs for .hru file
      if (k == 1) soil(j)%cmup_kgh = sol_cmass
      soil(j)%cmtot_kgh = soil(j)%cmtot_kgh + sol_cmass
!!    carbon outputs for .hru file


	cmass_pro = cmass_pro + sol_cmass
	nmass_pro = nmass_pro + sol_nmass
	sol_rsd_pro = sol_rsd_pro + soil(j)%ly(k)%rsd
	sol_cdec_pro = sol_cdec_pro + sol_cdec
	sol_fon_pro = sol_fon_pro + soil(j)%nut(k)%fon
	sol_no3_pro = sol_no3_pro + soil(j)%nut(k)%no3
	sol_nh3_pro = sol_nh3_pro + soil(j)%nut(k)%nh3
	sol_orgp_pro = sol_orgp_pro + soil(j)%nut(k)%orgp
	sol_fop_pro = sol_fop_pro + soil(j)%nut(k)%fop
	sol_solp_pro = sol_solp_pro + soil(j)%nut(k)%solp
	sol_mc_pro = sol_mc_pro + soil(j)%cbn(k)%mc
	sol_mn_pro = sol_mn_pro + soil(j)%nut(k)%mn
	sol_mp_pro = sol_mp_pro + soil(j)%nut(k)%mp
	wdn_pro = wdn_pro + wdn
	net_N_pro = net_N_pro + net_N
	solN_net_min_pro = solN_net_min_pro + solN_net_min

      end do
	
	!! writing daily profile output
      !!if (i==365) then
        write (100,9002) time%yrc,i,j,cmass_pro,sol_rsd_pro,sol_mc_pro  


      !!end if

!9000  format(i4,';',i3,';',i1,';',i4,20(';',f10.3))
!9001  format(i4,';',i3,';',i1,';',i4,10(';',f10.3))
9002  format(i4,';',i3,';',i4,';',f11.3,';',f11.3,';',f11.3)

      return
      end subroutine cbn_new

	!! LOCAL FUNCTIONS
	Function fwf(fc,wc,pwp)
		xx2 = 0.
		if (wc <= pwp) then
			xx2 = 0.4 * wc / pwp
		else if (wc <= fc) then
			xx2 = 0.4 + 0.6 * (wc - pwp)/(fc - pwp)
		else
			xx2 = 1.
		end if

        !!fwf = (1. + (1. - xx2) / (1. - 0.75)) * (xx2 / 1.) ** (1./ (1. - 0.75))    
        fwf = (1. + (1. - xx2) / 0.25) * (xx2) ** 4.
      End function

      Function fof(void,por)
        xx3 = 0.
        if (void >= 0.1) then
            xx3 = 0.2 + 0.8 * (void - 0.1) / (por - 0.1)
        else
            xx3 = 0.2 * void / 0.1
        end if
        fof = 0.5 + 0.5 * xx3 / (xx3 + Exp(-20. * xx3))
      End function

	
	Function fcgd(xx)
		tn = -5.
	  top = 35.
		tx = 50.
		qq = (tn - top)/(top - tx)
	  fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
		if (fcgd < 0.) fcgd = 0.
	End function

      Function ftilf(tillage, wc, sat)
        !! tillage factor effect on decomposition
        !! tillage factor returns to baseline (=1) based on WC 
        tillage = tillage * (1. - 0.02 * wc/sat) 
        if (tillage < 0.) tillage = 0.
        ftilf = 1. + tillage 
      End function
    

      Function fcx(pclay)
        !! saturated soil carbon concentration (%) from Hassink and Whitmore 1997
        fcx = 2.11 + 0.0375 * pclay
      End function


	Function fsol_cdec(pcarbon, cx, cfdec, tilf, csf, sol_cmass)
		!! decomposition adjustment by current SOC 
		decf = (pcarbon / cx) ** 0.5	
		! if (decf > 1.) decf = 1. 
		!! maximum soil carbon decomposition = 0.045 kg C / kg C per year
		fsol_cdec = cfdec / 365. * decf * tilf * csf * sol_cmass
	End function


      Function fCNnew(yy1,yy2,CNpool,yy5)
      !! CN ratio of newly formed organic matter
      !! based on CN or decomposing residue and nitrate in soil
      !! the same approach used for crop residues and manure
      !! CNpool = the CN of decomposing pool
      !! yy1 = the layer nitrate mass
      !! yy2 = the layer soil mass
      !! yy3 = nitrate concentration g/g
      !! yy4 = correction factor based on CN of decomposing pool
      !! yy5 = input-dependent constant to correct CN ratio

        yy3 = yy1 / yy2
        yy4 = 5.5 * (1. - 1. / (1. + (CNpool / yy5)**3.))
        fCNnew = 8.5 + yy4 * (0.5 + 0.5 / (1. + (yy3 / 8e-6)**3.)) 
      End function


	Function fhc(pclay, pcarbon, cx) 		 
	!! maximum and actual humification factor 
	!! hx = maximum humification factor
	!! hf = humification adjustment factor 
	!! pclay = %clay
	!! pcarbon = %carbon
	!! cx = saturated soil carbon, %
	
	real :: hx, hf, pclay, pcarbon
			 
		hx = 0.09 + 0.09 * (1. - Exp(-5.5 * pclay / 100.))
		!! humification adjustment by current SOC
		if (pcarbon > cx) then
			hf = 0.
		else
			hf = 1. - (pcarbon / cx) ** 6.
		end if
		fhc = hx * hf		  
	End function

	Function fnetmin(poold, R1, R2, hc, dummy, poolm, xinorg, cc1)
	!! This function computes net mineralization
	!! R1 = CN or CP ratio of decomposing pool
	!! R2 = CN or CP ratio of receiving pool
	!! hc = humification rate
	!! dummy = fraction of pool being decomposed
	!! poolm = current mass of pool being decomposed
	!! poold = mass of pool decomposed
	!! xinorg = mass of NO3 or P in solution
	!! xx = net mineralization of N or P
	!! cc1 = pool's carbon fraction

		xx = 0.
		xx = poold * cc1 * (1. / R1 - hc / R2) 
		
		if (xx > 0.) then
			dummy = poold / poolm
		else if (abs(xx)< xinorg) then 
			!! net mineralization is positive or
			!! immobilization not consuming all mineral N or P
			dummy = poold / poolm
		else
			!! immobilization, consuming all mineral N or P
			xx = -xinorg
			poold = xx / cc1 * 1. / (1. / R1 - hc / R2)
			dummy = poold / poolm
    		end if

        fnetmin = xx
      End function