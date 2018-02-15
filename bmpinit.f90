      subroutine bmpinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine sets default values for urban bmp parameters

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_onoff(:)   |none          |sub-basin detention pond is associated with
!!    dtp_iy(:)      |none          |year of the simulation that the reservoir 
!!                                  |becomes operational
!!    dtp_imo(:)     |none          |month the reservoir becomes operational
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_parm(:)    |none          |BMP outflow hydrograph shape parameter
!!    dtp_totwrwid(:)|m             |Total constructed width of the detention wall across
!!                                  |the creek
!!    dtp_stagdis(:) |none          |0=use weir/orifice discharge equation to calculate 
!!                                  |outflow, 1=use stage-dicharge relationship
!!    dtp_reltype(:) |none          |Equations for Stage-Discharge relationship,1=exponential 
!!                                  |function, 2=linear, 3=logarithmic, 4=cubic, 5=power   
!!    dtp_intcept(:) |none          |Intercept used in regression equations
!!    dtp_expont(:)  |none          |Exponent used in the exponential equation
!!    dtp_coef1(:)   |none          |Coefficient of 3rd degree in the polynomial equation
!!    dtp_coef2(:)   |none          |Coefficient of 2nd degree in the polynomial equation
!!    dtp_coef3(:)   |none          |Coefficient of 1st degree in the polynomial equation
!!    dtp_weirtype(:,:)|none        |Type of weir: 1=rectangular and 2=circular
!!    dtp_weirdim(:,:)|none         |Weir dimensions, 1=read user input, 0=use model calculation
!!    dtp_wdratio(:,:)|none         |Width depth ratio of rectangular weirs
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of orifice hole at different stages
!!    dtp_addon(:,:)  |m            |The distance between spillway levels
!!    dtp_flowrate(:,:)|m3/sec      |Maximum discharge from each stage of the weir/hole
!!    dtp_cdis(:,:)  |none          |Discharge coeffieicne for weir/orifice flow
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    res_out(:,:,:) |m**3/day      |measured average daily outflow from the 
!!                                  |reservoir for the month (needed if IRESCO=1)
!!                                  |(read in as m**3/s and converted to m**3/day)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    k           |none          |counter
!!    lnvol       |none          |variable to hold denominator value
!!    mon         |none          |counter
!!    titldum     |NA            |title line in .det file (not used in program)
!!    resmto      |NA            |name of reservoir outflow file
!!                               |(needed if IDETCO = 2)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use hydrograph_module
      use time_module
      use climate_parms
      
      implicit none
      integer :: k, eof,kk, jj
      real :: hwq,wqv,sub_ha,bmpfr_sf,bmpfr_ri

      eof = 0; bmpfr_sf=0.; bmpfr_ri=0.

      !! Detention pond
      !!----------------
      if (dtp_onoff(jj)==1) then
         if (dtp_imo(jj)<=0)      dtp_imo(jj) = 1
         if (dtp_iyr(jj)<=1000)   dtp_iyr(jj) = time%yrc
   	   if (dtp_evrsv(jj)<=0)    dtp_evrsv(jj) = 0.1
	   if (dtp_numweir(jj)<=0)  dtp_numweir(jj) = 1
	   if (dtp_numstage(jj)<=0) dtp_numstage(jj) = 1
	   if (dtp_numstage(jj)>1) then
	      do k=2,dtp_numstage(jj)
	          if (dtp_weirtype(jj,k)==1) dtp_addon(jj,k) = 0.
	      end do
	   endif

         !!	Estimating design flow rate if not entered by user
         do k=1,dtp_numstage(jj)
            if (dtp_flowrate(jj,k)<=0.0) then 
                dtp_flowrate(jj,k) = 0.5 * 1000.0 * dtp_pcpret(jj,k)       &
                  * subdr_km(jj) / (time%dtm * 60.)
	      end if

	      if (dtp_flowrate(jj,k)<0) then
	         print *,"Error.. Could not estimate emergency spillway volume"
               print *,"Please enter necessary data in *.pnd input file"
               print *,"for subbasin : ",jj
   !!            stop
	      end if
         end do

         !!	Separate cumulative flow information to individual weir 
         do k=2,dtp_numstage(jj)
            dtp_flowrate(jj,k) = dtp_flowrate(jj,k) / dtp_numweir(jj)
         end do
      	
         !!Estimate weir dimensions based on existing data 
         do k=1,dtp_numstage(jj)
            if (dtp_weirdim(jj,k)==0) then  !! Estimating weir dimensions
               if (dtp_weirtype(jj,k)==2) then	!! choosing weir type
                  call est_orfdim(dtp_flowrate(jj,k),dtp_diaweir(jj,k)      &
                       ,dtp_cdis(jj,k))
               else
                  call est_weirdim(dtp_wdratio(jj,k),dtp_flowrate(jj,k)     &
                       ,dtp_wrwid(jj,k),dtp_depweir(jj,k),dtp_cdis(jj,k))
               end if 
            else  !! read user-entered data
               if (dtp_weirtype(jj,k)==1) then  
                  dtp_wrwid(jj,k) = dtp_wdratio(jj,k) * dtp_depweir(jj,k)
               end if  
            end if
         end do
        
         !! divide rectangular weirs into multiple single stage weirs
         do k = 2, dtp_numstage(jj)
            dtp_addon(jj,k) = dtp_addon(jj,k-1) + dtp_depweir(jj,k-1) 
         end do
         
         do k = dtp_numstage(jj), 2, -1
            dtp_wrwid(jj,k) = dtp_wrwid(jj,k) - dtp_wrwid(jj,k-1)
            dtp_depweir(jj,k-1) = dtp_depweir(jj,k) + dtp_depweir(jj,k-1)
         end do
      end if
 
      !! Wet pond
      !!----------
      if (wtp_onoff(jj)==1) then
         if (wtp_imo(jj)<=0)      wtp_imo(jj) = 1
         if (wtp_iyr(jj)<=1000)   wtp_iyr(jj) = time%yrc
         if (wtp_k(jj) <=0)       wtp_k(jj) = 0.01
         if (wtp_evrsv(jj)<=0)    wtp_evrsv(jj) = 0.01
         if (wtp_evrsv(jj)>1)     wtp_evrsv(jj) = 0.99
         if (wtp_sdslope(jj)<3)   wtp_sdslope(jj) = 3. !COA Manual 1.6.6.C
         if (wtp_lenwdth(jj)<2)   wtp_lenwdth(jj) = 2. !COA Manual 1.6.6.C
         if (wtp_pdia(jj)<=0)     wtp_pdia(jj) = 0.1524 !meter(=6inches), COA Manual 1.6.6 
         if (wtp_plen(jj)<=2)     wtp_plen(jj) = 2 !meter
         if (wtp_pmann(jj)<=0)    wtp_pmann(jj) = 0.012 ! concrete surface
         if (wtp_ploss(jj)<=0)    wtp_ploss(jj) = 0
      end if
      
      !fraction runoff that directly enters the channel
      if(num_sf(jj)>=1) then
         do kk=1,num_sf(jj)
             bmpfr_sf = bmpfr_sf + sf_fr(jj,kk)
         end do
      endif
      if(num_ri(jj)>=1) then
         do kk=1,num_ri(jj)
             bmpfr_ri = bmpfr_ri + ri_fr(jj,kk)
         end do
      endif
      if (bmpfr_sf>1.or.bmpfr_ri>1) then 
         write (*,*) " "
         write (*,*) "Urban BMP Warning!!"
         write (*,*) "In subbasin ", jj
         write (*,*) "The fraction runoff draining to urban BMPs"
         write (*,*) " are larger than one, so the fraction values"
         write (*,*) " were automatically reassigned"
         write (*,*) " "
         do kk=1,num_sf(jj)
             sf_fr(jj,kk) = sf_fr(jj,kk) / bmpfr_sf
         end do
          do kk=1,num_sf(jj)
             ri_fr(jj,kk) = ri_fr(jj,kk) / bmpfr_ri
         end do
         bmpfr_sf = 1.; bmpfr_ri=1.
      endif
         
      !!Retention-Irrigation
      !!---------------------
      do k=1,num_ri(jj)
         ! skip the pond that has zero inflow
         if (ri_fr(jj,k)==0) cycle

         ! determine water quality volume for defult pond sizes
         !City of Austin Design Guideline 1.6.9.2 Table 1-12
         !Retention-Irrigation for Barton Springs Zone

         hwq = (1.8 * sub_ha_imp(jj) / sub_ha_urb(jj) + 0.6) !inches
         wqv = hwq / 12. * sub_ha_urb(jj) * ri_fr(jj,k) * 107639.104167 !ft3
                  
         if (ri_dim(jj,k)==0) then
           !Determine pond size automatically based on City of Austin's Design Guideline 1.6
            ri_vol(jj,k) = wqv * 0.028317 !m3
		      ri_dep(jj,k)=1.5 !assume 1.5m as default retention pond depth
            ri_sa(jj,k) = ri_vol(jj,k) / ri_dep(jj,k) 
            ri_dd (jj,k)=60.0 !drawdown time, hr
            ri_k(jj,k)=2.5
            ri_evrsv(jj,k)=0.6
         else
            !Evaluate unit variables provided by user
            if (ri_sa(jj,k)<1.or.ri_vol(jj,k)<1) then
               !City of Austin Design Guideline 1.6.5
               ri_vol(jj,k) = wqv * 0.028317 !m3
               ri_sa(jj,k) = ri_vol(jj,k) / ri_dep(jj,k) 
            end if

		   ri_dep(jj,k) = ri_vol(jj,k) / ri_sa(jj,k)
		   if (ri_dd(jj,k)<=0) ri_dd (jj,k)=72.0
		   if (ri_k(jj,k) <=0 ) ri_k(jj,k)=2.5
		   if (ri_evrsv(jj,k) <=0 ) ri_evrsv(jj,k)=0.1
		   if (ri_dep(jj,k)<=1) ri_dep(jj,k)=1. 
         end if
         
	   ! draw down time [number of time step]
	   ri_ndt(jj,k) = (ri_dd(jj,k) - 12.) * 60 / time%dtm  !minus the first dry 12 hours no-pumping 
	      
	   ! pumping rate that empties the basin in 72 hours with initial 12 hour of no operatrion
	   ri_pumpv(jj,k) = ri_vol(jj,k) / ri_ndt(jj,k) !m3/dt
	      
         if (ri_im(jj,k)<0.or.ri_im(jj,k)>12) ri_im(jj,k) = 0
         if (ri_iy(jj,k)<1000.and.ri_iy(jj,k)>0) ri_iy(jj,k) = 0
         if (ri_iy(jj,k)==0) ri_iy(jj,k) = time%yrc
         if (ri_im(jj,k)==0) ri_im(jj,k) = 1   

       write(77779,'(a11,i5)') 'Subbasin #:', jj   ! bmp_sedfil.out
       write(77779,'(a46)') '' 
       write(77779,'(a10,i5)') 'RI #:', K   ! bmp_sedfil.out
       write(77779,'(a17,f10.1,a4)') 'Total volume =', ri_vol(jj,k),'m^3'
       write(77779,'(a17,f10.1,a4)') 'Surface area =', ri_sa(jj,k),'m^2'
       write(77779,'(a17,f10.1,a3)') 'Drawdown time =', ri_dd (jj,k),'hr'
       write(77779,'(a17)') ''

      end do
  
      !!Sedimentation-Filtration
      !!---------------------
      
      do k=1,num_sf(jj)
         write(77778,'(a11,i5)') 'Subbasin #:', jj   ! bmp_sedfil.out
         write(77778,'(a46)') '' 
         write(77778,'(a10,i5)') 'SED-FIL #:', K   ! bmp_sedfil.out
         !determine water quality volume for defult pond sizes
         !City of Austin Design Guideline 1.6.2
         hwq = (0.5 + sub_ha_imp(jj) / sub_ha_urb(jj) - 0.2) !inches
         wqv = hwq / 12. * sub_ha_urb(jj) * sf_fr(jj,k) * 107639.104167 !ft3
                  
         if (sf_dim(jj,k)==0) then
            write(77778,'(a46)') 'This SED-FIL size is automatically'      &
              // ' estimated based on WQV.' 
            !Determine pond size automatically based on City of Austin's Design Guideline 1.6
            if (sf_typ(jj,k)==1.or.sf_typ(jj,k)==3) then
               ! full scale or sedimentation basin only
               sp_pvol(jj,k) = wqv * 0.028317 !m3
               sp_sa(jj,k) = sp_pvol(jj,k) / 1.5 !assume 1.5m depth for sed pond
               sp_pd(jj,k) = sqrt(4. * 2. * sp_sa(jj,k) * 1.5**0.5           &    
                      / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
               ft_sa(jj,k) = wqv/(7.+2.33*4.) * 0.093 !m2
               ft_fsa(jj,k) = 1. 
              
            else
               ! partial scale
               ft_sa(jj,k) = wqv * 0.028317 !m3
!               wqv/(4.+1.33*4.) * 0.093
               
            end if
            sp_bpw(jj,k) = 10. !m overflow weir width
            ft_pd(jj,k) = 1524. !mm
            ft_dep(jj,k) = 420. !mm
            ft_h(jj,k) = 1200. !mm
        
         else
         !Evaluate unit variables given by users
            if (sf_typ(jj,k)>3) sf_typ(jj,k) = 1
            if (sp_sa(jj,k)<1) then
               !City of Austin Design Guideline 1.6.5
               sp_pvol(jj,k) = wqv * 0.028317 !m3
               sp_sa(jj,k) = sp_pvol(jj,k) / 1.5 !assume 1.5m depth for sed pond
            end if
            if (sp_pd(jj,k)<0.1) sp_pd(jj,k) = 152.4 !mm diameter, equivalent to 6in
            if (ft_h(jj,k)<5) ft_h(jj,k) = 1200. !mm 
            if (ft_sa(jj,k)<1) then
               if (sf_typ(jj,k)==1) then
                  ft_sa(jj,k) = wqv/(7.+2.33*ft_h(jj,k)/304.8) * 0.093 !m2
                  ft_fsa(jj,k) = 1.
               else
                  ft_sa(jj,k) = wqv * 0.028317 !m3
                  ft_fsa(jj,k) = 1. /(4.+1.33*ft_h(jj,k)/304.8) 
               end if
            end if
         end if
         
         !Outflow control
         if (sp_qfg(jj,k)==0) then
            sp_pd(jj,k) = sqrt(4. * 2. * sp_sa(jj,k) * 1.5**0.5            &         
                / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
         end if
         if (ft_qfg(jj,k)==0) then
            ft_pd(jj,k) = sqrt(4. * 2. * ft_sa(jj,k) * 1.5**0.5            &          
                / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
         end if
         
         !Orifice pipe for sand filter should be equal or larger than 
         !sedimentation pond outlet pipe for full-type SedFils
         if (ft_pd(jj,k)<sp_pd(jj,k)) ft_pd(jj,k) = sp_pd(jj,k)
         
         if (ft_dep(jj,k)<100) ft_dep(jj,k) = 100.
         if (sf_ptp(jj,k)>1) sf_ptp(jj,k) = 1
         if (sf_typ(jj,k)==1) sf_ptp(jj,k) = 0 !no filter outflow control for partial systems
         if (sp_pd(jj,k)>254) sp_pd(jj,k) = 254. ! max 10inches dia
         if (sp_pd(jj,k)<10) sp_pd(jj,k) = 10. ! min 10mm dia
         if (ft_pd(jj,k)>254) ft_pd(jj,k) = 254. ! max 10inches
         if (ft_k(jj,k)<1) ft_k(jj,k) = 1.
         if (ft_dp(jj,k)<0.0001) ft_dp(jj,k) = 0.02
         if (ft_dc(jj,k)<0.01) ft_dc(jj,k) = 0.762
         if (ft_por(jj,k)<0.1) ft_por(jj,k) = 0.45
         if (tss_den(jj,k)<0.5) tss_den(jj,k) = 0.5
         if (ft_alp(jj,k)<0.1) ft_alp(jj,k) = 0.1
         if (sf_im(jj,k)<0.or.sf_im(jj,k)>12) sf_im(jj,k) = 0
         if (sf_iy(jj,k)<1000.and.sf_iy(jj,k)>0) sf_iy(jj,k) = 0
         if (sf_iy(jj,k)==0) sf_iy(jj,k) = time%yrc
         if (sf_im(jj,k)==0) sf_im(jj,k) = 1  
         if (ft_fsa(jj,k)==0) ft_fsa(jj,k) = 0.85 
         if (sf_typ(jj,k)==1) ft_fsa(jj,k) = 1 
         
 
         if (sf_typ(jj,k)==1) then
           write(77778,'(a37)') 'Full type sed-fil selected'
         elseif (sf_typ(jj,k)==2) then
           write(77778,'(a40)') 'Partial type sed-fil selected'
         else
           write(77778,'(a43)') 'Sedimentation pond only selected'
         endif
         if (sf_typ(jj,k)==1.or.sf_typ(jj,k)==3) then     
      write(77778,'(a18)') 'Sedimentation pond'
      write(77778,'(a17,f10.1,a4)') 'Total volume =', sp_pvol(jj,k),'m^3'
      write(77778,'(a17,f10.1,a4)') 'Surface area =', sp_sa(jj,k),'m^2'
      write(77778,'(a17,f10.1,a3)') 'Drain Pipe Dia =', sp_pd(jj,k),'mm'
      write(77778,'(a17)') ''
         endif
         if (sf_typ(jj,k)==1.or.sf_typ(jj,k)==2) then     
      write(77778,'(a11)') 'Sand Filter'
      write(77778,'(a17,f10.1,a4)') 'Surface area =', ft_sa(jj,k),'m^2'
      write(77778,'(a17,f10.1,a3)') 'Max ponding =', ft_h(jj,k),'mm'
      write(77778,'(a17,f10.1,a3)') 'Filter depth =', ft_dep(jj,k),'mm'
      write(77778,'(a17,f10.1,a3)') 'Drain Pipe Dia =', ft_pd(jj,k),'mm'
      write(77778,'(a17)') ''
        end if
        end do 
    
      return
      end subroutine bmpinit