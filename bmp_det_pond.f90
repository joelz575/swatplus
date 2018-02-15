      subroutine bmp_det_pond
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    the purpose of this program is to read in data from the detention pond
!!    input file (.dtp) and perform computations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_weirtype(:)|none          |Type of weir: 1 rectangular and 2 circular
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_wdratio(:,:)|none         |Width depth ratio (rectangular wier) at different stages
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of circular wier at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    dtp_cdis(:,:)  |none          |coeffieicne of discharge at different stages
!!    sub_subp_dt(:,:)  |mm H2O      |precipitation for time step in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |time step counter
!!    k           |none          |weir stage counter
!!    titldum     |NA            |dummy string
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT:  surf_seep,surf_evap,water_depth
!!    Intrinsic: Log,exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      use hydrograph_module
      use channel_module
      use climate_parms
      use jrw_datalib_module
      use time_module
      
      implicit none

      character (len=80) :: titldum
      integer :: ii, k, sb
      real :: qin,qout,qpnd,sedin,sedout,sedpnd,spndconc,qdepth,          &
              watdepact,qstage,backup_length,seep_sa,evap_sa,pcp_vol,     &
              evap_vol,seep_vol,warea,pi,qovmax,qaddon,depaddon
      
      pi = 3.14159
      sb = inum1
      qout = 0.; sedout = 0.; depaddon = 0.

      if (time%yrc<dtp_iyr(sb) .or. (time%yrc==dtp_iyr(sb) .and. time%mo <dtp_imo(sb))) then
         return
      endif
      
      !! Get initial values from previous day
      qpnd = dtp_ivol(sb) !m^3
      sedpnd = dtp_ised(sb) !tons

      ! Storage capacity under addon
      qaddon = (dtp_addon(sb,1) / dtp_parm(sb)) ** 3. / (3.* ch_hyd(sb)%s) * pi !m3

      !!	iterate for subdaily flow/sediment routing
      do ii=1,time%step
      
         qout = 0.; qovmax = 0; depaddon = 0.
         !qin = ob(icmd)%ts(1,ii)%flo + qpnd !m^3
         !sedin = ob(icmd)%ts(1,ii)%sed + sedpnd  !tons
         if (qin>1e-6) then
            spndconc = sedin / qin !initial sed conc, tons/m3
         else
            cycle
         end if

	   !! Estimate water depth
	   	 qdepth = dtp_parm(sb) * (3.*qin*ch_hyd(sb)%s/pi)**0.33333

	   !! skip to next time step if no ponding occurs 
	     if (qdepth<=0.0001) cycle   
         
         if (dtp_stagdis(sb)==0) then 
          !! Calculate weir outflow 
          do k = 1, dtp_numstage(sb)  
            qstage = 0.
            
            !! calculate weir discharge 
	        
	        if (dtp_weirtype(sb,k)==2) then
	        !! Circular weir
	           dtp_depweir(sb,k) = dtp_diaweir(sb,k) + dtp_addon(sb,k)
                                
	           if (qdepth>dtp_depweir(sb,k)) then  
               !! Fully submerged 
                  qdepth = qdepth - dtp_depweir(sb,k) 
                  watdepact = qdepth + dtp_diaweir(sb,k) / 2
  	              warea = 3.14159 * dtp_diaweir(sb,k) ** 2 / 4.
		        
		          !! orifice equation
   	              qstage = dtp_cdis(sb,k) * 0.6 * warea * sqrt(19.6 * watdepact) !m3/s/unit
                  qstage = qstage * dtp_numweir(sb) * 60. * time%dtm !m^3	         
               else
               !! Partially submerged
                  watdepact = Max(qdepth - dtp_addon(sb,k),0.)
                  dtp_wrwid(sb,k) = dtp_diaweir(sb,k) * 0.667
		           
		          !! weir/orifice discharge
		          qstage = dtp_cdis(sb,k) * 1.84 * dtp_wrwid(sb,k) * watdepact ** 1.5 !m3/s
		          qstage = qstage * dtp_numweir(sb) * 60. * time%dtm !m^3
               end if               
	         
	        else
	        !! Rectangular weir
	           watdepact = Max(qdepth - dtp_addon(sb,k),0.)

               !! Estimate weir/orifice discharge
		       qstage = dtp_cdis(sb,k) * 1.84 * dtp_wrwid(sb,k) * watdepact ** 1.5 !m3/s
		       qstage = qstage * dtp_numweir(sb) * 60. * time%dtm !m^3
	            
	        end if
            
            qout = qout + qstage
	      end do

	      !Limit total outflow amount less than available water above addon
	      if(qout>qin-qaddon) qout = Max(qin - qaddon,0.)
         
          !! Flow over the emergency weir
          watdepact = qdepth - (dtp_depweir(sb,1) + dtp_addon(sb,1))
          if (dtp_weirtype(sb,k)==1 .and. watdepact>0.) then
            qstage = dtp_cdis(sb,k) * 1.84 * dtp_totwrwid(sb) * (watdepact ** 1.5) * 60. *    &
             time%dtm !m3/s
            qout = qout + qstage 
          end if
         

		 else
		 !! Use stage-discharge relationship if available
		  if (dtp_stagdis(sb)==1) then  
		     select case(dtp_reltype(sb))
		       case(1) !! 1 is exponential function
		         qout = dtp_coef1(sb) * exp(dtp_expont(sb) * qdepth) + dtp_intcept(sb) 
		       case(2) !! 2 is Linear function
		         qout = dtp_coef1(sb) * qdepth + dtp_intcept(sb)       
		       case(3) !! 3 is logarthmic function
		         qout = dtp_coef1(sb) * log(qdepth) + dtp_intcept(sb)  
		       case(4) !! 4 is power function
		         qout = dtp_coef1(sb) * (qdepth**3) + dtp_coef2(sb) *      &
      	         (qdepth**2) + dtp_coef3(sb) * qdepth + dtp_intcept(sb)
               case(5)
                 qout = dtp_coef1(sb)*(qdepth**dtp_expont(sb))+            &
                       dtp_intcept(sb)
			 end select 
		     qout = qout * 60. * time%dtm
	      end if  !! end of stage-discharge calculation
	     end if  
                     
         !! Check mss balance for flow
         if (qout>qin) then !no detention occurs
            qout = qin
            qpnd = 0.
         else !detention occurs
            !!	Estimating surface area of water
            backup_length = qdepth / ch_hyd(sb)%s
            call surf_seep(qdepth,backup_length,seep_sa)
            call surf_evap(qdepth,backup_length,evap_sa)

            !! converting surface area to hectares (ha)
            seep_sa = seep_sa / 10000.0  
            evap_sa = evap_sa / 10000.0  

            !!	Estimate rainfall, evapotranspiration, and seepage
            pcp_vol  = 10.0 * sub_subp_dt(sb,ii) * evap_sa !m^3
            evap_vol = 10.0 * dtp_evrsv(sb) * pet_day * evap_sa !m^3
            seep_vol = 10.0 * ch_k(2,sb) * seep_sa * time%dtm / 60. !m^3

            !!	Check mass balance for water in the pond
            qpnd = qin + pcp_vol - qout - evap_vol - seep_vol
            if (qpnd<0) qpnd = 0.
	   end if
         
         !! Mass balance for sediment
         sedout = spndconc * qout !tons
         sedpnd = Max(0.,sedin - sedout) !tons
	      
   	   !! Store flow/sediment out of the pond at the subbasin outlet
   	   !ob(icmd)%ts(1,ii)%flo = Max(0.,qout)
   	   !ob(icmd)%ts(1,ii)%sed = Max(0.,sedout)

	   
      end do  !! Outermost do loop ends here

      ! Store end-of-day values for next day
      dtp_ivol(sb) = qpnd !m^3
      dtp_ised(sb) = sedpnd !tons
      
      return

      end subroutine bmp_det_pond 
   !-------------------------------------------------------------------
   	
	subroutine surf_seep(a,b,sa)

   !!	This subroutine computes seepage surface area of 
   !!	water backed up behind the detention pond weir
   !!	references: mathworld.wolfram.com  and 
   !!	en.wikipedia.org/wiki/Prolate_spheroid

	   implicit none
	   real::pi,a,b,sa,asq,bsq,ecc,intercal


	   pi=3.141593

	   asq=a*a
	   bsq=b*b
	   ecc=sqrt(1-(asq/bsq))
	   intercal=a*b*asin(ecc)/ecc

	   sa=pi*(asq+intercal)/2.0
   	
   	   return
	end subroutine surf_seep
   !-------------------------------------------------------------------

	subroutine surf_evap(a,b,saevap)

   !!	This subroutine computes surface area of 
   !!	water backed up available for evaporation

	   implicit none
	   real::pi,a,saevap,b

	   pi=3.141593
	   saevap=pi*a*b/2.0
   	
	   return
	end subroutine surf_evap
   !-------------------------------------------------------------------