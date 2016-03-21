      subroutine nut_pminrl2
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.  
!!    this is the alternate phosphorus model described in Vadas and White (2010)
   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    ihru         |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    solp(:)		|mg/kg	       |Solution pool phosphorous content
!!    actpp(:)    |mg/kg	       |Active pool phosphorous content
!!    stap(:)		|mg/kg	       |Stable pool phosphorous content
!!    vara					       |Intermediate Variable
!!    varb					       |Intermediate Variable
!!    varc					       |Intermediate Variable
!!    arate 					   |Intermediate Variable      |
!!    j           |none            |HRU number
!!    l           |none            |counter (soil layer)
!!    rmn1        |kg P/ha         |amount of phosphorus moving from the solution
!!                                 |mineral to the active mineral pool in the
!!                                 |soil layer
!!    roc         |kg P/ha         |amount of phosphorus moving from the active
!!                                 |mineral to the stable mineral pool in the 
!!                                 |soil layer
!!    rto         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: j, l
      real :: rto, rmn1, roc, wetness, base, vara, varb, varc, as_p_coeff
	!real*8  solp(mlyr),actpp(mlyr),stap(mlyr) !! locals for concentation based data

      j = 0
      j = ihru
        
	do l = 1, hru(j)%sol%nly !! loop through soil layers in this HRU
	!! make sure that no zero or negative pool values come in
	if (soil(j)%nut(l)%solp <= 1.e-6) soil(j)%nut(l)%solp = 1.e-6
	if (soil(j)%nut(l)%actp <= 1.e-6) soil(j)%nut(l)%actp = 1.e-6
      if (soil(j)%nut(l)%stap <= 1.e-6) soil(j)%nut(l)%stap = 1.e-6
      
!! Convert kg/ha to ppm so that it is more meaningful to compare between soil layers
	  solp = soil(j)%nut(l)%solp / soil(j)%phys(l)%conv_wt * 1000000.
	  actpp = soil(j)%nut(l)%actp / soil(j)%phys(l)%conv_wt * 1000000.
	  stap = soil(j)%nut(l)%stap/ soil(j)%phys(l)%conv_wt * 1000000.

!! ***************Soluble - Active Transformations***************	

	  !! Dynamic PSP Ratio
	    !!PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
	    if (soil(j)%phys(l)%clay > 0.) then
	      bsn_prm%psp = -0.045 * log(soil(j)%phys(l)%clay)+ (0.001 * solp) 
	      bsn_prm%psp = bsn_prm%psp - (0.035  * soil(j)%cbn(l)%cbn) + 0.43
	    else
	      bsn_prm%psp = 0.4
	    end if    		
		!! Limit PSP range
		if (bsn_prm%psp < .1)  bsn_prm%psp = 0.1 ! limits on PSP
	    if (bsn_prm%psp > 0.7)  bsn_prm%psp = 0.7  

        !! Calculate smoothed PSP average 
	  if (soil(j)%ly(l)%psp_store > 0.) then
	    bsn_prm%psp = (soil(j)%ly(l)%psp_store * 29. + bsn_prm%psp * 1.)/30
	  end if
        !! Store PSP for tomarrows smoothing calculation
	  soil(j)%ly(l)%psp_store = bsn_prm%psp

!!***************Dynamic Active/Soluble Transformation Coeff******************

	!! on day 1 just set to a value of zero
	   if ((iida == 1) .and. (time%yrs == 1)) then 
           soil(j)%ly(l)%a_days = 0 !! days since P Application 
           soil(j)%ly(l)%b_days = 0 !! days since P deficit
	   end if	   

	   !! Calculate P balance
		rto = 0.
		rto = bsn_prm%psp / (1.-bsn_prm%psp)
		rmn1 = 0.
		rmn1 = soil(j)%nut(l)%solp - soil(j)%nut(l)%actp * rto !! P imbalance

	  !! Move P between the soluble and active pools based on vadas et al., 2006
		if (rmn1 >= 0.) then !! Net movement from soluble to active	
		  rmn1 = Max(rmn1, (-1 * soil(j)%nut(l)%solp))
		!! Calculate Dynamic Coefficant		
          vara = 0.918 * (exp(-4.603 * bsn_prm%psp))          
		  varb = (-0.238 * ALOG(vara)) - 1.126
		  if (soil(j)%ly(l)%a_days >0) then 
		    arate = vara * (soil(j)%ly(l)%a_days ** varb)
		  else
		    arate = vara * (1) ** varb
		  end if
		  !! limit rate coeff from 0.05 to .5 helps on day 1 when a_days is zero
		  if (arate > 0.5) arate  = 0.5
		  if (arate < 0.1) arate  = 0.1
		  rmn1 = (arate) * rmn1		
	    soil(j)%ly(l)%a_days = soil(j)%ly(l)%a_days  + 1 !! add a day to the imbalance counter
	    soil(j)%ly(l)%b_days = 0
          End if

		if (rmn1 < 0.) then !! Net movement from Active to Soluble 		
		  rmn1 = Min(rmn1, soil(j)%nut(l)%actp)	
		  !! Calculate Dynamic Coefficant
		  base = (-1.08 * bsn_prm%psp) + 0.79
		  varc = base * (exp (-0.29))
	       !! limit varc from 0.1 to 1
		  if (varc > 1.0) varc  = 1.0
		  if (varc < 0.1) varc  = 0.1
          rmn1 = rmn1 * varc
		  soil(j)%ly(l)%a_days = 0
		  soil(j)%ly(l)%b_days = soil(j)%ly(l)%b_days  + 1 !! add a day to the imbalance counter
        End if

!!*************** Active - Stable Transformations ******************
        !! Estimate active stable transformation rate coeff
	  !! original value was .0006
		!! based on linear regression rate coeff = 0.005 @ 0% CaCo3 0.05 @ 20% CaCo3
		  as_p_coeff = 0.0023 * soil(j)%ly(l)%cal + 0.005 
          if (as_p_coeff > 0.05) as_p_coeff = 0.05
         if (as_p_coeff < 0.002) as_p_coeff = 0.002
        !! Estimate active/stable pool ratio
        !! Generated from sharpley 2003
      	ssp = 25.044 * (actpp + (actpp * rto))** -0.3833 
	  ! limit ssp to range in measured data
	  if (ssp > 10.) ssp = 10.
	  if (ssp < 0.7) ssp = 0.7

	  ! Smooth ssp, no rapid changes
		 if (soil(j)%ly(l)%ssp_store > 0.) then
		    ssp = (ssp + soil(j)%ly(l)%ssp_store * 99.)/100.
		 end if
		  	
	   roc = 0.
         roc = ssp * (soil(j)%nut(l)%actp + soil(j)%nut(l)%actp * rto) 
		 roc = roc - soil(j)%nut(l)%stap
		 roc = as_p_coeff * roc 
		 !! Store todays ssp for tomarrows calculation
		 soil(j)%ly(l)%ssp_store = ssp

!! **************** Account for Soil Water content, do not allow movement in dry soil************
         wetness = (soil(j)%phys(l)%st/soil(j)%phys(l)%fc) !! range from 0-1 1 = field cap
		 if (wetness >1.)  wetness = 1.
		 if (wetness <0.25)  wetness = 0.25 
		 rmn1 = rmn1 * wetness
		 roc  = roc  * wetness
	  
!! If total P is greater than 10,000 mg/kg do not allow transformations at all
	   If ((solp + actpp + stap) < 10000.) then 
	      !! Allow P Transformations
		  soil(j)%nut(l)%stap = soil(j)%nut(l)%stap + roc
		  if (soil(j)%nut(l)%stap < 0.) soil(j)%nut(l)%stap = 0.
		  soil(j)%nut(l)%actp = soil(j)%nut(l)%actp - roc + rmn1
		  if (soil(j)%nut(l)%actp < 0.) soil(j)%nut(l)%actp = 0.
		  soil(j)%nut(l)%solp = soil(j)%nut(l)%solp - rmn1
		  if (soil(j)%nut(l)%solp < 0.) soil(j)%nut(l)%solp = 0.
	   end if

!! Add water soluble P pool assume 1:5 ratio based on sharpley 2005 et al
	soil(j)%ly(l)%watp = soil(j)%nut(l)%solp / 5

      end do
      return
      end subroutine nut_pminrl2