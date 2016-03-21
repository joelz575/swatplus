      subroutine pl_rootfr	
	!! This subroutine distributes dead root mass through the soil profile
	!! code developed by Armen R. Kemanian in 2008 
	!! March, 2009 further adjustments expected

	real :: sol_thick(hru(ihru)%sol%nly)
	real :: cum_rd, cum_d, cum_rf, x1, x2
	integer :: k, l, jj
	
	jj = ihru
      
      if (stsol_rd(jj) < 1.e-6) then
         soil(jj)%ly(1)%rtfr = 1
         return
      endif

	! Normalized Root Density = 1.15*exp[-11.7*NRD] + 0.022, where NRD = normalized rooting depth
	! Parameters of Normalized Root Density Function from Dwyer et al 19xx
	a = 1.15
	b = 11.7
	c = 0.022
	d = 0.12029 ! Integral of Normalized Root Distribution Function 
				! from 0 to 1 (normalized depth) = 0.12029

	l = 0
	k = 0
	cum_d = 0.
	cum_rf = 0.
         sol_thick(:) = 0.
         rtfr = 0.

	do l = 1, hru(jj)%sol%nly
	  if (l == 1) then
	    sol_thick(l) = soil(jj)%phys(l)%d
	  else	
	    sol_thick(l) = soil(jj)%phys(l)%d - soil(jj)%phys(l-1)%d
	  end if
		
	  cum_d = cum_d + sol_thick(l)
	  if (cum_d >= stsol_rd(jj)) cum_rd = stsol_rd(jj)
	  if (cum_d < stsol_rd(jj)) cum_rd = cum_d
	  x1 = (cum_rd - sol_thick(l)) / stsol_rd(jj)
	  x2 = cum_rd / stsol_rd(jj)
           xx1 = -b * x1
	  if (xx1 > 20.) xx1 = 20.
           xx2 = -b * x2
           if (xx2 > 20.) xx2 = 20.
	     soil(jj)%ly(l)%rtfr = (a/b*(Exp(xx1) - Exp(xx2)) + c *(x2 - x1))/d
           xx = cum_rf
	  cum_rf = cum_rf + soil(jj)%ly(l)%rtfr
           if (cum_rf > 1.) then
	       soil(jj)%ly(l)%rtfr = 1. - xx
             cum_rf = 1.0
           end if
	  k = l
	  if (cum_rd >= stsol_rd(jj)) Exit
		 
	end do

	!!	 ensures that cumulative fractional root distribution = 1
	do l = 1, hru(jj)%sol%nly
		soil(jj)%ly(l)%rtfr = soil(jj)%ly(l)%rtfr / cum_rf
		If (l == k) Exit ! exits loop on the same layer as the previous loop
    end do
    
    return
   
	end subroutine pl_rootfr