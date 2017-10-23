      subroutine ch_rtmusk
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the
!!    Muskingum method

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    flwin(:)    |m^3 H2O       |flow into reach on previous day
!!    flwout(:)   |m^3 H2O       |flow out of reach on previous day
!!    i           |none          |current day of simulation
!!    pet_day     |mm H2O        |potential evapotranspiration
!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at
!!                               |bankfull depth
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    phi(10,:)   |hr            |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(13,:)   |hr            |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current day
!!    flwout(:)   |m^3 H2O       |flow out of reach on current day
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    c1          |
!!    c2          |
!!    c3          |
!!    c4          |m^3 H2O       |
!!    det         |hr            |time step (24 hours)
!!    jrch        |none          |reach number
!!    nn          |              |number of subdaily computation points for stable 
!!                               |routing in the muskingum routing method
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    tbase       |none          |flow duration (fraction of 24 hr)
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
!!    wtrin       |m^3 H2O       |water entering reach on day
!!    xkm         |hr            |storage time constant for the reach on
!!                               |current day
!!    yy          |none          |variable to hold intermediate calculation
!!                               |value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    code provided by Dr. Valentina Krysanova, Pottsdam Institute for
!!    Climate Impact Research, Germany
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University

      use basin_module
      use jrw_datalib_module
      use parm, only : pet_day, rcharea, rtwtr, sdti
      
      integer :: nn,ii
      real :: xkm, det, yy, c1, c2, c3, c4, wtrin, p, vol, c, rh
      real :: topw,msk1,msk2,detmax,detmin,qinday,qoutday
	real :: volrt, maxrt, adddep, addp, addarea
	real :: rttlc1, rttlc2, rtevp1, rtevp2

      qinday = 0; qoutday = 0
      det = 24.
      
!! Water entering reach on day
      wtrin = 0.
      wtrin = ob(icmd)%hin%flo 

!! Compute storage time constant for reach (msk_co1 + msk_co2 = 1.)
	msk1 = bsn_prm%msk_co1 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
	msk2 = bsn_prm%msk_co2 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
	bsn_prm%msk_co1 = msk1
	bsn_prm%msk_co2 = msk2
      xkm = 0.
      xkm = ch(jrch)%phi(10) * bsn_prm%msk_co1 + ch(jrch)%phi(13) * bsn_prm%msk_co1
      
!! Muskingum numerical stability -Jaehak Jeong, 2011
!! Check numerical stability
      detmax = 2.* xkm * (1.- bsn_prm%msk_x)
      detmin = 2.* xkm * bsn_prm%msk_x
      
!! Discretize time interval to meet the stability criterion 
      if (det>detmax) then
        if (det/2.<=detmax) then
            det = 12; nn = 2
        elseif (det/4.<=detmax) then
            det = 6; nn = 4
        else
            det = 1; nn = 24
        endif
      else
        det = 24; nn = 1
      end if
      
 !! Inflow during a sub time interval     
      wtrin = wtrin / nn
      
!! Iterate for the day      
      do ii=1,nn
      
 !! calculate volume of water in reach
         vol = 0.
         vol = wtrin + ch(jrch)%rchstor

!! Find average flowrate in a sub time interval
         volrt = vol / (86400. / nn)

!! Find maximum flow capacity of the channel at bank full
      c = 0.
      c = ch_hyd(jhyd)%side
	p = ch(jrch)%phi(6) + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c)
	rh = ch(jrch)%phi(1) / p
	maxrt = Qman(ch(jrch)%phi(1), rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)

      sdti = 0.
	rchdep = 0.
	p = 0.
	rh = 0.
	vc = 0.

!! If average flowrate is greater than than the channel capacity at bank full
!! then simulate flood plain flow else simulate the regular channel flow
      if (volrt > maxrt) then
	  rcharea =ch(jrch)%phi(1)
	  rchdep = ch_hyd(jhyd)%d
	  p = ch(jrch)%phi(6) + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c)
	  rh = ch(jrch)%phi(1) / p
	  sdti = maxrt
	  adddep = 0
	!! find the crossectional area and depth for volrt
	!! by iteration method at 1cm interval depth
	!! find the depth until the discharge rate is equal to volrt
	  Do While (sdti < volrt)
          adddep = adddep + 0.01
          addarea = rcharea + ((ch_hyd(jhyd)%w * 5) + 4 * adddep) *      &
                                                         adddep
          addp = p + (ch_hyd(jhyd)%w * 4) + 2. * adddep *                &
                                                     Sqrt(1. + 4 * 4)
	    rh = addarea / addp
          sdti = Qman(addarea, rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)
	  end do
	  rcharea = addarea
	  rchdep = ch_hyd(jhyd)%d + adddep
	  p = addp
	  sdti = volrt
	else
	!! find the crossectional area and depth for volrt
	!! by iteration method at 1cm interval depth
	!! find the depth until the discharge rate is equal to volrt
	  Do While (sdti < volrt)
	    rchdep = rchdep + 0.01
	    rcharea = (ch(jrch)%phi(6) + c * rchdep) * rchdep
	    p = ch(jrch)%phi(6) + 2. * rchdep * Sqrt(1. + c * c)
	    rh = rcharea / p
	    sdti = Qman(rcharea, rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)
	  end do
	  sdti = volrt
	end if

!! calculate top width of channel at water level
         topw = 0.
         if (rchdep <= ch_hyd(jhyd)%d) then
           topw = ch(jrch)%phi(6) + 2. * rchdep * c
         else
           topw = 5 * ch_hyd(jhyd)%w + 2. *(rchdep - ch_hyd(jhyd)%d) * 4.
         end if

      if (sdti > 0) then

!! calculate velocity and travel time
	   vc = sdti / rcharea
         ch(jrch)%vel_chan = vc
	   rttime = ch_hyd(jhyd)%l * 1000. / (3600. * vc)

!! Compute coefficients
      yy = 0.
      c1 = 0.
      c2 = 0.
      c3 = 0.
      c4 = 0.
      yy = 2. * xkm * (1. - bsn_prm%msk_x) + det
      c1 = (det - 2. * xkm * bsn_prm%msk_x) / yy
      c2 = (det + 2. * xkm * bsn_prm%msk_x) / yy
      c3 = (2. * xkm * (1. - bsn_prm%msk_x) - det) / yy

!! Compute water leaving reach on day
	   if (time%yrs == 1 .and. i == time%idaf) then
	     ch(jrch)%flwin = ch(jrch)%rchstor
	     ch(jrch)%flwout = ch(jrch)%rchstor
	   end if

         rtwtr = c1 * wtrin + c2 * ch(jrch)%flwin + c3 * ch(jrch)%flwout
	   if (rtwtr < 0.) rtwtr = 0.

	rtwtr = Min(rtwtr, (wtrin + ch(jrch)%rchstor))

!! calculate amount of water in channel at end of day
      ch(jrch)%rchstor = ch(jrch)%rchstor + wtrin - rtwtr
!! Add if statement to keep rchstor from becoming negative
      if (ch(jrch)%rchstor < 0.0) ch(jrch)%rchstor = 0.0

!! transmission and evaporation losses are proportionally taken from the 
!! channel storage and from volume flowing out

       !! calculate transmission losses
	  rttlc = 0.

	  if (rtwtr > 0.) then

	!!  Total time in hours to clear the water

      rttlc = det * ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p
	  rttlc2 = rttlc * ch(jrch)%rchstor / (rtwtr + ch(jrch)%rchstor)

	    if (ch(jrch)%rchstor <= rttlc2) then
	      rttlc2 = min(rttlc2, ch(jrch)%rchstor)
	      ch(jrch)%rchstor = ch(jrch)%rchstor - rttlc2
	      rttlc1 = rttlc - rttlc2
	      if (rtwtr <= rttlc1) then
	        rttlc1 = min(rttlc1, rtwtr)
	        rtwtr = rtwtr - rttlc1
	      else
	        rtwtr = rtwtr - rttlc1
	      end if
	    else
	      ch(jrch)%rchstor = ch(jrch)%rchstor - rttlc2
	      rttlc1 = rttlc - rttlc2
	      if (rtwtr <= rttlc1) then
	        rttlc1 = min(rttlc1, rtwtr)
	        rtwtr = rtwtr - rttlc1
	      else
	        rtwtr = rtwtr - rttlc1
	      end if
	    end if
	  rttlc = rttlc1 + rttlc2
        end if


        !! calculate evaporation
	  rtevp = 0.
       if (rtwtr > 0.) then

          aaa = bsn_prm%evrch * pet_day / 1000.

	      if (rchdep <= ch_hyd(jhyd)%d) then
               rtevp = aaa * ch_hyd(jhyd)%l * 1000. * topw
	      else
		      if (aaa <=  (rchdep - ch_hyd(jhyd)%d)) then
                 rtevp = aaa * ch_hyd(jhyd)%l * 1000. * topw
	         else
	           rtevp = (rchdep - ch_hyd(jhyd)%d) 
	           rtevp = rtevp + (aaa - (rchdep - ch_hyd(jhyd)%d)) 
                 topw = ch(jrch)%phi(6) + 2. * ch_hyd(jhyd)%d * c        
	           rtevp = rtevp * ch_hyd(jhyd)%l * 1000. * topw
	         end if
	      end if

	    rtevp2 = rtevp * ch(jrch)%rchstor / (rtwtr + ch(jrch)%rchstor)

	      if (ch(jrch)%rchstor <= rtevp2) then
	         rtevp2 = min(rtevp2, ch(jrch)%rchstor)
	         ch(jrch)%rchstor = ch(jrch)%rchstor - rtevp2
	         rtevp1 = rtevp - rtevp2
	         if (rtwtr <= rtevp1) then
	           rtevp1 = min(rtevp1, rtwtr)
	           rtwtr = rtwtr - rtevp1
	         else
	           rtwtr = rtwtr - rtevp1
	         end if
	      else
	         ch(jrch)%rchstor = ch(jrch)%rchstor - rtevp2
	         rtevp1 = rtevp - rtevp2
	         if (rtwtr <= rtevp1) then
	           rtevp1 = min(rtevp1, rtwtr)
	           rtwtr = rtwtr - rtevp1
	         else
	           rtwtr = rtwtr - rtevp1
	         end if
	      end if
	      rtevp = rtevp1 + rtevp2
         end if

!! define flow parameters for current iteration
         ch(jrch)%flwin = 0.
         ch(jrch)%flwout = 0.
         ch(jrch)%flwin = wtrin
         ch(jrch)%flwout = rtwtr

!! define flow parameters for current day
         qinday = qinday + wtrin
         qoutday = qoutday + rtwtr      
      
      
!! total outflow for the day
      rtwtr = qoutday

      else
        rtwtr = 0.
        sdti = 0.
	  ch(jrch)%rchstor = 0.
	  ch(jrch)%vel_chan = 0.
        ch(jrch)%flwin = 0.
        ch(jrch)%flwout = 0.
      end if
      
      end do

!! precipitation on reach is not calculated because area of HRUs 
!! in subbasin sums up to entire subbasin area (including channel
!! area) so precipitation is accounted for in subbasin loop

!!      volinprev(jrch) = wtrin
!!	qoutprev(jrch) = rtwtr

      if (rtwtr < 0.) rtwtr = 0.
      if (ch(jrch)%rchstor < 0.) ch(jrch)%rchstor = 0.

      if (ch(jrch)%rchstor < 10.) then
        rtwtr = rtwtr + ch(jrch)%rchstor
        ch(jrch)%rchstor = 0.
      end if

      return
      end subroutine ch_rtmusk