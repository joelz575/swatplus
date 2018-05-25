      subroutine ch_rtday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes the daily flow through the reach using a 
!!    variable storage coefficient

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    pet_day     |mm H2O        |potential evapotranspiration
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    det         |hr            |time step (24 hours)
!!    c           |none          |inverse of channel side slope
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    scoef       |none          |Storage coefficient (fraction of water in 
!!                               |reach flowing out on day)
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
!!    wtrin       |m^3 H2O       |amount of water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt, Min
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University

      use basin_module
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob, icmd, jrch
      use channel_velocity_module

      implicit none
      
      real :: wtrin         !m^3 H2O       |amount of water flowing into reach on day
      real :: scoef         !none          |Storage coefficient (fraction of water in
      real :: p             !m             |wetted perimeter
      !real :: tbase        !not used in this subroutine
      real :: topw          !m             |top width of main channel
      real :: vol           !m^3 H2O       |volume of water in reach at beginning of
                            !              |day
      real :: c             !none          |inverse of channel side slope
      real :: rh            !m             |hydraulic radius
      real :: volrt         !units         |description 
      real :: maxrt         !units         |description 
      real :: dddep         !units         |description 
      real :: addp          !units         |description 
      real :: addarea       !units         |description 
      real :: vc            !m/s           |flow velocity in reach
      real :: aaa           !units         |description 
      real :: rttlc1        !units         |description 
      real :: rttlc2        !units         |description 
      real :: rtevp1        !units         |description 
      real :: rtevp2        !units         |description 
      real :: det           !hr            |time step (24 hours)
      real :: qman          !m^3/s or m/s  |flow rate or flow velocity
      real :: adddep        !units         |description  
      real :: rchvol        !m^3           | volume storage when travel time < cal time
      integer :: itermx     !units         |description 
      
      
      wtrin = ob(icmd)%hin%flo

      !! calculate volume of water in reach
      vol = wtrin

      !! Find average flowrate in a day
      volrt = vol / 86400

      !! Find maximum flow capacity of the channel at bank full
      c = ch_hyd(jhyd)%side
	  p = ch_vel(jrch)%wid_btm + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c)
	  rh = ch_vel(jrch)%area / p
	  maxrt = Qman(ch_vel(jrch)%area, rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)

      sdti = 0.
	  rchdep = 0.
	  p = 0.
	  rh = 0.
	  vc = 0.

      !! If average flowrate is greater than than the channel capacity at bank full
      !! then simulate flood plain flow else simulate the regular channel flow
      if (volrt > maxrt) then
	    rcharea = ch_vel(jrch)%area
	    rchdep = ch_hyd(jhyd)%d
	    p = ch_vel(jrch)%wid_btm + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c)
	    rh = ch_vel(jrch)%area / p
	    sdti = maxrt
	    adddep = 0
	    !! find the crossectional area and depth for volrt
	    !! by iteration method at 1cm interval depth
	    !! find the depth until the discharge rate is equal to volrt
        itermx = 0
	    Do While (sdti < volrt)
          adddep = adddep + 0.01
          addarea = rcharea + ((ch_hyd(jhyd)%w * 5) + 4 * adddep) * adddep
          addp = p + (ch_hyd(jhyd)%w * 4) + 2. * adddep * Sqrt(1. + 4 * 4)
	      rh = addarea / addp
          sdti = Qman(addarea, rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)
          itermx = itermx + 1
          if (itermx > 100) exit
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
	      rcharea = (ch_vel(jrch)%wid_btm + c * rchdep) * rchdep
	      p = ch_vel(jrch)%wid_btm + 2. * rchdep * Sqrt(1. + c * c)
	      rh = rcharea / p
          sdti = Qman(rcharea, rh, ch_hyd(jhyd)%n, ch_hyd(jhyd)%s)
	    end do
	    sdti = volrt
	  end if

      !! calculate top width of channel at water level
      topw = 0.
      if (rchdep <= ch_hyd(jhyd)%d) then
        topw = ch_vel(jrch)%wid_btm + 2. * rchdep * c
      else
        topw = 5 * ch_hyd(jhyd)%w + 2. * (rchdep - ch_hyd(jhyd)%d) * 4.
      end if

      !! Time step of simulation (in hour)
      det = 24.

      if (sdti > 0.) then
        !! calculate velocity and travel time
  	    vc = sdti / rcharea  
        ch(jrch)%vel_chan = vc
	    rttime = ch_hyd(jhyd)%l * 1000. / (3600. * vc)

        !! calculate volume of water leaving reach on day
        scoef = 0.
 	    rtwtr = 0.
        scoef = 2. * det / (2. * rttime + det)
                 
        !rtwtr = scoef * (wtrin + ch(jrch)%rchstor)
        rtwtr = scoef * wtrin
        if (scoef > 1.) then 
          rchvol = rcharea * ch_hyd(jhyd)%l * 1000.
          rtwtr = wtrin + ch(jrch)%rchstor - rchvol
        else 
          rtwtr = scoef * (wtrin + ch(jrch)%rchstor)    
        end if

        !! calculate amount of water in channel at end of day
        ch(jrch)%rchstor = ch(jrch)%rchstor + wtrin - rtwtr
        
        !! Add if statement to keep rchstor from becoming negative
        if (ch(jrch)%rchstor < 0.0) ch(jrch)%rchstor = rcharea * ch_hyd(jhyd)%l * 1000.

        !! transmission and evaporation losses are proportionally taken from the 
        !! channel storage and from volume flowing out

       !! calculate transmission losses
	    rttlc = 0.

	    if (rtwtr > 0.) then

	      !!  Total time in hours to clear the water
          det = min(det, rttime)
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
              topw = ch_vel(jrch)%wid_btm + 2. * ch_hyd(jhyd)%d * c           
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

      else
        rtwtr = 0.
        sdti = 0.
	    ch(jrch)%rchstor = 0.
	    ch(jrch)%vel_chan = 0.
        ch(jrch)%flwin = 0.
        ch(jrch)%flwout = 0.
      end if

      !! precipitation on reach is not calculated because area of HRUs 
      !! in subbasin sums up to entire subbasin area (including channel
      !! area) so precipitation is accounted for in subbasin loop

      !! volinprev(jrch) = wtrin; qoutprev(jrch) = rtwtr

      if (rtwtr < 0.) rtwtr = 0.
      if (ch(jrch)%rchstor < 0.) ch(jrch)%rchstor = 0.

      if (ch(jrch)%rchstor < 10.) then
        rtwtr = rtwtr + ch(jrch)%rchstor
        ch(jrch)%rchstor = 0.
      end if

      return
      end subroutine ch_rtday