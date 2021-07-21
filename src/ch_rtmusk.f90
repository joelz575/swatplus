      subroutine ch_rtmusk
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the
!!    Muskingum method

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning"s "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    flwin(:)    |m^3 H2O       |flow into reach on previous day
!!    flwout(:)   |m^3 H2O       |flow out of reach on previous day
!!    i           |none          |current day of simulation
!!    pet_day     |mm H2O        |potential evapotranspiration
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
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob, icmd, jrch, isdch
      use time_module
      use channel_velocity_module
      use sd_channel_module
      use climate_module
      
      implicit none
      
      integer :: nn        !none              |number of subdaily computation points for stable 
                           !                  |routing in the muskingum routing method
      integer :: ii        !none              |counter
      integer :: i         !none              |current day of simulation
      real :: xkm          !hr                |storage time constant for the reach on
                           !                  |current day 
      real :: det          !hr                |time step (24 hours)
      real :: yy           !none              |variable to hold intermediate calculation
                           !                  |value
      real :: c1           !units             |description 
      real :: c2           !units             |description
      real :: c3           !units             |description
      real :: c4           !m^3 H2O           |
      real :: p            !m                 |wetted perimeter
      real :: vol          !m^3 H2O           |volume of water in reach at beginning of
                           !                  |day
      real :: c            !none              |inverse of channel side slope
      real :: rh           !m                 |hydraulic radius
      real :: topw         !m                 |top width of main channel
      real :: msk1         !units             |description 
      real :: msk2         !units             |description 
      real :: detmax       !units             |description 
      real :: detmin       !units             |description 
      real :: qinday       !units             |description 
      real :: qoutday      !units             |description  
	  real :: volrt        !units             |description 
      real :: maxrt        !units             |description 
      real :: adddep       !units             |description 
      real :: addp         !units             |description 
      real :: addarea      !units             |description 
	  real :: rttlc1       !units             |description 
      real :: rttlc2       !units             |description 
      real :: rtevp1       !units             |description 
      real :: rtevp2       !units             |description 
      real :: qman         !m^3/s or m/s      |flow rate or flow velocity
      real :: vc           !m/s               |flow velocity in reach
      real :: aaa          !units             |description 
      real :: sum_rttlc    !                  |           !! Van Tam Nguyen 10/2018
      real :: sum_rtevp    !                  |           !! Van Tam Nguyen 10/2018 
      
      real :: inflo        !m^3           |inflow water volume
      real :: xs_area      !m^2           |cross section area of channel
      real :: dep_flo      !m             |depth of flow
      real :: wet_perim    !m             |wetted perimeter
      real :: ttime        !hr            |travel time through the reach
      real :: t_inc        !hr            |time in routing step - 1/time%step
      real :: outflo       !m^3           |outflow water volume
      real :: tl           !m^3           |transmission losses during time step
      real :: trans_loss   !m^3           |transmission losses during day
      real :: rate_flo     !m^3/s         |flow rate
      real :: ev           !m^3           |evaporation during time step
      real :: evap         !m^3           |evaporation losses during day
      integer :: iwst

      jrch = isdch
      jhyd = sd_dat(jrch)%hyd
      
      qinday = 0
      qoutday = 0
      det = 24.
      sum_rttlc = 0.0   !! Van Tam Nguyen 10/2018
      sum_rtevp = 0.0   !! Van Tam Nguyen 10/2018
      ch(jrch)%chfloodvol = 0.

      !! Compute storage time constant for reach (msk_co1 + msk_co2 = 1.)
	  msk1 = bsn_prm%msk_co1 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
	  msk2 = bsn_prm%msk_co2 / (bsn_prm%msk_co1 + bsn_prm%msk_co2)
	  bsn_prm%msk_co1 = msk1
	  bsn_prm%msk_co2 = msk2
      xkm = ch_vel(jrch)%wid_btm * bsn_prm%msk_co1 + ch_vel(jrch)%stor_dis_1bf * bsn_prm%msk_co1
      
      !! Muskingum numerical stability -Jaehak Jeong, 2011
      !! Check numerical stability
      detmax = 2.* xkm * (1.- bsn_prm%msk_x)
      detmin = 2.* xkm * bsn_prm%msk_x
      
      !! Discretize time interval to meet the stability criterion 
      if (det > detmax) then
        if (det / 2. <= detmax) then
          det = 12
          nn = 2
        elseif (det / 4. <= detmax) then
          det = 6
          nn = 4
        else
          det = 1
          nn = 24
        endif
      else
        det = 24
        nn = 1
      end if
      
      !! volume at start of day
      vol = sd_ch(jrch)%stor

      !! subdaily time step
      do ii = 1, time%step

        !! water entering reach during time step
        inflo = ob(icmd)%tsin(ii)

        !! update volume of water in reach
        vol = vol + inflo
        vol = Max(vol, 1.e-14)

        !! Find average flowrate in a subdaily time interval
        volrt = vol / (86400. / nn)

        !! Find maximum flow capacity of the channel at bank full
        c = ch_hyd(jhyd)%side
	    p = sd_ch_vel(jrch)%wid_btm + 2. * sd_ch(jrch)%chd * Sqrt(1. + c * c)
	    rh = sd_ch_vel(jrch)%area / p
	    maxrt = Qman(ch_vel(jrch)%area, rh, ch_hyd(jhyd)%n, sd_ch(jrch)%chs)

        sdti = 0.
	    rchdep = 0.
	    p = 0.
	    rh = 0.
	    vc = 0.

        !! If average flowrate is greater than than the channel capacity at bank full
        !! then simulate flood plain flow else simulate the regular channel flow
        if (volrt > maxrt) then
	      rcharea = sd_ch_vel(jrch)%area
	      rchdep = sd_ch(jrch)%chd
	      p = sd_ch_vel(jrch)%wid_btm + 2. * sd_ch(jrch)%chd * Sqrt(1. + c * c)
	      rh = sd_ch_vel(jrch)%area / p
	      sdti = maxrt
	      adddep = 0
	      !! find the crossectional area and depth for volrt by iteration method at 1cm interval depth
	      !! find the depth until the discharge rate is equal to volrt
	      Do While (sdti < volrt)
            adddep = adddep + 0.01
            addarea = rcharea + ((sd_ch(jrch)%chw * 5) + 4 * adddep) * adddep
            addp = p + (sd_ch(jrch)%chw * 4) + 2. * adddep * Sqrt(1. + 4 * 4)
	        rh = addarea / addp
            sdti = Qman(addarea, rh, ch_hyd(jhyd)%n, sd_ch(jrch)%chs)
	      end do
	      rcharea = addarea
	      rchdep = sd_ch(jrch)%chd + adddep
	      p = addp
	      sdti = volrt
          !! store floodplain water that can be used by riparian HRU"s [Ann van Griensven]       
          ch(jrch)%chfloodvol = (volrt - maxrt)* 86400 * rt_delt
	    else
	      !! find the crossectional area and depth for volrt
	      !! by iteration method at 1cm interval depth
	      !! find the depth until the discharge rate is equal to volrt
	      Do While (sdti < volrt)
	        rchdep = rchdep + 0.01
	        rcharea = (sd_ch_vel(jrch)%wid_btm + c * rchdep) * rchdep
	        p = sd_ch_vel(jrch)%wid_btm + 2. * rchdep * Sqrt(1. + c * c)
	        rh = rcharea / p
	        sdti = Qman(rcharea, rh, ch_hyd(jhyd)%n, sd_ch(jrch)%chs)
	      end do
	      sdti = volrt
	    end if

        !! calculate top width of channel at water level
        if (rchdep <= sd_ch(jrch)%chd) then
          topw = sd_ch_vel(jrch)%wid_btm + 2. * rchdep * c
        else
          topw = 5 * sd_ch(jrch)%chw + 2. *(rchdep - sd_ch(jrch)%chd) * 4.
        end if

        if (sdti > 0) then

        !! calculate velocity and travel time
        vc = sdti / rcharea
	    rttime = sd_ch(jrch)%chl * 1000. / (3600. * vc)

        !! Compute coefficients
        yy = 2. * xkm * (1. - bsn_prm%msk_x) + det
        c1 = (det - 2. * xkm * bsn_prm%msk_x) / yy
        c2 = (det + 2. * xkm * bsn_prm%msk_x) / yy
        c3 = (2. * xkm * (1. - bsn_prm%msk_x) - det) / yy
        c4 = 0.

   ! ***    outflo = c1 * wtrin + c2 * ch(jrch)%flwin + c3 * ch(jrch)%flwout
	    outflo = Min(outflo, (vol))

          ttime = Min(24., ttime)
          !! calculate transmission losses
          tl = sd_ch(jrch)%chk * sd_ch(jrch)%chl * wet_perim * ttime   !mm/hr * km * mm * hr = m3       
          tl = Min(tl, outflo)
          outflo = outflo - tl
          trans_loss = trans_loss + tl

          !! calculate evaporation
          if (outflo > 0.) then
            !! calculate width of channel at water level
            if (dep_flo <= sd_ch(jrch)%chd) then
              topw = sd_ch_vel(jrch)%wid_btm + 2. * dep_flo * sd_ch(jrch)%chss
            else
              topw = 5. * sd_ch(jrch)%chw + 8. * (dep_flo - sd_ch(jrch)%chd)
            end if
            
            iwst = 1
            ev = bsn_prm%evrch * wst(iwst)%weat%pet * sd_ch(jrch)%chl * topw * ttime
            if (ev < 0.) ev = 0.
            ev = Min(ev, outflo)
            outflo = outflo - ev
            evap = evap + ev
          end if

          !! set volume of water in channel at end of hour
          vol = vol - outflo - tl - ev
          ob(icmd)%hyd_flo(1,ii) = outflo
          
        end if          !! rate_flo > 0. 

      end do            !! end of sub-daily loop

      !! save storage volume for next day
      sd_ch(jrch)%stor = vol

      return
      end subroutine ch_rtmusk
