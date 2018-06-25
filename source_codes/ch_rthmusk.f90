      subroutine ch_rthmusk

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes flow through a reach using the
!!    Muskingum method at a given time step 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_n(2,:)   |none          |Manning"s "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    curyr       |none          |current year of simulation (consecutive)
!!    flwin(:)    |m^3 H2O       |flow into reach on previous day
!!    flwout(:)   |m^3 H2O       |flow out of reach on previous day
!!    pet_ch      |mm H2O        |potential evapotranspiration for the day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current day
!!    flwout(:)   |m^3 H2O       |flow out of reach on current day
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow for time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation from reach during time step
!!    hrttlc(:)   |m^3 H2O       |transmission losses from reach during time step
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rhy(:)      |m H2O         |main channel hydraulic radius
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
!!    det         |hr            |time step 
!!    ii          |none          |counter (Number of operational step during day)
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    tbase       |none          |flow duration (fraction of 1 hr)
!!    nstep       |none          |number of steps in a day
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of day
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
!!	Modified by N.Kannan, Blackland Research Center, Temple, USA

      use basin_module
      use climate_module
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob
      use channel_velocity_module
      
      implicit none

      integer :: ii        !none          |counter (Number of operational step during day)
      integer :: jrch      !none          |reach number
      integer :: icmd      !units         |description 
      integer :: i         !units         |description 
      real :: qman         !m^3/s or m/s  |flow rate or flow velocity
      real :: xkm          !hr            |storage time constant for the reach on
                           !              |current day
      real :: det          !hr            |time step
      real :: yy           !none          |variable to hold intermediate calculation
                           !              |value
      real :: c1           !units         |description
      real :: c2           !units         |description 
      real :: c3           !units         |description
      real :: c4           !m^3 H2O       |
      real :: p            !m             |wetted perimeter
      real :: vol          !m^3 H2O       |volume of water applied in irrigation 
                           !              |operation
      real :: c            !none          |inverse of channel side slope
      real :: tbase        !none          |flow duration (fraction of 1 hr)
      real :: topw         !m             |top width of main channel

      !! set current day within subdaily hydrograph ts
 !! Compute storage time constant for reach
      xkm = ch_vel(jrch)%st_dis * bsn_prm%msk_co1 + ch_vel(jrch)%stor_dis_1bf * bsn_prm%msk_co2
      det = time%dtm / 60. 

 !! Compute coefficients
      yy = 2. * xkm * (1. - bsn_prm%msk_x) + det
      c1 = (det - 2. * xkm * bsn_prm%msk_x) / yy
      c2 = (det + 2. * xkm * bsn_prm%msk_x) / yy
      c3 = (2. * xkm * (1. - bsn_prm%msk_x) - det) / yy
      c4 = ch_vel(jrch)%vel_bf * ch_hyd(jhyd)%l * det / yy
 	
      do ii = 1, time%step   !! begin time step loop
        !! Water entering reach on day
        wtrin = ob(icmd)%tsin(ii)%flo 
 
        !! Compute water leaving reach at the end of time step
        if (time%yrs == 1 .and. i == time%day_start .and. ii == 1) then
         hrtwtr(ii) = c1 * wtrin + c2 * ch(jrch)%rchstor + c3 * ch(jrch)%rchstor + c4
        else
         hrtwtr(ii) = c1 * wtrin + c2 * ch(jrch)%flwin + c3 * ch(jrch)%flwout
        end if
        if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.

        !! define flow parameters for current time step
        ch(jrch)%flwin = wtrin
        ch(jrch)%flwout = hrtwtr(ii)
        !! calculate volume of water in reach
        vol = 0.
        if (ii == 1) then
          hrchwtr(ii) = ch(jrch)%rchstor
          vol = wtrin + ch(jrch)%rchstor
        else
          hrchwtr = hhstor(ii-1)
          vol = wtrin + hhstor(ii-1)
        end if
        vol = Max(vol,1.e-14) ! changed from e-4 to e-14 for urban modeing by J.Jeong 4/21/2008

        !! calculate cross-sectional area of flow
        hharea(ii) = vol / (ch_hyd(jhyd)%l * 1000.)

        !! calculate depth of flow
        c = ch_hyd(jhyd)%side
        if (hharea(ii) <= ch_vel(jrch)%area) then
          hdepth(ii) = Sqrt(hharea(ii) / c + ch_vel(jrch)%wid_btm *          &         
            ch_vel(jrch)%wid_btm / (4. * c * c)) -ch_vel(jrch)%wid_btm / (2. * c)
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
        else
          hdepth(ii) = Sqrt((hharea(ii) - ch_vel(jrch)%area) / 4. + 25. *   &
          ch_hyd(jhyd)%w * ch_hyd(jhyd)%w / 64.) - 5.*                    &  
                                              ch_hyd(jhyd)%w / 8.
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
          hdepth(ii) = hdepth(ii) + ch_hyd(jhyd)%d
        end if

        !! calculate wetted perimeter
        p = 0.
        if (hdepth(ii) <= ch_hyd(jhyd)%d) then
          p = ch_vel(jrch)%wid_btm + 2. * hdepth(ii) * Sqrt(1. + c * c)
        else
          p = ch_vel(jrch)%wid_btm + 2. * ch_hyd(jhyd)%d * Sqrt(1. + c * c) +    &
             4. * ch_hyd(jhyd)%w + 2. * (hdepth(ii) - ch_hyd(jhyd)%d)       &
                                                     * Sqrt(17.)
        end if

        !! calculate hydraulic radius
        rhy(ii) = 0.
        if (p > 0.01) then
          rhy(ii) = hharea(ii) / p
        else
          rhy(ii) = 0.
        end if

        !! calculate flow in reach [m3/s]
        hsdti(ii) = Qman(hharea(ii), rhy(ii), ch_hyd(jhyd)%n,           &
                                                   ch_hyd(jhyd)%s)

        !! calculate travel time[hour]
        if (hsdti(ii) > 1.e-4) then
         hhtime(ii) = ch_hyd(jhyd)%l * hharea(ii) / (3.6 * hsdti(ii))
         if (hhtime(ii) < 1.) then
           rttime = rttime + hhtime(ii)
         else
           rttime = rttime + 1.
         end if
        end if

        !! calculate transmission losses
        !! transmission losses are ignored if ch_hyd(jhyd)%k is set to zero
        !! in .rte file
        if (hhtime(ii) < 1.) then
          hrttlc(ii) = ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p * hhtime(ii)
        else
          hrttlc(ii) = ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p
        end if
        hrttlc(ii) = Min(hrtwtr(ii),hrttlc(ii))
        hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
        rttlc = rttlc + hrttlc(ii)

        if (hrtwtr(ii) > 0.) then
          !! calculate flow duration
          tbase = 0.
          tbase = hhtime(ii)
          if (tbase > 1.) tbase = 1.

          !! calculate width of channel at water level
          topw = 0.
          if (hdepth(ii) <= ch_hyd(jhyd)%d) then
            topw = ch_vel(jrch)%wid_btm + 2. * hdepth(ii) * ch_hyd(jhyd)%side
          else
            topw = 5. * ch_hyd(jhyd)%w + 2. *                            &            
                  (hdepth(ii)- ch_hyd(jhyd)%d) * 4.
          end if

          !! calculate evaporation
          if (hhtime(ii) < 1.) then
            hrtevp(ii) = bsn_prm%evrch * pet_ch/time%step *                 &
          ch_hyd(jhyd)%l * topw * hhtime(ii)
          else
            hrtevp(ii) = bsn_prm%evrch * pet_ch/time%step *                 &              
               ch_hyd(jhyd)%l * topw
          end if
          if (hrtevp(ii) < 0.) hrtevp(ii) = 0.
          hrtevp(ii) = Min(hrtwtr(ii),hrtevp(ii))
          hrtwtr(ii) = hrtwtr(ii) - hrtevp(ii)
          rtevp = rtevp + hrtevp(ii)
        end if

        !! set volume of water in channel at end of hour
        if (ii == 1) then
          hhstor(ii) = ch(jrch)%rchstor + wtrin - hrtwtr(ii)            &
          - hrtevp(ii) - hrttlc(ii)
        else
          hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) -              &              
                           hrtevp(ii) - hrttlc(ii)
        end if
        if (hhstor(ii) < 0.) then
          hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
          hhstor(ii) = 0.
          if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
        end if

      end do                   !! end time step loop

!! calculate amount of water in channel at end of day
      if (hhstor(time%step) < 10.) then
        hrtwtr(time%step) = hrtwtr(time%step)+hhstor(time%step)
        hhstor(time%step) = 0.
      end if
      if (hrtwtr(time%step) < 0.) hrtwtr(time%step) = 0.

!! daily average values
      !! set volume of water in reach at end of day
      ch(jrch)%rchstor = hhstor(time%step)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow cross-sectional area
      rcharea = Sum(hharea) / time%step
      !! calculate average flow depth
      rchdep = Sum(hdepth) / time%step
      !! calculate average flow rate
      sdti = Sum(hsdti) / time%step

      return
      end subroutine ch_rthmusk