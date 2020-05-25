      subroutine ch_rthr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach 
!!    using a constant storage coefficient  
!!	Routing method: Variable Storage routing   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_n(2,:)       |none          |Manning"s "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    pet_ch          |mm H2O        |potential evapotranspiration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation losses for hour
!!    hrttlc(:)    |m^3 H2O       |transmission losses for hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rhy(:)          |m H2O         |main channel hydraulic radius
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    scoef       |none          |storage coefficient
!!    nstep       |none          |No. of steps in a day (depends on model operational time step)
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!	Modified by N.Kannan, Blackland Research, Temple, USA

      use basin_module
      use climate_module
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob
      use channel_velocity_module
      
      implicit none

      integer :: ii        !none          |counter (hour)
      integer :: inhyd     !none          |inflow hydrograph storage location number 
      integer :: inum2     !none          |inflow hydrograph storage location number
      integer :: icmd      !units         |description
      integer :: jrch      !none          |reach number
      real :: c            !none          |inverse of channel side slope
      real :: p            !m             |wetted perimeter
      real :: scoef        !none          |storage coefficient
      real :: vol          !m^3 H2O       |volume of water in reach
      real :: topw         !m             |width of channel at water level
      real :: qman         !m^3/s or m/s  |flow rate or flow velocity

      inhyd = 0
      inhyd = inum2

!	nstep = int(1440 / time%dtm) !! nstep is a global variable

!!     start of sub-daily loop

      do ii = 1, time%step

        !! water entering reach during time step
        wtrin = ob(icmd)%ts(1,ii)%flo  

        !! calculate volume of water in reach
        if (ii == 1) then
          hrchwtr(ii) = ch(jrch)%rchstor
          vol = wtrin + ch(jrch)%rchstor
        else
          hrchwtr(ii) = hhstor(ii-1)
          vol = wtrin + hhstor(ii-1)
        end if
        vol = Max(vol,1.e-14) ! changed from e-4 to e-14 for urban modeing by J.Jeong 4/21/2008

        !! calculate cross-sectional area of flow

        hharea(ii) = vol / (ch_hyd(jhyd)%l * 1000.)

        !! calculate depth of flow

        c = 0.
        c = ch_hyd(jhyd)%side
        if (hharea(ii) <= ch_vel(jrch)%area) then
           hdepth(ii) = Sqrt(hharea(ii) / c + ch_vel(jrch)%wid_btm *        &         
           ch_vel(jrch)%wid_btm / (4. * c * c)) - ch_vel(jrch)%wid_btm / (2. * c)
          if (hdepth(ii) < 0.) hdepth(ii) = 0.
        else
          hdepth(ii) = Sqrt((hharea(ii) - ch_vel(jrch)%area) / 4. + 25. * & 
             ch_hyd(jhyd)%w * ch_hyd(jhyd)%w / 64.) - 5. *              &
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
             4. * ch_hyd(jhyd)%w + 2. * (hdepth(ii) - ch_hyd(jhyd)%d) *     &
               Sqrt(17.)
        end if

        !! calculate hydraulic radius

        rhy(ii) = 0.
        if (p > 0.01) then
          rhy(ii) = hharea(ii) / p
        else
          rhy(ii) = 0.
        end if

        !! calculate rate of flow in reach

        hsdti(ii) = Qman(hharea(ii), rhy(ii), ch_hyd(jhyd)%n,               & 
                                              ch_hyd(jhyd)%s)
       
        if (hsdti(ii) > 0.) then

          !! calculate travel time

          hhtime(ii) = ch_hyd(jhyd)%l * hharea(ii) / (3.6 * hsdti(ii))
          if (hhtime(ii) < 1.) then
            rttime = rttime + hhtime(ii)
          else
            rttime = rttime + 1.
          end if

          !! calculate volume of water leaving reach on day

          scoef = 0.
          scoef = 2. / (2. * hhtime(ii) + 1.)
          if (scoef > 1.) scoef = 1.
          hrtwtr(ii) = scoef * vol
          if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.


          !! calculate transmission losses

          hrttlc = 0.
          if (hhtime(ii) < 1.) then
            hrttlc(ii)=ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p * hhtime(ii)
          else
            hrttlc(ii) = ch_hyd(jhyd)%k * ch_hyd(jhyd)%l * p
          end if
          hrttlc(ii) = Min(hrtwtr(ii),hrttlc(ii))
          hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
          rttlc = rttlc + hrttlc(ii)

          hrtevp = 0.
          if (hrtwtr(ii) > 0.) then

            !! calculate width of channel at water level

            topw = 0.
            if (hdepth(ii) <= ch_hyd(jhyd)%d) then
              topw = ch_vel(jrch)%wid_btm + 2. * hdepth(ii)*ch_hyd(jhyd)%side
            else
              topw = 5. * ch_hyd(jhyd)%w + 8. * (hdepth(ii) -            & 
                                               ch_hyd(jhyd)%d)
            end if

            !! calculate evaporation

            if (hhtime(ii) < 1.) then
              hrtevp(ii) = bsn_prm%evrch * pet_ch/time%step *               & 
               ch_hyd(jhyd)%l * topw * hhtime(ii)
            else
              hrtevp(ii) = bsn_prm%evrch * pet_ch/time%step *               &
               ch_hyd(jhyd)%l * topw
            end if
            if (hrtevp(ii) < 0.) hrtevp(ii) = 0.
            hrtevp(ii) = Min(hrtwtr(ii),hrtevp(ii))
            hrtwtr(ii) = hrtwtr(ii) - hrtevp(ii)
            rtevp = rtevp + hrtevp(ii)
          end if

          !! set volume of water in channel at end of hour

          if (ii == 1) then
            hhstor(ii) = ch(jrch)%rchstor + wtrin - hrtwtr(ii) -             & 
            hrtevp(ii) - hrttlc(ii)
          else
            hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) -                 & 
            hrtevp(ii) - hrttlc(ii)
          end if
          if (hhstor(ii) < 0.) then
            hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
            hhstor(ii) = 0.
            if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
          end if
        end if

      end do                     !! end of sub-daily loop

!! calculate amount of water in channel at end of day

!      if (hhstor(time%step) < 0.1.and.hrtwtr(ii-1)>0.) then
!        hrtwtr(time%step) = hrtwtr(time%step) + hhstor(time%step)
!        hhstor(time%step) = 0.
!      end if
      if (hrtwtr(time%step) < 0.) hrtwtr(time%step) = 0.
      
!! calculation of daily average values

      !! set volume of water in reach at end of day
      ch(jrch)%rchstor = hhstor(time%step)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow area
      rcharea = Sum (hharea) / time%step
      !! calculate average flow depth
      rchdep = Sum(hdepth) / time%step
      !! calculate average flow rate
      sdti = Sum(hsdti) / time%step

      return
      end subroutine ch_rthr