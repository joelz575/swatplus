      subroutine cli_pgenhr(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes daily rainfall exponentially within the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    hru_sub(:)   |none          |subbasin in which HRU is located
!!    idg(:)       |none          |array location of random number seed
!!                                |used for a given process
!!    jj           |none          |HRU number
!!    rndseed(:,:) |none          |random number generator seed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wst(:)%weat%ts(:)   |mm H2O        |rainfall during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ab           |mm H2O        |lowest value al5 can have
!!    ajp          |mm H2O        |highest value al5 can have
!!    al5          |none          |fraction of total rainfall on day that occurs
!!                                |during 0.5h highest intensity rainfall
!!    altc         |none          |equation coefficient
!!    blm          |none          |lowest random number value allowed
!!    dur          |hours         |duration of storm during day
!!    ihour        |none          |counter
!!    itime        |none          |time step during day
!!    j            |none          |HRU number
!!    k            |none          |random number seed, counter
!!    nhour        |none          |number of time steps per hour
!!    pkrain       |mm H2O        |volume of rain at time of peak rainfall
!!    pkrr         |mm/hr         |peak rainfall rate
!!    pt           |min           |time during day
!!    qmn          |none          |mean random number value
!!    rtp          |min           |time of peak rainfall rate
!!    rx           |mm H2O        |total rainfall at end of time step
!!    sumrain      |mm H2O        |total amount of daily rainfall prior to
!!                                |time step
!!    uplm         |none          |highest random number value
!!    vv           |none          |random number between 0.0 and 1.0 that 
!!                                |represents time to peak rainfall rate
!!                                |expressed as a fraction of storm duration
!!    xk1          |none          |1st constant in dimensionless exponential
!!                                |rainfall distribution
!!    xk2          |none          |2nd constant in dimensionless exponential
!!                                |rainfall distribution
!!    xkp1         |hr            |1st constant in exponential rainfall
!!                                |distribution
!!    xkp2         |hr            |2nd constant in exponential rainfall
!!                                |distribution
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Atri, Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use time_module
      use parm, only : al5
      use hydrograph_module

      integer, intent (in) :: iwgn
      integer :: itime, pt, ihour, nhour, k
      real :: vv, blm, qmn, uplm, dur, ab, ajp, altc, pkrain, rtp
      real :: xk1, xk2, xkp1, xkp2, rx, pkrr, sumrain

      !! zero subdaily precip array
      wst(iwst)%weat%ts = 0.
      
      if (wst(iwst)%weat%precip < 1.e-6) return
      
      !! calculate maximum half-hour rainfall
      ab = 0.02083
      ajp = 1. - expo(-125. / (wst(iwst)%weat%precip + 5.))
      al5 = Atri(ab, wgn_pms(iwgn)%amp_r(time%mo), ajp, rndseed(10,iwgn))

      !! need peak rainfall rate 
      !! calculate peak rate using same method used for peak runoff
      pkrr = 2. * wst(iwst)%weat%precip * al5

      !! generate random number between 0.0 and 1.0
      !! because all input set to constant value, vv always the same
      !! vv => time to peak expressed as fraction of total storm duration
      blm = 0.05
      qmn = 0.25
      uplm = 0.95
      k = 8
      vv = Atri(blm, qmn, uplm, k)
      
      !! calculate storm duration
      xk1 = vv / 4.605
      xk2 = (1.- vv) / 4.605
      dur = wst(iwst)%weat%precip / (pkrr * (xk1 + xk2))
      if (dur > 24.0) then
        dur = 24.0
        pkrr = wst(iwst)%weat%precip / (dur * (xk1 + xk2))
      end if

      !! calculate amount of total rainfall fallen at time of peak
      !! rainfall and time of peak rainfall in units of minutes
      pkrain = vv * wst(iwst)%weat%precip
      rtp = vv * dur * 60

      !! calculate constants for exponential rainfall distribution
      !! equation
      xkp1 = dur * xk1 
      xkp2 = dur * xk2

      pt = time%dtm
      itime = 1
      sumrain = 0.

      !! do before time of peak rainfall
      !! do while pt less than rtp
      do
        if (pt >= Int(rtp)) exit
        rx = 0.
        rx = pkrain - pkrr * xkp1 *                                      &                                  
                  (1. - Exp((Real(pt) - rtp) / (60. * xkp1)))
        wst(iwst)%weat%ts(itime) = rx - sumrain
        pt = pt + time%dtm
        itime = itime + 1
        if (itime > time%step) exit
        sumrain = rx
      end do

      !! after peak rainfall and before end of storm
      do
        if (pt >= Int(dur * 60.)) exit
        rx = pkrain + pkrr * xkp2 *                                     &                                     
              (1. - Exp((rtp - Real(pt)) / (60. * xkp2)))
        wst(iwst)%weat%ts(itime) = rx - sumrain
        pt = pt + time%dtm
        itime = itime + 1
        if (itime > time%step) exit
        sumrain = rx
      end do

      !! at end of storm
      if (wst(iwst)%weat%precip > sumrain .and. itime <= time%step) then
        wst(iwst)%weat%ts(itime-1) = wst(iwst)%weat%ts(itime-1) + (wst(iwst)%weat%precip - sumrain)
      end if

      return
      end subroutine cli_pgenhr