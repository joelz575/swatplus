      subroutine ch_rtbact
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes bacteria through the stream network

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrchwtr(:)       |m^3 H2O     |water stored in reach at beginning of hour
!!    inum1            |none        |reach number
!!    inum2            |none        |inflow hydrograph storage location number
!!    rch_bactlp(:)    |# cfu/100ml |less persistent bacteria stored in reach
!!    rch_bactp(:)     |# cfu/100ml |persistent bacteria stored in reach
!!    rchwtr           |m^3 H2O     |water stored in reach at beginning of day
!!    thbact           |none        |temperature adjustment factor for bacteria
!!                                  |die-off/growth
!!    wdlprch          |1/day       |Die-off factor for less persistent bacteria
!!                                  |in streams
!!    wdprch           |1/day       |Die-off factor for persistent bacteria in 
!!                                  |streams
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hbactlp(:)   |# cfu/100mL  |less persistent bacteria in reach/outflow
!!                               |during hour
!!    hbactp(:)    |# cfu/100mL  |persistent bacteria in reach/outflow during
!!                               |hour
!!    rch_bactlp(:)|# cfu/100ml  |less persistent bacteria in reach/outflow
!!                               |at end of day
!!    rch_bactp(:) |# cfu/100ml  |persistent bacteria in reach/outflow at end
!!                               |of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    initlp      |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (less persistent)
!!    initp       |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (persistent)
!!    jrch        |none          |reach number
!!    netwtr      |m^3 H2O       |net amount of water in reach during time step
!!    tday        |day           |routing time for the reach
!!    totbactlp   |10^4 cfu      |mass less persistent bacteria
!!    totbactp    |10^4 cfu      |mass persistent bacteria
!!    wtmp        |deg C         |temperature of water in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use time_module

      real, external :: Theta

      real :: totbactp, totbactlp, netwtr, initlp, initp
      real :: tday, wtmp

      if (rtwtr > 0. .and. rchdep > 0.) then

      wtmp = 5.0 + 0.75 * wst(iwst)%weat%tave
      if (wtmp <= 0.) wtmp = 0.1

!     skipping hourly bacteria route for now  04/16/07 nubs
      if (time%step > 0) then                !! hourly mass balance
        initlp = 0.
        initp = 0.
        initlp = ch(jrch)%rch_bactlp
        initp = ch(jrch)%rch_bactp
        do ii = 1, time%step
          !! total bacteria mass in reach
          totbactp = 0.
          totbactlp = 0.
          totbactp = ob(icmd)%ts(1,ii)%bacp * ob(icmd)%ts(1,ii)%flo       &    
                                      + initp * hrchwtr(ii)
          totbactlp = ob(icmd)%ts(1,ii)%baclp * ob(icmd)%ts(1,ii)%flo     &
                                      + initlp * hrchwtr(ii)

          !! compute bacteria die-off
          totbactp = totbactp * Exp(-Theta(wdprch / 24.,thbact,wtmp))
          totbactp = Max(0., totbactp)
          totbactlp = totbactlp * Exp(-Theta(wdlprch / 24.,thbact,wtmp))
          totbactlp = Max(0., totbactlp)

          !! new concentration
          netwtr = 0.
          netwtr = ob(icmd)%ts(1,ii)%flo  + hrchwtr(ii)
          if (netwtr >= 1.) then
            hbactp(ii) = totbactp / netwtr
            hbactlp(ii) = totbactlp / netwtr
          end if
          initlp = 0.
          initp = 0.
          initp = hbactp(ii)
          initlp = hbactlp(ii)
        end do
      end if

      !! daily mass balance
      !! total bacteria mass in reach

      totbactp = ob(icmd)%hin%bacp * ob(icmd)%hin%flo     &   
                                           + ch(jrch)%bactp * rchwtr
      totbactlp = ob(icmd)%hin%baclp * ob(icmd)%hin%flo                &              
                         + ch(jrch)%bactlp * rchwtr

      !! compute bacteria die-off
      !! calculate flow duration
      tday = rttime / 24.0
      if (tday > 1.0) tday = 1.0
      totbactp = totbactp * Exp(-Theta(wdprch,thbact,wtmp)*tday)
      totbactp = Max(0., totbactp)
      totbactlp = totbactlp * Exp(-Theta(wdlprch,thbact,wtmp)*tday) 
      totbactlp = Max(0., totbactlp)

      !! new concentration
      netwtr = 0.
      netwtr = ob(icmd)%hin%flo  + rchwtr
	
!!	!! change made by CS while running region 4; date 2 jan 2006	
	 if (totbactp < 1.e-6) totbactp = 0.0 
	 if (totbactlp < 1.e-6) totbactlp = 0.0
      if (netwtr >= 1.) then
        ch(jrch)%rch_bactp = totbactp / netwtr
        ch(jrch)%rch_bactlp = totbactlp / netwtr
      else
        ch(jrch)%rch_bactp = 0.
        ch(jrch)%rch_bactlp = 0.
      end if
      
      end if

      return
      end subroutine ch_rtbact