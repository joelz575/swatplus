      subroutine cli_slrgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates solar radiation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    solarav(:,:)|MJ/m^2/day    |average daily solar radiation for the month
!!    wgncur(3,:) |none          |parameter which predicts impact of precip on
!!                               |daily solar radiation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rav         |MJ/m^2        |modified monthly average solar radiation
!!    rx          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!      integer, intent (in) :: j

      use parm, only : i_mo, wgncur
      use hydrograph_module
      use climate_parms

      real :: rx, rav

      rav = wgn(iwgn)%solarav(i_mo) / (1. - 0.5 * wgn_pms(iwgn)%pr_wdays(i_mo))
      if (wst(iwst)%weat%precip > 0.0) rav = 0.5 * rav
      rx = wst(iwst)%weat%solradmx - rav
      wst(iwst)%weat%solrad = rav + wgncur(3,iwgn) * rx / 4.
      if (wst(iwst)%weat%solrad <= 0.) wst(iwst)%weat%solrad = .05 * wst(iwst)%weat%solradmx
      
      wst(iwst)%weat%solrad = wgn(iwgn)%solarav(i_mo)
      return
      end subroutine cli_slrgen