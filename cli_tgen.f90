      subroutine cli_tgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates temperature data when the user chooses to 
!!    simulate or when data is missing for particular days in the
!!    weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in month
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    wgncur(1,:) |none          |parameter which predicts impact of precip on
!!                               |daily maximum air temperature
!!    wgncur(2,:) |none          |parameter which predicts impact of precip on
!!                               |daily minimum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tamp        |deg C         |difference between mean monthly air temperature
!!                               |and monthly max or min temperature
!!    tmng        |deg C         |generated minimum temperature for the day
!!    tmxg        |deg C         |generated maximum temperature for the day
!!    txxm        |deg C         |modified monthly maximum temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real :: tmxg, tmng, tamp, txxm

      tamp = .5 * (wgn(iwgn)%tmpmx(i_mo) - wgn(iwgn)%tmpmn(i_mo))
      txxm = wgn(iwgn)%tmpmx(i_mo) + tamp * wgn_pms(iwgn)%pr_wdays(i_mo)
      
      if (wst(iwst)%weat%precip > 0.0) txxm = txxm - tamp

      tmxg = txxm + wgn(iwgn)%tmpstdmx(i_mo) * wgncur(1,iwgn)
      tmng = (wgn(iwgn)%tmpmn(i_mo)) + wgn(iwgn)%tmpstdmn(i_mo) *  wgncur(2,iwgn)
      if (tmng > tmxg) tmng = tmxg - .2 * Abs(tmxg)

      wst(iwst)%weat%tmax = tmxg
      wst(iwst)%weat%tmin = tmng

      return
      end subroutine cli_tgen