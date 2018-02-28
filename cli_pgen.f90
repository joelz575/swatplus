      subroutine cli_pgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates precipitation data when the user chooses to 
!!    simulate or when data is missing for particular days in the weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    pcp_stat(:,3,:)|none       |skew coefficient for the average daily
!!                               |precipitation
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    rnd3(:)     |none          |random number between 0.0 and 1.0
!!    rndseed(:,:)|none          |random number seeds 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rnd3(:)     |none          |random number between 0.0 and 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pcpgen      |mm H2O        |generated precipitation value for the day
!!    r6          |none          |variable to hold intermediate calculation
!!    v8          |none          |random number between 0.0 and 1.0
!!    vv          |none          |random number between 0.0 and 1.0
!!    xlv         |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use climate_module
      use hydrograph_module
      use time_module

      real :: vv, pcpgen, v8, r6, xlv

      pcpgen = 0.
      vv = Aunif(rndseed(idg(1),iwgn))
      if (wst(iwst)%weat%precip_prior_day == 'dry')  then
        xx = wgn(iwgn)%pr_wd(time%mo)
      else
        xx = wgn(iwgn)%pr_ww(time%mo)
      endif
      if (vv > xx) then
        pcpgen = 0.
      else
        v8 = Aunif(rndseed(idg(3),iwgn))
        !!skewed rainfall distribution
        r6 = wgn(iwgn)%pcpskw(time%mo) / 6.
        xlv = (cli_Dstn1(rnd3(iwgn),v8) - r6) * r6 + 1.
        xlv = (xlv**3 - 1.) * 2. / wgn(iwgn)%pcpskw(time%mo)
        rnd3(iwgn) = v8
        pcpgen = xlv * wgn(iwgn)%pcpstd(time%mo) + wgn_pms(iwgn)%pcpmean(time%mo)
        pcpgen = pcpgen * wgn_pms(iwgn)%pcf(time%mo)
        if (pcpgen < .1) pcpgen = .1
      end if

      wst(iwst)%weat%precip = pcpgen

      return
      end subroutine cli_pgen