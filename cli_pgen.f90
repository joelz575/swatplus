      subroutine cli_pgen(iwgn)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates precipitation data when the user chooses to 
!!    simulate or when data is missing for particular days in the weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    npcp(:)     |none          |prior day category
!!                               |1 dry day
!!                               |2 wet day
!!    pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    pcp_stat(:,3,:)|none       |skew coefficient for the average daily
!!                               |precipitation
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    rcor        |none          |correction coefficient for generated rainfall
!!                               |to ensure that the annual means for generated
!!                               |and observed values are comparable. (needed
!!                               |only if RDIST=1)
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
      use parm, only : i_mo, npcp, rcor

      real :: vv, pcpgen, v8, r6, xlv

      pcpgen = 0.
      vv = 0.
      vv = Aunif(rndseed(idg(1),iwgn))
      if (npcp(iwst) == 1)  then
        xx = wgn(iwgn)%pr_wd(i_mo)
      else
        xx = wgn(iwgn)%pr_ww(i_mo)
      endif
      if (vv > xx) then
        pcpgen = 0.
      else
        v8 = 0.
        v8 = Aunif(rndseed(idg(3),iwgn))
        if (bsn_prm%rdist == 0) then
          !!skewed rainfall distribution
          r6 = 0.
          xlv = 0.
          r6 = wgn(iwgn)%pcpskw(i_mo) / 6.
          xlv = (cli_Dstn1(rnd3(iwgn),v8) - r6) * r6 + 1.
          xlv = (xlv**3 - 1.) * 2. / wgn(iwgn)%pcpskw(i_mo)
          rnd3(iwgn) = v8
          pcpgen = xlv * wgn(iwgn)%pcpstd(i_mo) +            &                     
                              wgn_pms(iwgn)%pcpmean(i_mo)
          pcpgen = pcpgen * wgn_pms(iwgn)%pcf(i_mo)
        else
          !! mixed exponential rainfall distribution
          pcpgen=((-Log(v8))**bsn_prm%rexp)*wgn_pms(iwgn)%pcpmean(i_mo) * rcor
        end if
        if (pcpgen < .1) pcpgen = .1
      end if

      wst(iwst)%weat%precip = pcpgen

      return
      end subroutine cli_pgen