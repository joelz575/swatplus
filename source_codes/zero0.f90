      subroutine zero0

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use hru_module, only : aird,auto_wstr, bio_eat, bio_init,        &
       bio_min,bio_trmp,brt,bss,canstor,cn2,cnop,cumei,   &
       cumeira,cumrai,cumrt,divmax,dormhr,driftco,filterw,flowmin,frac_harvk,frt_kg,grz_days,  &
       hru,hru_dafr,igrz,      &
       irr_asq,irr_mx,irr_no,irr_noa,irr_sc,irr_sca,irramt,irrno,irrsc,irrsq,isep,isep_ly,iseptic,itb,        &
       lai_init,latno3,manure_id,nop,orgn_con,orgp_con,par,phubase,qird,ranrns_hru,     &
       sed_con,sepcrk,sol_sumsolp,soln_con,solp_con,sstmaxd,stmaxd,tileq,tmx,uh,         &
       wstrs_id,wt_shall,wtab,yr_skip

      implicit none
      
      real :: ammonian                    !mg N/L             |ammonia concentration in reach
      real :: cbodcnst                    !                   |
      real :: cbodmon                     !                   |
      real :: cbodyr                      !                   |
      real :: chlacnst                    !                   |
      real :: chlayr                      !                   |
      real :: chlamon                     !                   |
      real :: dc                          !                   |
      real :: curbden                     !km/ha              |curb length density 
      real :: deptil                      !mm                 |depth of mixing caused by tillage operation
      real :: dirtmx                      !kg/curb km         |maximum amount of solids allowed t
                                          !                   |build up on impervious surfaces
      real :: dorm_flag                   !                   |
      real :: dtwt                        !                   |
      real :: dz                          !                   |
      real :: effmix                      !none               |mixing efficiency of tillage operation
      real :: evrsv                       !none               |lake evap coeff
      real :: fimp                        !fraction           |fraction of HRU area that is
                                          !                   |impervious (both directly and
                                          !                   |indirectly connected)
      real :: fminn                       !kg minN/kg frt     |fract of fert which is mineral nit (NO3+NH3)
      real :: fminp                       !kg minN/kg frt     |frac of fert which is mineral phos 
      real :: fnh3n                       !kg NH3-N/kg N      |frac of mineral N content of fert which is NH3
      real :: forgn = 0.                  !kg orgN/kg frt     |frac of fert which is org n
      real :: forgp = 0.                  !kg orgP/kg frt     |frac of fert which is org p
      real :: gee                         !none               |factor -g- in Kirkham equation
      real :: gw_delaye                   !                   |
      real :: hdrain                      !mm                 |equivalent depth from water surface in drain tube to
                                          !                   |impermeable layer
      real :: hqd                         !                   |
      integer :: iop                      !none               !counter
      integer :: ipest                    !none               !counter
      integer :: irreff                   !                   |
      integer :: irr_sq                   !                   |
      integer :: idum                     !                   |
      real :: pltnfr                      !kg N/kg biomass    |nitrogen uptake parameter normal fraction
                                          !                   |of N in crop biomass at emergence 
      real :: pltpfr                      !kg P/kg biomass    |phosphorus uptake parameter normal
                                          !                   |fraction of P in crop biomass at emergence
      real :: ranrns                      !mm                 |random roughness of a given tillage operation
      real :: rchrg_n                     !                   |amount of nitrate getting to the shallow aquifer 
      real :: sol_sumn03                  !                   |
      real :: gw_qdeep                    !                   | 
      real :: tile_no3                    !                   |
      real :: eo_30d                      !                   |
 

      wtab = 0.8
!    Drainmod tile equations  01/2006 
      cumeira = 0.
      cumei = 0.
      cumrai = 0.
      cumrt = 0.
      ranrns_hru = 20.
!    Drainmod tile equations  01/2006
      lai_init = 0.
      aird = 0.
      ammonian = 0.
      auto_wstr = 0.
      bio_init = 0.
      bio_min = 0.
      bio_eat = 0.
      bio_trmp = 0.
      brt = 0.
      bss = 0.
      canstor = 0.
      cbodcnst = 0.
      cbodmon = 0.
      cbodyr = 0.

!!    Initialization by balaji
      chlacnst = 0.
      chlamon = 0.
      chlayr = 0.
      cn2 = 0.
      cnop = 0.
      curbden = 0.
      dc = 0.
      deptil = 0.
      dirtmx = 0.
      divmax = 0.
      dormhr = 0.
      dorm_flag = 0
      driftco = 0.
      dtwt = 600.
      dz = 0
      eo_30d = 0.
      effmix = 0.
      evrsv = 0.
      filterw = 0.
      fimp = 0.
      flowmin = 0.
      fminn = 0.
      fminp = 0.
      fnh3n = 0.
      forgn = 0.
      forgp = 0.
      frac_harvk = 0.
      frt_kg = 0.
!    Drainmod tile equations  01/2006 
      gee = 0.
!    Drainmod tile equations  01/2006
      gw_delaye = 0.
!    Drainmod tile equations  01/2006 
      hdrain = 0.
!    Drainmod tile equations  01/2006
      hqd = 0.
      hru_dafr = 0.
      wstrs_id = 0
      manure_id = 0
      igrz = 0
      iop = 0
      ipest = 0
      irramt = 0.
      irreff = 1.
      irrsc = 0
      irrno = 0
      irr_sca = 0
      irr_noa = 0
      irrsq = 0
      irrno = 0
      irrsc = 0
      irr_sc = 0
      irr_no = 0
      irr_sq = 0
      irr_asq = 0
      irr_sca = 0
      irr_noa = 0
      iseptic = 0
      isep_ly = 0
      itb = 0
      grz_days = 0
      irr_mx = 0.
  !! change per JGA irrigation 4/2/2009
      latno3 = 0.
      orgn_con = 0.
      orgp_con = 0.
	  phubase = 0.
      pltnfr = 0.
      pltpfr = 0.
!! drainmod tile equations   06/2006
      ranrns = 0.
!! drainmod tile equations   06/2006
      qird = 0.
      rchrg_n = 0.
!! routing 5/3/2010 gsm per jga
      idum = 0
      sstmaxd = 0.
!    Drainmod tile equations  01/2006
      sed_con = 0.
      sepcrk = 0.
!    Drainmod tile equations  01/2006 
      stmaxd = 0.
!    Drainmod tile equations  01/2006 

!!   added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
      sol_sumn03 = 0.
      sol_sumsolp = 0.
      soln_con = 0.
      solp_con = 0.
      gw_qdeep = 0.
      tile_no3 = 0.
      tileq = 0.
      uh = 0.
      wt_shall = 0.
      yr_skip = 0

      return
      end