      subroutine zero0

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use hru_module, only : aird,auto_wstr, bio_eat, bio_init,        &
       bio_min,bio_trmp,brt,bss,canstor,cn2,cnop,cumei,   &
       cumeira,cumrai,cumrt,divmax,dormhr,driftco,filterw,flowmin,frac_harvk,frt_kg,grz_days,  &
       hru,hru_dafr,hrupest,igrz,      &
       irr_asq,irr_mx,irr_no,irr_noa,irr_sc,irr_sca,irramt,irrno,irrsc,irrsq,isep,isep_ly,iseptic,itb,        &
       lai_init,latno3,manure_id,nop,npno,orgn_con,orgp_con,par,phubase,qird,ranrns_hru,     &
       sed_con,sepcrk,sol_rd,sol_sumsolp,soln_con,solp_con,sstmaxd,stmaxd,stsol_rd,tileq,tmx,uh,         &
       wstrs_id,wt_shall,wtab,yr_skip

      use bacteria_module

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
      bactkddb = 0.
      bactlpdb = 0.
      bactpdb = 0.
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
      hrupest = 0
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
      npno = 0
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
      stsol_rd = 0.
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