      subroutine zero0

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use parm
      isdr_no = 0
      
       ifirstatmo = 1
       mo_atmo = 0
      
!!    apex command initialize
      idapa = 0
      iypa = 0
      cf = 0.0
      cfh = 0.0
      cfdec = 0.0
      wtab = 0.8
!    Drainmod tile equations  01/2006 
      cumeira = 0.
      cumei = 0.
      cumrai = 0.
      cumrt = 0.
      ranrns_hru = 20.
!    Drainmod tile equations  01/2006
      afrt_surface = 0.
      lai_init = 0.
      aird = 0.
      ammonian = 0.
      auto_eff = 0.
      auto_nyr = 0.
      auto_napp = 0.
      auto_nstrs = -1.
      auto_wstr = 0.
      bactkddb = 0.
      bactlpdb = 0.
      bactpdb = 0.
      bio_init = 0.
      bio_min = 0.
      bio_targ = 0.
      bio_eat = 0.
      bio_trmp = 0.
      brt = 0.
      bss = 0.
      canstor = 0.
      cbodcnst = 0.
      cbodmon = 0.
      cbodyr = 0.
      cfrt_id = 0
      cfrt_kg = 0.
      ch_k = 0.

!!    Initialization by balaji
      chlacnst = 0.
      chlamon = 0.
      chlayr = 0.
      cn1 = 0.
      cn2 = 0.
      cn3 = 0.
      cnop = 0.
      cpst_id = 0
      curbden = 0.
      dc = 0.
      deptil = 0.
      dirtmx = 0.
      disoxcnst = 0.
      disoxmon = 0.
      disoxyr = 0.
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
      hi_targ = 0.
      hqd = 0.
      hru_dafr = 0.
      hrupest = 0
      hru_rufr = 0.
      iafrttyp = 0
      icfrt = 0
      icpst = 0
      icr = 0
      iday_fert = 0
      idc = 0
      idop = 0
      idtill = 0
      wstrs_id = 0
      ifirsta = 1 
      ifrt_freq = 0
      manure_id = 0
      igrz = 0
      iop = 0
      iopera = 1
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
      mcr = 1
      mgtop = 0
	  mgt1iop = 0
      mgt2iop = 0
      mgt3iop = 0
      mgt4op = 0.0
      mgt5op = 0.0
      mgt6op = 0.0
      mgt7op = 0.0
      mgt8op = 0.0
      mgt9op = 0.0
      mgt10iop = 0
      ndcfrt = 0
      fert_days = 0
      grz_days = 0
      irr_mx = 0.
  !! change per JGA irrigation 4/2/2009
      latno3 = 0.
      nicr = 0
      npno = 0
      ntil = 1
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
      sq_rto = 0.
!    Drainmod tile equations  01/2006 
      stmaxd = 0.
!    Drainmod tile equations  01/2006 

!!   added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
      sol_sumn03 = 0.
      sol_sumsolp = 0.
      strsw_sum = 0.
      strstmp_sum = 0.
      strsn_sum = 0.
      strsp_sum = 0.
      strsa_sum = 0.
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