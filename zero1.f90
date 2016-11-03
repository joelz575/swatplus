      subroutine zero1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use parm
      
!!  septic changes 6/07/10  jaehak
      bio_bod = 0.
      fcoli = 0.  
      bio_fecal = 0.  
      biom = 0.
      rbiom = 0.
      bz_perc = 0.
      plqm = 0.
      qstemm = 0
      i_sep = 0
      percp = 0
      sep_opt= 1
      sep_tsincefail = 0
     
      cont_cn = 0.
      cont_p = 0.
      filt_w = 0.
      grwat_veg = 0.
      ida_lup = 0           !CB 8/24/09\
      nop = 1 
      phu_op = 0.
!!  septic changes 1/29/09 
      plqm = 0.
      plq_rt = 0.
!!  septic changes 1/29/09
      pr_w = 0.
      pst_kg = 0.
      pst_dep = 0.
      pst_lag = 0.
      rchrg = 0.
      resouty = 0.
      rk1 = 0.
      rk2 = 0.
      rk3 = 0.
      rk4 = 0.
      rk5 = 0.
      rk6 = 0.
!!  septic changes 1/29/09
      rsp_rt = 0.
      slg_rt = 0.
!!  septic changes 1/29/09 
      rsdin = 0.
      sedst = 0.
      silt = 0.
      sno_hru = 0.
      sol_cov = 0.
      sol_pst = 0.
      sol_wp = 0.
      solarav = 0.
      solpstcnst = 0.
      solpstmon = 0.
      solpstyr = 0.
      srbpstcnst = 0.
      srbpstmon = 0.
      srbpstyr = 0.
      strip_n = 0.
      strip_cn = 0.
      strip_c = 0.
      strip_p = 0.
      rch_san = 0.
      rch_sil = 0.
      rch_cla = 0.
      rch_sag = 0.
      rch_lag = 0.
      rch_gra = 0.
      sub_fr = 0.
      sub_km = 0.
      sub_dgra = 0.
      sub_petco = 0.       ! CB 8/24/09
      sweepeff = 0.
      swtrg = 0
      t_ov = 0.
      tconc = 0.
      terr_p = 0.
      terr_cn = 0.
      terr_sl = 0.
      thalf = 0.
      tnconc = 0.
      tno3conc = 0.
      tnyld = 0.
      tnylda = 0.
      tpconc = 0.
      trapeff = 0.
      urbcoef = 0.
      urbcn2 = 0.
      usle_cfac = 0.
      usle_eifac = 0.
!!  septic changes 1/29/09
      vp = 0. 
      wfsh = 0.
      manure_kg = 0.
      depsilfp = 0.
      depsagfp = 0.
      deplagfp = 0.
      depgrafp = 0.
      depsanch = 0.
      depsilch = 0.
      depclach = 0.
      depsagch = 0.
      deplagch = 0.
      depgrach = 0.

      sanst = 0.
      silst = 0.
      clast = 0.
      sagst = 0.
      lagst = 0.
      grast = 0.

      return
      end