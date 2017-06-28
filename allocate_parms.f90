      subroutine allocate_parms
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine allocates array sizes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mch         |none          |max number of channels
!!    mcr         |none          |max number of crops grown per year
!!    mhru        |none          |max number of HRUs
!!    mhyd        |none          |max number of hydrographs
!!    mlyr        |none          |max number of soil layers
!!    mpst        |none          |max number of pesticides used in wshed
!!    mrecm       |none          |max number of recmon files
!!    mres        |none          |max number of reservoirs
!!    nstep       |none          |max number of time steps per day
!!    msub        |none          |max number of subbasins
!!    myr         |none          |max number of years of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      use time_module
      
!! initialize variables    
      mvaro = 33
      mstdo = 113
      mhyd = 1  !!added for jaehak vars

      allocate (pot(mhru))
      
!!    drains
      allocate (wnan(10))
      allocate (ranrns_hru(mhru))
      
      !dimension plant arrays used each day and not saved
       mpc = 10
       allocate (uno3d(mpc))
       allocate (uapd(mpc))
       allocate (un2(mpc))
       allocate (up2(mpc))
       allocate (translt(mpc))
       allocate (par(mpc))
       allocate (htfac(mpc))
       allocate (epmax(mpc))
       epmax = 0.

!!    arrays for plant communities
      allocate (ipl_com(mhru))
      allocate (cht_mx(mhru))
      allocate (cvm_com(mhru))
      allocate (blai_com(mhru))
      allocate (rsdco_plcom(mhru))
      allocate (iplt_airr(mhru))
      allocate (iplt_afert(mhru))
      allocate (strsw_av(mhru))
      allocate (percn(mhru))
      allocate (tileq(mhru))
		
      mxsubch = Max(msub+1,mch+1)
      itempa = Max(mhru,mch)     
      
!!    apex/command variables
      allocate (ifirsta(mapex))
      allocate (iypa(mapex))
      allocate (idapa(mapex))
!! septic changes added 1/28/09 gsm
      allocate (percp(mhru))
      allocate (i_sep(mhru))
      allocate (sep_tsincefail(mhru))
      allocate (qstemm(mhru))
      allocate (bio_bod(mhru))
      allocate (biom(mhru))
      allocate (rbiom(mhru))
      allocate (fcoli(mhru))
      allocate (bz_perc(mhru))
!!    carbon outputs for .hru file
!!    carbon outputs for .hru file
      allocate (plqm(mhru))

      allocate (itb(mhru))
      
!! septic changes added 1/28/09 gsm
      allocate (qird(mhru))      
      if (time%step > 0) allocate (hhqday(mhru,time%step))
      allocate (hru1(msub))
      allocate (hrutot(msub))
      allocate (sub_bd(msub))
      allocate (sub_etday(msub))
      allocate (sub_fr(msub))
      allocate (sub_km(msub))
      allocate (sub_orgn(msub))
      allocate (sub_surfq(msub))
      allocate (sub_pst(mpst,msub))
      allocate (sub_hhwtmp(msub,time%step))   ! 24 changed to nstep 4 urban modeling  Oct. 19,2007
      allocate (uh(mhru,time%step+1))         !! was 49 changed to nstep  OCt22, 2007

      allocate (ch_k(2,mxsubch))
      allocate (sub_pet(mxsubch))
 
 !!  added per JGA for Srini by gsm 9/8/2011
 !! arrays for mangement output (output.mgt)  
      allocate (sol_sumno3(mhru))
      allocate (sol_sumsolp(mhru))
      allocate (strsw_sum(mhru))
      allocate (strstmp_sum(mhru))
      allocate (strsn_sum(mhru))
      allocate (strsp_sum(mhru))
      allocate (strsa_sum(mhru))

!!    arrays which contain data related to years of rotation,
!!    applications, and HRUs
      allocate (auto_wstr(mhru))
!! burn 3/5/09   
     
      allocate (cfrt_id(mhru))
      allocate (cfrt_kg(mhru))
      allocate (cpst_id(mhru))
      allocate (cpst_kg(mhru))
     
      allocate (wstrs_id(mhru))
      allocate (ifrt_freq(mhru))
      allocate (ipst_freq(mhru))

      allocate (imp_trig(mhru))
      allocate (irr_asq(mhru))
      allocate (irr_mx(mhru))
      allocate (irrsq(mhru))
      allocate (irr_eff(mhru))
      allocate (irr_sc(mhru))
      allocate (irr_no(mhru))
      allocate (irr_sca(mhru))
      allocate (irr_noa(mhru))
      allocate (iseptic(mhru))
      allocate (fert_days(mhru))
      allocate (pest_days(mhru))

 !!   burn 3/5/09

!!    changes pesticide incorporation in soil 3/31/08 gsm

!!    arrays which contain data related to years of rotation,
!!    crops grown per year, and HRUs
      allocate (hi_targ(mhru))  
      allocate (nstress(mhru))
      allocate (tnyld(mhru))
      allocate (tnylda(mhru))

!!    arrays which contain data related to years of rotation,
!!    grazings per year, and HRUs
      allocate (bio_eat(mhru))
      allocate (bio_trmp(mhru))
      allocate (grz_days(mhru))
      allocate (manure_id(mhru))
      allocate (manure_kg(mhru))

!!    arrays which contain data related to HRUs
      allocate (afrt_surface(mhru))
      allocate (aird(mhru))
      allocate (anano3(mhru))
      allocate (auto_eff(mhru))
      allocate (auto_nyr(mhru))
      allocate (auto_napp(mhru))
      allocate (auto_nstrs(mhru))
      allocate (bactlpq(mhru))
      allocate (bactlps(mhru))
      allocate (bactpq(mhru))
      allocate (bactps(mhru))
      allocate (bio_min(mhru))
      allocate (brt(mhru))
      allocate (canstor(mhru))
      allocate (cbodu(mhru))
      allocate (chl_a(mhru))
      allocate (cklsp(mhru))
      allocate (cn1(mhru))
      allocate (cn2(mhru))
      allocate (cn3(mhru))
      allocate (cnday(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (cumei(mhru))
	  allocate (cumeira(mhru))
	  allocate (cumrt(mhru))
	  allocate (cumrai(mhru))
!    Drainmod tile equations  01/2006
      allocate (divmax(mhru))
      allocate (dormhr(mhru))
      allocate (doxq(mhru))
      allocate (driftco(mhru))
      allocate (filterw(mhru))
      allocate (flowfr(mhru))
      allocate (flowmin(mhru))
      allocate (fsred(mhru))
      allocate (hru_dafr(mhru))
      allocate (hru_ra(mhru))
      allocate (hru_rmx(mhru))
      allocate (hru_sub(mhru))
      allocate (hrupest(mhru))
      allocate (iafrttyp(mhru))
      allocate (icfrt(mhru))
      allocate (icpst(mhru))
      allocate (icr(mhru))
      allocate (iday_fert(mhru))
      allocate (iday_pest(mhru))
      allocate (igrz(mhru))
      allocate (irramt(mhru))
      allocate (yr_skip(mhru))
      allocate (isweep(mhru))
      allocate (phusw(mhru))
      allocate (bio_targ(mhru))
      allocate (irr_flag(mhru))
      imho = max(mhru,20)
      allocate (irrno(mhru))
      allocate (irrsc(mhru))
      allocate (lai_yrmx(mhru))
      allocate (latno3(mhru))
      allocate (latq(mhru))
      allocate (ncf(mhru))
      allocate (ndeat(mhru))
      allocate (ndcfrt(mhru))
      allocate (ndcpst(mhru))
      allocate (ngr(mhru))
      allocate (nirr(mhru))
      allocate (nop(mhru))
      allocate (nplnt(mhru))
      allocate (ntil(mhru))
      allocate (orgn_con(mhru))
      allocate (orgp_con(mhru))
      allocate (ovrlnd(mhru))
      allocate (phubase(mhru))

      allocate (pplnt(mhru))
      allocate (qdr(mhru))
      allocate (rhd(mhru))

      allocate (rsdin(mhru))
      allocate (sci(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (sstmaxd(mhru))	  
!    Drainmod tile equations  01/2006 
      allocate (sedminpa(mhru))
      allocate (sedminps(mhru))
      allocate (sedorgn(mhru))
      allocate (sedorgp(mhru))
      allocate (sedyld(mhru))

      allocate (sanyld(mhru))
      allocate (silyld(mhru))
      allocate (clayld(mhru))
      allocate (sagyld(mhru))
      allocate (lagyld(mhru))
      allocate (grayld(mhru))
      allocate (sed_con(mhru))
      allocate (sepbtm(mhru))
      allocate (smx(mhru))
      allocate (sno_hru(mhru))
      allocate (snotmp(mhru))
      allocate (soln_con(mhru))
      allocate (solp_con(mhru))
      allocate (sol_cov(mhru))
!!    Drainmod tile equations  01/2006 
	allocate (stmaxd(mhru))
      allocate (itill(mhru))
      allocate (stsol_rd(mhru))
      allocate (surfq(mhru))
      allocate (surqno3(mhru))
      allocate (surqsolp(mhru))
      allocate (swtrg(mhru))
      allocate (t_ov(mhru))
      allocate (tauton(mhru))
      allocate (tautop(mhru))
      allocate (tcfrtn(mhru))
      allocate (tcfrtp(mhru))
      allocate (tconc(mhru))
      allocate (tc_gwat(mhru))
      allocate (tgrazn(mhru))
      allocate (tgrazp(mhru))
      allocate (tileno3(mhru))
      allocate (tmn(mhru))
      allocate (tmpav(itempa))
      allocate (tmp_hi(mhru))
      allocate (tmp_lo(mhru))
      allocate (tmx(mhru))
      allocate (trapeff(mhru))
      allocate (twash(mhru))
      allocate (u10(mhru))
      allocate (usle_cfac(mhru))
      allocate (usle_eifac(mhru))
!      allocate (usle_mult(mhru))
!      allocate (usle_ls(mhru))
      allocate (wtab(mhru))
      allocate (wtab_mn(mhru))
      allocate (wtab_mx(mhru))
      allocate (wfsh(mhru))


      allocate (bss(4,mhru))
      allocate (wrt(2,mhru))
      allocate (surf_bs(17,mhru))
      allocate (phi(13,mch))  
      allocate (wat_phi(13,mhru))
      allocate (rfqeo_30d(30,mhru))
      allocate (eo_30d(30,mhru))


!!    arrays which contain data related to pesticides, HRUs
      allocate (pst_lag(mpst,3,mhru))
      allocate (npno(mpst))

!!    arrays
      allocate (wpstaao(mpst,5))

!!Armen Jan 2008 end
!! sj aug 09 SWAT-C MC stuff
	allocate (cf(mhru))
	allocate (cfh(mhru))
	allocate (cfdec(mhru))
!! sj aug 09 end
	allocate (hhsurf_bs(2,mhru,time%step))  !! nstep changed to nstep  OCt. 18,2007
      allocate (ubnrunoff(time%step),ubntss(time%step))

!! Arrays for subdaily erosion modeling by Jaehak Jeong
	allocate (hhsedy(mhru,time%step),ovrlnd_dt(mhru,time%step))  
	allocate (dratio(msub),init_abstrc(mhru),hhsurfq(mhru,time%step))
	allocate (sub_subp_dt(msub,time%step),sub_hhsedy(msub,time%step))

!! Arrays for bmp simulation by jaehak jeong
	allocate (subdr_km(mhyd),subdr_ickm(mhyd),sub_cn2(msub))
      ! sedimentation-filtration
      allocate (num_sf(msub),sf_fr(msub,10),sf_dim(msub,10),            &
        sf_typ(msub,10),sf_im(msub,10),sf_iy(msub,10),sp_sa(msub,10),   &
        sp_pvol(msub,10),sp_pd(msub,10),sp_sedi(msub,10),               &
        sp_sede(msub,10),ft_sa(msub,10),ft_fsa(msub,10),                &
        ft_dep(msub,10),ft_h(msub,10),ft_pd(msub,10),                   &
        ft_k(msub,10),ft_dp(msub,10),ft_dc(msub,10),                    &
        ft_por(msub,10),tss_den(msub,10),ft_alp(msub,10),               &
        sp_qi(msub,10),sp_k(msub,10),sp_bpw(msub,10),                   &
        ft_bpw(msub,10),sp_dp(msub,10),ft_sed_cumul(msub,10),           &
        sp_sed_cumul(msub,10),ft_qfg(msub,10),sp_qfg(msub,10)) 
      allocate (sub_ha_imp(msub),ft_qpnd(msub,10),ft_qsw(msub,10),      &
        ft_qin(msub,10),ft_qout(msub,10),ft_sedpnd(msub,10),            &
        sf_ptp(msub,10),ft_fc(msub,10),sub_ha_urb(msub)) 
!! additional var by Ann

      ! detention pond
 	allocate(dtp_subnum(mhyd),dtp_imo(mhyd),dtp_iyr(mhyd),                  &
        dtp_numweir(mhyd),dtp_numstage(mhyd),                               &
        dtp_stagdis(mhyd),dtp_reltype(mhyd),dtp_onoff(mhyd))
	
	allocate(dtp_evrsv(msub),                                               &
        dtp_inflvol(msub),dtp_totwrwid(msub),dtp_parm(msub),                &
        dtp_wdep(msub),dtp_totdep(msub),dtp_watdepact(msub),                &
        dtp_outflow(msub),dtp_totrel(msub),dtp_backoff(msub),               &
        dtp_seep_sa(msub),dtp_evap_sa(msub),dtp_pet_day(msub),              &
        dtp_pcpvol(msub),dtp_seepvol(msub),dtp_evapvol(msub),               &
        dtp_flowin(msub),dtp_backup_length(msub),dtp_intcept(msub),         &
        dtp_expont(msub),dtp_coef1(msub),dtp_coef2(msub),                   &
        dtp_coef3(msub),dtp_ivol(msub),dtp_ised(msub))
 
  	allocate(dtp_wdratio(msub,10),dtp_depweir(msub,10),                     &
        dtp_diaweir(msub,10),dtp_retperd(msub,10),dtp_pcpret(msub,10),      &
        dtp_cdis(msub,10),dtp_flowrate(msub,10),                            &
        dtp_wrwid(msub,10),dtp_weirtype(msub,10),dtp_weirdim(msub,10),      &
        dtp_addon(msub,10)) 
      !! additional var by jeong for nutrient speciation

!! Variables for soil P and additional operations mjw
	allocate (min_res(mhru))
	
      !retention irrigation
      allocate(ri_sed(msub,10),ri_fr(msub,10),ri_dim(msub,10),           &
         ri_im(msub,10),ri_iy(msub,10),ri_sa(msub,10),ri_vol(msub,10),   &
         ri_qi(msub,10),ri_k(msub,10),ri_dd(msub,10),ri_evrsv(msub,10),  &
         ri_dep(msub,10),ri_ndt(msub,10),                                &
         ri_totpvol(time%step),ri_luflg(mhru),                           &
         ri_subkm(msub),ri_sed_cumul(msub,10),irmmdt(time%step),         &
         ri_pumpv(msub,10),ri_sedi(msub,10))   
      allocate(num_ri(msub), ri_pmpvol(10,time%step),hrnopcp(mhru,0:time%step), &
         ri_qloss(10,time%step))

      allocate(wtp_subnum(mhyd),wtp_onoff(mhyd),wtp_imo(mhyd),           &
        wtp_iyr(mhyd),wtp_dim(mhyd),wtp_stagdis(mhyd),wtp_sdtype(mhyd),  &
        wtp_pvol(mhyd),wtp_pdepth(mhyd),wtp_sdslope(mhyd),               &
        wtp_lenwdth(mhyd),wtp_extdepth(mhyd),wtp_hydeff(mhyd),           &
        wtp_evrsv(mhyd),wtp_sdintc(mhyd),wtp_sdexp(mhyd),wtp_sdc1(mhyd), &
        wtp_sdc2(mhyd),wtp_sdc3(mhyd),wtp_pdia(mhyd),wtp_plen(mhyd),     &
        wtp_pmann(mhyd),wtp_ploss(mhyd),wtp_k(mhyd),                     &
        wtp_dp(mhyd),wtp_sedi(mhyd),wtp_sede(mhyd),wtp_qi(mhyd))
                 	      
	!!for print out at daily, monthly, and annual scale
	allocate(sedc_d(mhru))
	allocate(surfqc_d(mhru))
	allocate(latc_d(mhru))
	allocate(percc_d(mhru))
	allocate(foc_d(mhru))
	allocate(NPPC_d(mhru))
	allocate(rsdc_d(mhru)) 
	allocate(grainc_d(mhru))
	allocate(stoverc_d(mhru))
	allocate(emitc_d(mhru))
	allocate(rspc_d(mhru))	   


       !Tillage factor on SOM decomposition
       allocate(tillage_switch(mhru))
       allocate(tillage_depth(mhru))
       allocate(tillage_days(mhru))
       allocate(tillage_factor(mhru))
       tillage_switch = 0
       tillage_depth = 0.
       tillage_days = 0
       tillage_factor = 0.
      !! By Zhang for C/N cycling
      !! ============================
      	  
      call zero0
      call zero1
      call zero2
      call zeroini
      call zero_urbn
     
!!    zero reservoir module
      return
      end