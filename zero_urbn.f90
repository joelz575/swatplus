      subroutine zero_urbn

!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    this subroutine zeros all array values used in urban modeling

      use parm, only : cn2,dratio,dtp_addon,dtp_cdis,dtp_coef1,dtp_coef2,dtp_coef3,dtp_depweir,dtp_diaweir,     &
         dtp_evrsv,dtp_expont,dtp_flowrate,dtp_imo,dtp_intcept,dtp_ised,dtp_ivol,dtp_iyr,dtp_numstage,         &
         dtp_numweir,dtp_onoff,dtp_parm,dtp_pcpret,dtp_reltype,dtp_subnum,dtp_totwrwid,dtp_wdratio,dtp_weirdim,&
         dtp_weirtype,dtp_wrwid,ft_alp,ft_bpw,ft_dc,ft_dep,ft_dp,ft_fc,ft_fsa,ft_h,ft_k,ft_pd,ft_por,ft_qfg,   &
         ft_qin,ft_qout,ft_qpnd,ft_qsw,ft_sa,ft_sed_cumul,ft_sedpnd,hhsedy,hhsurf_bs,hrnopcp,init_abstrc,irmmdt, &
         nop,num_ri,num_sf,ovrlnd,ovrlnd_dt,par,ri_dd,ri_dep,ri_dim,ri_evrsv,ri_fr,ri_im,ri_iy,ri_k,ri_luflg,  &
         ri_ndt,ri_pumpv,ri_qi,ri_qloss,ri_sa,ri_sed,ri_sed_cumul,ri_sedi,ri_subkm,ri_vol,sci,sf_dim,          &
         sf_fr,sf_im,sf_iy,sf_ptp,sf_typ,sp_bpw,sp_dp,sp_k,sp_pd,sp_pvol,sp_qfg,sp_qi,sp_sa,                   &               
         sp_sed_cumul,sp_sede,sp_sedi,sub_cn2,sub_ha_imp,sub_ha_urb,subdr_ickm,surf_bs,tss_den,ubnrunoff,      &
         ubntss,wtp_dim,wtp_dp,wtp_evrsv,wtp_extdepth,wtp_hydeff,wtp_imo,wtp_iyr,wtp_lenwdth,wtp_onoff,        &
         wtp_pdepth,wtp_pdia,wtp_plen,wtp_ploss,wtp_pmann,wtp_pvol,wtp_qi,wtp_sdc1,wtp_sdc2,                   &
         wtp_sdc3,wtp_sdexp,wtp_sdintc,wtp_sdslope,wtp_sdtype,wtp_sede,wtp_sedi,wtp_stagdis
     
      
	sci = 0.

	hhsurf_bs = 0. 
      ubnrunoff = 0.
      ubntss = 0.
      abstinit = 0.
      
!!	subdaily sediment modeling by J.Jeong
	hhsedy=0.
	spl_eros = 0.
	dratio = 0.
	init_abstrc = 0
	
!!    subdaily bmp modeling
      !sed-fil
      num_sf = 0
      sf_fr = 0.
      sf_dim = 0
      sf_typ = 0.
      sf_im = 0.
      sf_iy = 0.
      sp_sa = 0.
      sp_pvol = 0.
      sp_qi = 0.
      sp_pd = 0.
      sp_bpw = 0.
      sp_k = 0.
      sp_sedi = 0.
      sp_sede = 0.
      sp_dp = 0.
      ft_sa = 0.
      ft_fsa = 0.
      ft_dep = 0.
      ft_h = 0.
      ft_pd = 0.
      ft_bpw = 0.
      ft_k = 0.
      ft_dp = 0.
      ft_dc = 0.
      ft_por = 0.4	
      tss_den = 0.
      ft_alp = 0.
      sub_ha_imp = 0.
      sub_ha_urb = 0.
	ft_qpnd = 0.
	ft_qsw = 0.
	ft_qin = 0.
	ft_qout = 0.
	ft_sedpnd = 0.
	ft_sed_cumul = 0.
	sp_sed_cumul = 0.
	ft_qfg = 0
	sp_qfg = 0
	sf_ptp = 0
	ft_fc = 0
	!detention pond
	dtp_subnum = 0
	dtp_imo = 0
	dtp_iyr = 0
	dtp_numweir = 0
	dtp_numstage = 0
	stp_stagdis = 0
	dtp_reltype = 0
	dtp_onoff = 0
	dtp_evrsv = 0.
	dtp_totwrwid = 0.
	dtp_parm = 0.
	dtp_intcept = 0.
	dtp_expont = 0.
	dtp_coef1 = 0.
	dtp_coef2 = 0.
	dtp_coef3 = 0.
	dtp_wdratio = 0.
	dtp_depweir = 0.
	dtp_diaweir = 0.
	dtp_pcpret = 0.
	dtp_cdis = 1.
	dtp_wrwid = 0.
	dtp_weirtype = 0.
	dtp_weirdim = 0.
	dtp_addon = 0.
	dtp_ivol = 0.
	dtp_ised = 0.
	dtp_flowrate = 0.
      !retention-irrigation
      ri_luflg(:) = 0
      ri_sed = 0.
      ri_sed = 0.
      ri_fr = 0.
      ri_dim = 0.
      ri_im = 0.
      ri_iy = 0.
      ri_sa = 0.
      ri_vol = 0.
      ri_qi = 0.
      ri_sedi = 0.
      ri_k = 0.
      ri_dd = 0.
      ri_evrsv = 0.
      ri_dep = 0.
      ri_ndt = 0.
      ri_subkm = 0.
      num_ri = 0
      hrnopcp = 100.
      ri_sed_cumul = 0.
      irmmdt = 0.
      subdr_kg = 0.
      subdr_ickm = 0.
      ri_qloss = 0
      ri_pumpv = 0
      
      !! wet pond
      wtp_onoff = 0
      wtp_imo = 0
      wtp_iyr = 0
      wtp_dim = 0
      wtp_stagdis = 0
      wtp_sdtype = 0
      wtp_pvol = 0.
      wtp_pdepth = 0.
      wtp_sdslope = 0.
      wtp_lenwdth = 0.
      wtp_extdepth = 0.
      wtp_hydeff = 0.
      wtp_evrsv = 0.
      wtp_sdintc = 0.
      wtp_sdexp = 0.
      wtp_sdc1 = 0.
      wtp_sdc2 = 0.
      wtp_sdc3 = 0.
      wtp_pdia = 0.
      wtp_plen = 0.
      wtp_pmann = 0.
      wtp_ploss = 0.
      sub_cn2 = 0.   
 	  wtp_dp = 0.
	  wtp_sedi = 0.
	  wtp_sede = 0.	 
	  wtp_qi = 0.
      ovrlnd_dt = 0.
	  
      return
      end