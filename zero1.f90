      subroutine zero1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use hru_module, only : bio_bod,biom,bz_perc,cn2,fcoli,hru,i_sep,ida_lup,manure_kg,nop,par,percp,   &
       plqm,pst_dep,pst_lag,qstemm,rbiom,sep_tsincefail,sno_hru,        &
       sol_cov,sweepeff,swtrg,t_ov,tconc,trapeff,usle,usle_cfac,   &
       usle_ei,usle_eifac,wfsh
      
      implicit none
      
      real :: bio_fecal                 !                |
      real :: sep_opt                   !                |
      real :: cont_cn                   !                |
      real :: cont_p                    !                | 
      real :: filt_w                    !                |
      real :: grwat_veg                 !                |
      real :: phu_op                    !                |
      real :: plq_rt                    !                |
      real :: pr_w                      !none            |probability of wet day after dry day in month 
      real :: pst_kg                    !kg/ha           |amount of pesticide applied to HRU
      real :: rchrg                     !mm              |recharge
      real :: resouty                   !                | 
      real :: rk1                       !1/day or 1/hr   |CBOD deoxygenation rate coeff in reach at 20 deg C
      real :: rk2                       !1/day or 1/hr   |reaeration rate in accordance with Fickian diffusion in reach at 20 deg C
      real :: rk3                       !1/day or 1/hr   |rate of loss of CBOD due to settling in reach at 20 deg C
      real :: rk4                       !mg O2/          |sed oxygen demand rate in reach ((m**2)*day)|at 20 deg C or mg O2/((m**2)*hr)
      real :: rk5                       !1/day           |coliform die-off rate in reach
      real :: rk6                       !1/day           |decay rate for arbitrary non-conservative constituent in reach
      real :: rsp_rt                    !                | 
      real :: slg_rt                    !                | 
      real :: sedst                     !metric tons     |amount of sediment stored in reach
                                        !                |reentrained in channel sediment routing
      real :: silt                      !%               |percent silt content in soil material
      real :: sol_pst                   !kg/ha           |pesticide in first layer of soil
      real :: sol_wp                    !                |
      real :: solarav                   !MJ/m^2/day      |average daily solar radiation for the month
      real :: solpstcnst                !                |
      real :: solpstmon                 !                |
      real :: solpstyr                  !                |
      real :: srbpstcnst                !                | 
      real :: srbpstmon                 !                | 
      real :: srbpstyr                  !                | 
      real :: strip_n                   !                | 
      real :: strip_cn                  !                | 
      real :: strip_c                   !                |  
      real :: strip_p                   !                | 
      real :: thalf                     !days            |time for the amount of solids on
                                        !                |impervious areas to build up to 1/2
                                        !                |the maximum level
      real :: tnconc                    !mg N/kg sed     |concentration of total nitrogen in
                                        !                |suspended solid load from impervious
                                        !                |areas
      real :: tno3conc                  !mg NO3-N/kg sed |concentration of NO3-N in suspended
                                        !                |solid load from impervious areas
      real :: tpconc                    !mg P/kg sed     |concentration of total phosphorus in
                                        !                |suspended solid load from impervious
                                        !                |areas 
      real :: urbcoef                   !1/mm            |wash-off coefficient for removal of
                                        !                |constituents from an impervious surface
      real :: urbcn2                    ! none           |moisture condiction II curve number for imp areas
      real :: vp                        !                |  
      real :: depsilfp                  !                |
      real :: depsagfp                  !                |
      real :: deplagfp                  !                |
      real :: depgrafp                  !                |
      real :: depsanch                  !                |
      real :: depsilch                  !                |
      real :: depclach                  !                |
      real :: depsagch                  !                |
      real :: deplagch                  !                |
      real :: depgrach                  !                |
      real :: sanst                     !                | 
      real :: silst                     !                |
      real :: clast                     !                |
      real :: sagst                     !                |
      real :: lagst                     !                |
      real :: grast                     !                | 

     
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
      sweepeff = 0.
      swtrg = 0
      t_ov = 0.
      tconc = 0.
      thalf = 0.
      tnconc = 0.
      tno3conc = 0.
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