      subroutine sim_initday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initialized arrays at the beginning of the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i_mo        |none          |current month being simulated
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hr            |day length for the day in HRU
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!                               |hour in day in HRU
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto 
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |pesticide loading from HRU in the water phase
!!    wst(:)%weat%ts(:)  |mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    hrupstd(:,:,:)|varies      |HRU daily pesticide output array
!!    sub_bd(:)   |Mg/m^3        |average bulk density for subbasin
!!    sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_orgn(:) |kg N/ha       |organic nitrogen in soil of subbasin
!!    sub_pst(:,:)|kg/ha         |pesticide in soil of subbasin
!!    sub_surfq(:)|mm H2O        |surface runoff generated on day in subbasin
!!    sub_wyld(:) |mm H2O        |water yield on day in subbasinn
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use organic_mineral_mass_module

      !!initialize variables at beginning of day
      cbodu = 0.
      chl_a = 0.
      cnday = 0.
      dayl = 0.
      doxq = 0.
      drift = 0.
      hru_ra = 0.
      hru_rmx = 0.
      hrupstd = 0.
	  irr_flag = 0
      latno3 = 0.
      latq = 0.
      nplnt = 0.
      percn = 0.
      petmeas = 0.
      pplnt = 0.
      qdr = 0.
      rhd = 0.
      sedminpa = 0.
      sedminps = 0.
      sedorgn = 0.
      sedorgp = 0.

      sedyld = 0.
      sanyld = 0.
      silyld = 0.
      clayld = 0.
      sagyld = 0.
      lagyld = 0.
      grayld = 0.

      sepbtm = 0.
      sub_bd = 0.
      sub_etday = 0.
      sub_hhwtmp = 0.
      sub_orgn = 0.
      sub_pet = 0.
      sub_pst = 0.
	sub_dgra = 0.
      sub_surfq = 0.
      sub_wyld = 0.
      surfq = 0.
      surqno3 = 0.
      surqsolp = 0.
      tileno3 = 0.    !CB 8/24/09
      tmn = 0.
      tmpav = 0.
      tmx = 0.
      u10 = 0.

!----------------------------------------------------        
! added by J.Jeong for urban modeling 4/29/2008
      ubnrunoff = 0.
      ubntss = 0.
      latq = 0.
	  sub_subp_dt = 0.
	  sub_hhsedy = 0.
      hhsurfq = 0.
!-----------------------------------------------------        

      !!add by zhang
      !!==========================
        sedc_d = 0.
        surfqc_d =0.
        latc_d = 0.
        percc_d = 0.
        foc_d = 0.
        NPPC_d = 0.
        rsdc_d = 0. 
        grainc_d = 0.
        stoverc_d = 0.
        emitc_d = 0.
        rspc_d = 0.   
	sub_NEPC_d=0.
	sub_stover_c_d=0.
	sub_emit_c_d=0.
      !!add by zhang
      !!==========================
	
        !! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
                  
          sol_sumno3 = 0.
          sol_sumsolp = 0.
          do j = 1, mhru
            do ly = 1, soil(j)%nly
              sol_sumno3(j) = sol_sumno3(j) + soil1(j)%mn(ly)%no3 +          &
                soil1(j)%mn(ly)%nh4
              sol_sumsolp(j) = sol_sumsolp(j) +  soil1(j)%mp(ly)%lab
            enddo
          enddo

      return
      end