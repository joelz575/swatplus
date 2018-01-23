      subroutine sim_initday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initialized arrays at the beginning of the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
!!    sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
     
      use parm, only : cbodu,chl_a,clayld,cnday,dayl,doxq,etday,grayld,hhsedy,                     &
         hhsurfq,hru,hru_ra,hru_rmx,i_mo,irr_flag,lagyld,latno3,latq,npl,nplnt,                    &
         par,percn,petmeas,pot,pplnt,qdr,rcn,rhd,sagyld,sanyld,sedminpa,                           &
         sedminps,sedorgn,sedorgp,sedyld,sepbtm,silyld,soil,sol,sol_sumno3,sol_sumsolp,            &
         sub_etday,sub_hhsedy,sub_hhwtmp,sub_pet,sub_subp_dt,surfq,                                &
         surqno3,surqsolp,tileno3,tmn,tmpav,tmx,u10,ubnrunoff,ubntss
            
      use organic_mineral_mass_module
      use carbon_module
      use hydrograph_module, only : sp_ob

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
      sub_etday = 0.
      sub_hhwtmp = 0.
      sub_pet = 0.
      sub_dgra = 0.
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

      ! zero carbon losses for the day
      do j = 1, sp_ob%hru
        cbn_loss(j) = cbn_lossz
      end do
	
        !! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
                  
          sol_sumno3 = 0.
          sol_sumsolp = 0.
          do j = 1, sp_ob%hru
            do ly = 1, soil(j)%nly
              sol_sumno3(j) = sol_sumno3(j) + soil1(j)%mn(ly)%no3 +          &
                soil1(j)%mn(ly)%nh4
              sol_sumsolp(j) = sol_sumsolp(j) +  soil1(j)%mp(ly)%lab
            enddo
          enddo

      return
      end