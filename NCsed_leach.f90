      subroutine orgncswat2(iwave)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic nitrogen removed in
!!    surface runoff - when using CSWAT==2 it 


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha           |area of watershed in hectares
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgn(:)     |none         |organic N enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    ihru          |none         |HRU number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    sub_bd(:)     |Mg/m^3       |bulk density in subbasin first soil layer
!!    sub_fr(:)     |none         |fraction of watershed area in subbasin
!!    sub_orgn(:)   |kg N/ha      |amount of nitrogen stored in all organic
!!    sedc_d(:)     |kg C/ha      |amount of C lost with sediment
!!
!!
!!     
!!                                |pools 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of organic N in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    xx          |kg N/ha       |amount of organic N in first soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use jrw_datalib_module
      use organic_mineral_mass_module

      integer, intent (in) :: iwave
      integer :: j
      real :: xx, wt1, er, conc
      real :: sol_mass, QBC, VBC, YBC, YOC, YW, TOT, YEW, X1, PRMT_21, PRMT_44
      real :: DK,  V, X3, CO, CS, perc_clyr, latc_clyr  
      integer :: k
      latc_clyr = 0.
        
      j = 0
      j = ihru
      
      xx = 0.
	  wt1 = 0.  !! conversion factor
      er = 0.	!! enrichment ratio
        !! HRU calculations
        !xx = sol_n(1,j) + sol_fon(1,j) + sol_mn(1,j)
        xx = rsd1(j)%str%n + rsd1(j)%meta%n + soil(j)%cbn(1)%hpn + soil(j)%cbn(1)%hsn
        !wt = sol_bd(1,j) * sol_z(1,j) * 10. (tons/ha)
        !wt1 = wt/1000
        wt1 = soil(j)%phys(1)%bd * soil(j)%phys(1)%d / 100.

        if (hru(j)%hyd%erorgn > .001) then
          er = hru(j)%hyd%erorgn
        else
          er = enratio
        end if

      conc = xx * er / wt1

      if (iwave <= 0) then
        !! HRU calculations
        sedorgn(j) = .001 * conc * sedyld(j) / hru(j)%area_ha
      else
        !! subbasin calculations
        sedorgn(j) = .001 * conc * sedyld(j) / (da_ha * sub_fr(iwave))
      end if

	!! update soil nitrogen pools only for HRU calculations
      if (iwave <= 0 .and. xx > 1.e-6) then
        xx1 = (1. - sedorgn(j) / xx)
        
        !!add by zhang to update soil nitrogen pools
        
		rsd1(j)%str%n = rsd1(j)%str%n * xx1
		rsd1(j)%meta%n = rsd1(j)%meta%n * xx1
		soil(j)%cbn(1)%hpn = soil(j)%cbn(1)%hpn * xx1
		soil(j)%cbn(1)%hsn = soil(j)%cbn(1)%hsn * xx1
		!sol_BMN(1,j) = sol_BMN(1,j) * xx1
      end if
      
      !return
      
      !Calculate runoff and leached C&N from micro-biomass
      latc_clyr = 0.
      sol_mass = 0.  
      !kg/ha
      sol_mass = (soil(j)%phys(1)%d / 1000.) * 10000. * soil(j)%phys(1)%bd * 1000. * (1- soil(j)%phys(1)%rock / 100.)
      
      
      QBC=0.    !c loss with runoff or lateral flow
      VBC=0.    !c los with vertical flow
      YBC=0.    !BMC LOSS WITH SEDIMENT
      YOC=0.    !Organic C loss with sediment
      YW=0.     !YW = WIND EROSION (T/HA)
      TOT = soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + rsd1(j)%meta%c + rsd1(j)%str%c !Total organic carbon in layer 1
      !YEW = MIN(er*(sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9)
      ! Not sure whether should consider enrichment ratio or not!
      YEW = MIN((sedyld(j)/hru(j)%area_ha+YW/hru(j)%area_ha)/(sol_mass/1000.),.9) !fraction of soil erosion of total soil mass
      X1=1.-YEW
	  !YEW=MIN(ER*(YSD(NDRV)+YW)/WT(LD1),.9)
	  !ER enrichment ratio
	  !YSD water erosion
	  !YW wind erosion
      YOC=YEW*TOT
      soil1(j)%hs(1)%c = soil1(j)%hs(1)%c * X1
      soil1(j)%hp(1)%c = soil1(j)%hp(1)%c * X1
      rsd1(j)%str%m = rsd1(j)%str%m * X1
      rsd1(j)%meta%m = rsd1(j)%meta%m * X1
      rsd1(j)%lig%m = rsd1(j)%lig%m * X1
      rsd1(j)%str%c = rsd1(j)%str%c * X1
      rsd1(j)%meta%c = rsd1(j)%meta%c * X1
      rsd1(j)%lig%c = rsd1(j)%lig%c * X1
      rsd1(j)%lig%c = rsd1(j)%str%c - rsd1(j)%lig%c
      if (surfq(j) > 0) then
        !write(*,*) 'stop'
      end if
      IF(soil(j)%cbn(1)%bmc > .01) THEN
          PRMT_21 = 0.  !KOC FOR CARBON LOSS IN WATER AND SEDIMENT(500._1500.) KD = KOC * C
          PRMT_21 = 1000.
          soil(j)%cbn(1)%woc = rsd1(j)%str%c + rsd1(j)%meta%c + soil1(j)%hp(1)%c + soil1(j)%hs(1)%c + soil(j)%cbn(1)%bmc 
          DK=.0001*PRMT_21*soil(j)%cbn(1)%woc
          !X1=PO(LD1)-S15(LD1)
          X1 = soil(j)%phys(1)%por*soil(j)%phys(1)%d-soil(j)%phys(1)%wpmm !mm
          IF (X1 <= 0.) THEN
            X1 = 0.01
          END IF
          XX=X1+DK
          !V=QD+Y4
          V = surfq(j) + soil(j)%ly(k)%prk + soil(j)%ly(1)%flat
	      !QD surface runoff
          X3=0.
          IF(V>1.E-10)THEN
              X3 = soil(j)%cbn(1)%bmc * (1.-EXP(-V/XX)) !loss of biomass C
              PRMT_44 = 0. !RATIO OF SOLUBLE C CONCENTRATION IN RUNOFF TO PERCOLATE(0.1_1.)
              PRMT_44 = .5
              CO=X3/(soil(j)%ly(k)%prk + PRMT_44*(surfq(j)+soil(j)%ly(1)%flat)) !CS is the horizontal concentration
              CS=PRMT_44*CO                                     !CO is the vertical concentration
              VBC=CO*(soil(j)%ly(k)%prk) 
              soil(j)%cbn(1)%bmc = soil(j)%cbn(1)%bmc - X3
              QBC=CS*(surfq(j)+soil(j)%ly(1)%flat)
        !     COMPUTE WBMC LOSS WITH SEDIMENT
              IF(YEW>0.)THEN
                  CS=DK*soil(j)%cbn(1)%bmc/XX
                  YBC=YEW*CS
              END IF
          END IF
      END IF

      soil(j)%cbn(1)%bmc=soil(j)%cbn(1)%bmc-YBC 
      surfqc_d(j) = QBC*(surfq(j)/(surfq(j)+soil(j)%ly(1)%flat+1.e-6))
       
      soil(j)%ly(1)%latc = QBC*(soil(j)%ly(1)%flat/(surfq(j)+soil(j)%ly(1)%flat+1.e-6))
      soil(j)%ly(1)%percc = VBC 
      sedc_d(j) = YOC + YBC
      
      latc_clyr = latc_clyr + soil(j)%ly(1)%latc   
      DO k = 2, soil(j)%nly
          if (soil(j)%ly(k)%prk > 0 .and. k == soil(j)%nly) then
            !write (*,*) 'stop'
          end if
          sol_thick = 0.
          sol_thick = soil(j)%phys(k)%d-soil(j)%phys(k-1)%d
          soil(j)%cbn(1)%woc = soil1(j)%str(k)%c + soil1(j)%meta(k)%c + soil1(j)%hp(k)%c + soil1(j)%hs(k)%c 
          Y1 = soil1(j)%microb(k)%c + VBC
          VBC=0.
          IF(Y1>=.01)THEN
              V=soil(j)%ly(k)%prk + soil(j)%ly(k)%flat
              IF(V>0.)VBC=Y1*(1.-EXP(-V/(soil(j)%phys(k)%por*sol_thick-soil(j)%phys(k)%wpmm+.0001*PRMT_21*soil(j)%cbn(1)%woc)))              
          END IF
          soil(j)%ly(k)%latc = VBC*(soil(j)%ly(k)%flat/(soil(j)%ly(k)%prk + soil(j)%ly(k)%flat+1.e-6))
          soil(j)%ly(k)%percc = VBC-soil(j)%ly(k)%latc
          soil1(j)%microb(k)%c = Y1 - VBC

        !! calculate nitrate in percolate        
        !perc_clyr = 0.
        perc_clyr = perc_clyr + soil(j)%ly(k)%percc
        
        latc_clyr = latc_clyr + soil(j)%ly(k)%latc
      END DO
     
        latc_d(j) = latc_clyr
        percc_d(j) = perc_clyr

      return
      end