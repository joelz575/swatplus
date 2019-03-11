      subroutine ch_watqual2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations

!!	adapted by Ann van Griensven, Belgium

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1          |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2          |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3          |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                |algal photosynthesis
!!    ai4          |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                                |respiration
!!    ai5          |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                |nitrogen oxidation
!!    ai6          |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                |nitrogen oxidation
!!    algae(:)     |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)  |mg N/L        |ammonia concentration in reach
!!    bc1(:)       |1/day         |rate constant for biological oxidation of NH3
!!                                |to NO2 in reach at 20 deg C
!!    bc2(:)       |1/day         |rate constant for biological oxidation of NO2
!!                                |to NO3 in reach at 20 deg C
!!    bc3(:)       |1/day         |rate constant for hydrolysis of organic N to
!!                                |ammonia in reach at 20 deg C
!!    bc4(:)       |1/day         |rate constant for the decay of organic P to
!!                                |dissolved P in reach at 20 deg C
!!    chlora(:)    |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    k_l          |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n          |mg N/L        |michaelis-menton half-saturation constant
!!                                |for nitrogen
!!    k_p          |mg P/L        |michaelis-menton half saturation constant
!!                                |for phosphorus
!!    lambda0      |1/m           |non-algal portion of the light extinction
!!                                |coefficient
!!    lambda1      |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2      |(1/m)(ug chla/L)**(-2/3)
!!                                |nonlinear algal self-shading coefficient
!!    mumax        |1/day         |maximum specific algal growth rate at 20 deg 
!!                                |C
!!    nitraten(:)  |mg N/L        |nitrate concentration in reach
!!    nitriten(:)  |mg N/L        |nitrite concentration in reach
!!    organicn(:)  |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)  |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_dox(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchdep       |m             |depth of flow on day
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rhoq         |1/day         |algal respiration rate at 20 deg C
!!    rk1(:)       |1/day         |CBOD deoxygenation rate coefficient in reach 
!!                                |at 20 deg C
!!    rk2(:)       |1/day         |reaeration rate in accordance with Fickian
!!                                |diffusion in reach at 20 deg C
!!    rk3(:)       |1/day         |rate of loss of CBOD due to settling in reach
!!                                |at 20 deg C
!!    rk4(:)       |mg O2/        |sediment oxygen demand rate in reach
!!                 |  ((m**2)*day)|at 20 deg C
!!    rs1(:)       |m/day         |local algal settling rate in reach at 20 deg
!!                                |C
!!    rs2(:)       |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                 |  ((m**2)*day)|in reach at 20 deg C
!!    rs3(:)       |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                 |  ((m**2)*day)|reach at 20 deg C
!!    rs4(:)       |1/day         |rate coefficient for organic nitrogen
!!                                |settling in reach at 20 deg C
!!    rs5(:)       |1/day         |organic phosphorus settling rate in reach at
!!                                |20 deg C
!!    rttime       |hr            |reach travel time
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    tfact        |none          |fraction of solar radiation computed in the
!!                                |temperature heat balance that is
!!                                |photosynthetically active
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algae(:)    |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:) |mg N/L        |ammonia concentration in reach
!!    chlora(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    nitraten(:) |mg N/L        |nitrate concentration in reach
!!    nitriten(:) |mg N/L        |nitrite concentration in reach
!!    organicn(:) |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_dox(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algi        |MJ/(m2*hr)    |daylight average, photosynthetically active,
!!                               |light intensity
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                               |to NO2 modified to reflect impact of low 
!!                               |oxygen concentration
!!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                               |to NO3 modified to reflect impact of low
!!                               |oxygen concentration
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    cinn        |mg N/L        |effective available nitrogen concentration
!!    cordo       |none          |nitrification rate correction factor
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    f1          |none          |fraction of algal nitrogen uptake from
!!                               |ammonia pool
!!    fl_1        |none          |growth attenuation factor for light, based on
!!                               |daylight-average light intensity
!!    fll         |none          |growth attenuation factor for light averaged
!!                               |over the diurnal cycle
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/day         |local algal growth rate at 20 deg C
!!    jrch        |none          |reach number
!!    lambda      |1/m           |light extinction coefficient
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in 
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    tday        |none          |flow duration (fraction of 24 hr)
!!    thbc1       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NH3 to NO2
!!    thbc2       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NO2 to NO3
!!    thbc3       |none          |temperature adjustment factor for local
!!                               |hydrolysis of organic N to ammonia N
!!    thbc4       |none          |temperature adjustment factor for local
!!                               |decay of organic P to dissolved P
!!    thgra       |none          |temperature adjustment factor for local algal
!!                               |growth rate
!!    thrho       |none          |temperature adjustment factor for local algal
!!                               |respiration rate
!!    thrk1       |none          |temperature adjustment factor for local CBOD
!!                               |deoxygenation
!!    thrk2       |none          |temperature adjustment factor for local oxygen
!!                               |reaeration rate
!!    thrk3       |none          |temperature adjustment factor for loss of
!!                               |CBOD due to settling
!!    thrk4       |none          |temperature adjustment factor for local
!!                               |sediment oxygen demand
!!    thrs1       |none          |temperature adjustment factor for local algal
!!                               |settling rate
!!    thrs2       |none          |temperature adjustment factor for local
!!                               |benthos source rate for dissolved phosphorus
!!    thrs3       |none          |temperature adjustment factor for local
!!                               |benthos source rate for ammonia nitrogen
!!    thrs4       |none          |temperature adjustment factor for local
!!                               |organic N settling rate
!!    thrs5       |none          |temperature adjustment factor for local
!!                               |organic P settling rate
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    uu          |varies        |variable to hold intermediate calculation
!!                               |result
!!    vv          |varies        |variable to hold intermediate calculation
!!                               |result
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use channel_data_module
      use channel_module
      use hydrograph_module, only : ob
      use climate_module

      implicit none
      
      integer :: icmd          !units         |description
      integer :: jrch          !none          |reach number
      integer :: iwst          !none          |counter
      integer :: iwgn          !units         |description
      real :: soxy             !mg O2/L       |saturation concetration of dissolved oxygen
      real :: chlin            !mg chl-a/L    |chlorophyll-a concentration in inflow
      real :: algin            !mg alg/L      |algal biomass concentration in inflow
      real :: orgnin           !mg N/L        |organic N concentration in inflow
      real :: ammoin           !mg N/L        |ammonium N concentration in inflow
      real :: nitratin         !mg N/L        |nitrate concentration in inflow
      real :: nitritin         !mg N/L        |nitrite concentration in inflow
      real :: orgpin           !mg P/L        |organic P concentration in inflow
      real :: dispin           !mg P/L        |soluble P concentration in inflow
      real :: cbodin           !mg/L          |carbonaceous biological oxygen demand
      real :: disoxin          !mg O2/L       |dissolved oxygen concentration in inflow
      real :: tday             !none          |flow duration (fraction of 24 hr)
      real :: wtmp             !deg C         |temperature of water in reach
      real :: fll              !none          |growth attenuation factor for light averaged
      real :: gra              !1/day         |local algal growth rate at 20 deg C
      real :: lambda           !1/m           |light extinction coefficient
      real :: fnn              !none          |algal growth limitation factor for nitrogen
      real :: fpp              !none          |algal growth limitation factor for phosphorus
      real :: algi             !MJ/(m2*hr)    |daylight average, photosynthetically active,
                               !              |light intensity
      real :: fl_1             !none          |growth attenuation factor for light, based on
                               !              |daylight-average light intensity
      real :: xx               !varies        |variable to hold intermediate calculation result 
      real :: yy               !varies        |variable to hold intermediate calculation result
      real :: zz               !varies        |variable to hold intermediate calculation result
      real :: ww               !varies        |variable to hold intermediate calculation result
      real :: cinn             !mg N/L        |effective available nitrogen concentration
      real :: heatin           !units         |description
      real :: vv               !varies        |variable to hold intermediate calculation result
      real :: cordo            !none          |nitrification rate correction factor
      real :: f1               !none          |fraction of algal nitrogen uptake from
                               !              |ammonia pool
      real :: algcon           !mg alg/L      |initial algal biomass concentration in reach
      real :: orgncon          !mg N/L        |initial organic N concentration in reach
      real :: nh3con           !mg N/L        |initial ammonia concentration in reach
      real :: no2con           !mg N/L        |initial nitrite concentration in reach
      real :: no3con           !mg N/L        |initial nitrate concentration in reach
      real :: orgpcon          !mg P/L        |initial organic P concentration in reach
      real :: solpcon          !mg P/L        |initial soluble P concentration in reach
      real :: cbodcon          !mg/L          |initial carbonaceous biological oxygen demand
      real :: o2con            !mg O2/L       |initial dissolved oxygen concentration in 
      real :: wtrtot           !m^3 H2O       |inflow + storage water
      real :: bc1mod           !1/day         |rate constant for biological oxidation of NH3
      real :: bc2mod           !1/day         |rate constant for biological oxidation of NO2
      real :: thgra = 1.047    !none          |temperature adjustment factor for local algal growth rate
      real :: thrho = 1.047    !none          |temperature adjustment factor for local algal respiration rate
      real :: thrs1 = 1.024    !none          |temperature adjustment factor for local algal settling rate
      real :: thrs2 = 1.074    !none          |temperature adjustment factor for local
                               !              |benthos source rate for dissolved phosphorus
      real :: thrs3 = 1.074    !none          |temperature adjustment factor for local
                               !              |benthos source rate for ammonia nitrogen
      real :: thrs4 = 1.024    !none          |temperature adjustment factor for local
                               !              |organic N settling rate
      real :: thrs5 = 1.024    !none          |temperature adjustment factor for local
                               !              |organic P settling rate      
      real :: thbc1 = 1.083    !none          |temperature adjustment factor for local
                               !              |biological oxidation of NH3 to NO2
      real :: thbc2 = 1.047    !temperature adjustment factor for local
                               !              |biological oxidation of NO2 to NO3
      real :: thbc3 = 1.047    !none          |temperature adjustment factor for local
                               !              |hydrolysis of organic N to ammonia N
      real :: thbc4 = 1.047    !none          |temperature adjustment factor for local
                               !              |decay of organic P to dissolved P
      real :: thrk1 = 1.047    !none          |temperature adjustment factor for local CBOD
                               !              |deoxygenation
      real :: thrk2 = 1.024    !none          |temperature adjustment factor for local oxygen
                               !              |reaeration rate
      real :: thrk3 = 1.024    !none          |temperature adjustment factor for loss of
                               !              |CBOD due to settling
      real :: thrk4 = 1.060    !none          |temperature adjustment factor for local
                               !              |sediment oxygen demand 
	  real :: dalgae           !units         |description
      real :: dchla            !units         |description
      real :: dorgn            !units         |description
      real :: dnh4             !units         |description
      real :: dno2             !units         |description
      real :: dno3             !units         |description
      real :: dorgp            !units         |description
      real :: dsolp            !units         |description
      real :: dbod             !units         |description
      real :: ddisox           !units         |description
      real :: rchwtr           !m^3 H2O       |water stored in reach at beginning of day 
      real :: o2con2           !units         |description
      real :: setl             !units         |description
      real :: theta            !units         |description
      real :: uu               !varies        |variable to hold intermediate calculation
                               !              |result
      real :: hh               !units         |description
      real :: o2proc           !units         |description                       

       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = ob(icmd)%hin%flo 

       if (rtwtr / 86400.> 0.01.and.wtrin>0.001) then
!! concentrations

         !! initialize concentration of nutrient in reach
         wtrtot = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         wtrtot = wtrin + rchwtr

         algcon = ch(jrch)%algae
         orgncon = ch(jrch)%organicn
         nh3con = ch(jrch)%ammonian
         no2con = ch(jrch)%nitriten
         no3con = ch(jrch)%nitraten
         orgpcon =  ch(jrch)%organicp
         solpcon = ch(jrch)%disolvp
         cbodcon = ch(jrch)%rch_cbod
         o2con  = ch(jrch)%rch_dox
	   wtmp = ch(jrch)%wattemp

!	write(104,*) "t",jrch,disoxin, wtrin, rch_dox(jrch)	
!         o2con = (disoxin * wtrin + rch_dox(jrch) * rchwtr) / wtrtot

         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13

         if (wtmp <= 0.) wtmp = 0.1

         !! calculate effective concentration of available nitrogen
         !! QUAL2E equation III-15
         cinn = nh3con + no3con

         !! calculate saturation concentration for dissolved oxygen
         !! QUAL2E section 3.6.1 equation III-29
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
         xx = 6.642308e07 / ((wtmp + 273.15)**2)
         yy = 1.243800e10 / ((wtmp + 273.15)**3)
         zz = 8.621949e11 / ((wtmp + 273.15)**4)
         soxy = Exp(ww - xx + yy - zz)
         if (soxy < 1.e-6) soxy = 0. 
!! end initialize concentrations

!! O2 impact calculations
        !! calculate nitrification rate correction factor for low
        !! oxygen QUAL2E equation III-21
        cordo = 0.
!       write(104, *) o2con, "o"
	o2con2=o2con
	if (o2con2.le.0.1) o2con2=0.1
	if (o2con2.gt.30.) o2con2=30.
        cordo = 1.0 - Exp(-0.6 * o2con2)
!       write(104, *) cordo, "cordo"
	  	if (o2con.le.0.001) o2con=0.001
	if (o2con.gt.30.) o2con=30.
        cordo = 1.0 - Exp(-0.6 * o2con)

        !! modify ammonia and nitrite oxidation rates to account for
        !! low oxygen
        bc1mod = 0.
        bc2mod = 0.
        bc1mod = ch_nut(jnut)%bc1 * cordo
        bc2mod = ch_nut(jnut)%bc2 * cordo
!! end O2 impact calculations

         
!	tday is the calculation time step = 1 day

         tday = 1.0

!! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ch_nut(jnut)%ai0 * algcon > 1.e-6) then
           lambda = ch_nut(jnut)%lambda0 + (ch_nut(jnut)%lambda1 *           &
             ch_nut(jnut)%ai0 * algcon)                                      &
             + ch_nut(jnut)%lambda2 * (ch_nut(jnut)%ai0 *                    &
             algcon) ** (.66667)
         else
           lambda = ch_nut(jnut)%lambda0
         endif

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.
         fpp = 0.
         fnn = cinn / (cinn + ch_nut(jnut)%k_n)
         fpp = solpcon / (solpcon + ch_nut(jnut)%k_p)

         !! calculate daylight average, photosynthetically active,
         !! light intensity QUAL2E equation III-8
         !! Light Averaging Option # 2
         iwgn = wst(iwst)%wco%wgn
         if (wgn_pms(iwgn)%daylth > 0.) then
           algi = wst(iwst)%weat%solrad * ch_nut(jnut)%tfact /           &
              wgn_pms(iwgn)%daylth
         else
           algi = 0.00001
         end if

         !! calculate growth attenuation factor for light, based on
         !! daylight average light intensity QUAL2E equation III-7b
         fl_1 = 0.
         fll = 0.
         fl_1 = (1. / (lambda * rchdep)) *                              &                         
             Log((ch_nut(jnut)%k_l + algi) / (ch_nut(jnut)%k_l + algi * &
             (Exp(-lambda * rchdep))))
         fll = 0.92 * (wgn_pms(iwgn)%daylth / 24.) * fl_1

         !! calculcate local algal growth rate
         gra = 0.
         select case (ch_nut(jnut)%igropt)
           case (1)
             !! multiplicative QUAL2E equation III-3a
             gra = ch_nut(jnut)%mumax * fll * fnn * fpp
           case (2)
             !! limiting nutrient QUAL2E equation III-3b
             gra = ch_nut(jnut)%mumax * fll * Min(fnn, fpp)
           case (3)
             !! harmonic mean QUAL2E equation III-3c
             if (fnn > 1.e-6 .and. fpp > 1.e-6) then
               gra = ch_nut(jnut)%mumax * fll * 2. /                     &
                  ((1. / fnn) + (1. / fpp))
             else
               gra = 0.
             endif
         end select

    
         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         dalgae = 0.
	 setl=min(1.,Theta(ch_nut(jnut)%rs1,thrs1,wtmp)/ rchdep)
         dalgae = algcon + (Theta(gra,thgra,wtmp) * algcon -            &           
         Theta(ch_nut(jnut)%rhoq,thrho,wtmp) * algcon - setl *          &
         algcon) * tday
         if (dalgae < 0.00001) ch(jrch)%algae = 0.00001

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1

         dchla = 0.
         dchla = dalgae* ch_nut(jnut)%ai0 / 1000.
!! end algal growth 

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0.
         yy = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon
         zz = Theta(ch_nut(jnut)%rk3,thrk3,wtmp) * cbodcon
         dbod = 0.
         dbod = cbodcon - (yy + zz) * tday
         if (dbod < 0.00001) dbod = 0.00001

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
  
	   uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
	   hh=Theta(ch_nut(jnut)%rk2,thrk2,wtmp) 
         uu = Theta(ch_nut(jnut)%rk2,thrk2,wtmp) * (soxy - o2con)
         if (algcon.gt.0.001) then                                
	                vv = (ch_nut(jnut)%ai3 * Theta(gra,thgra,wtmp) -          & 
         ch_nut(jnut)%ai4 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp)) *            &
         algcon
  
	   else
	   algcon=0.001
	   end if
         ww = Theta(ch_nut(jnut)%rk1,thrk1,wtmp) * cbodcon

         if(rchdep.gt.0.001) xx = Theta(ch_nut(jnut)%rk4,thrk4,wtmp)      &
     	   / (rchdep * 1000.)
         if (nh3con.gt.0.001) then
	   yy = ch_nut(jnut)%ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con
         else
	   nh3con=0.001
	   end if
	   if (no2con.gt.0.001) then
	   zz = ch_nut(jnut)%ai6 * Theta(bc2mod,thbc2,wtmp) * no2con
         else
	   no2con=0.001
	   end if
         ddisox = o2con + (uu + vv - ww - xx - yy - zz) * tday
		o2proc=o2con-ddisox
         if (ddisox < 0.00001) ddisox = 0.00001

 
!! end oxygen calculations

!! nitrogen calculations
         !! calculate organic N concentration at end of day
         !! QUAL2E section 3.3.1 equation III-16
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ch_nut(jnut)%ai1 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *    &
             algcon
         yy = Theta(ch_nut(jnut)%bc2,thbc3,wtmp) * orgncon
         zz = Theta(ch_nut(jnut)%rs4,thrs4,wtmp) * orgncon
         dorgn = 0.
         dorgn = orgncon + (xx - yy - zz) * tday
         if (dorgn < 0.00001) dorgn = 0.00001


        !! calculate fraction of algal nitrogen uptake from ammonia
        !! pool QUAL2E equation III-18
        f1 = 0
        f1 = ch_nut(jnut)%p_n * nh3con / (ch_nut(jnut)%p_n * nh3con +      &
             (1. - ch_nut(jnut)%p_n) * no3con + 1.e-6)

        !! calculate ammonia nitrogen concentration at end of day
        !! QUAL2E section 3.3.2 equation III-17
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = Theta(ch_nut(jnut)%bc2,thbc3,wtmp) * orgncon
        xx = Theta(bc1mod,thbc1,wtmp) * nh3con
        yy = Theta(ch_nut(jnut)%rs3,thrs3,wtmp) / (rchdep * 1000.)
        zz = f1 * ch_nut(jnut)%ai1 * algcon * Theta(gra,thgra,wtmp)
        dnh4  = 0.
        dnh4  = nh3con + (ww - xx + yy - zz) * tday
        if (dnh4  < 1.e-6) dnh4  = 0.

         !! calculate concentration of nitrite at end of day
        !! QUAL2E section 3.3.3 equation III-19
        yy = 0.
        zz = 0.
        yy = Theta(bc1mod,thbc1,wtmp) * nh3con
        zz = Theta(bc2mod,thbc2,wtmp) * no2con
        dno2 = 0.
        dno2 = no2con + (yy - zz) * tday
        if (dno2 < 1.e-6) dno2 = 0.

        !! calculate nitrate concentration at end of day
        !! QUAL2E section 3.3.4 equation III-20
        yy = 0.
        zz = 0.
        yy = Theta(bc2mod,thbc2,wtmp) * no2con
        zz = (1. - f1) * ch_nut(jnut)%ai1 * algcon *                     &
                              Theta(gra,thgra,wtmp)
        dno3  = 0.
        dno3  = no3con + (yy - zz) * tday
        if (dno3 < 1.e-6) dno3  = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day QUAL2E section 3.3.6 equation III-24
        xx = 0.
        yy = 0.
        zz = 0.
        xx = ch_nut(jnut)%ai2 * Theta(ch_nut(jnut)%rhoq,thrho,wtmp) *     &
           algcon
        yy = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        zz = Theta(ch_nut(jnut)%rs5,thrs5,wtmp) * orgpcon
        dorgp = 0.
        dorgp= orgpcon + (xx - yy - zz) * tday
        if (dorgp < 1.e-6) dorgp = 0.

        !! calculate dissolved phosphorus concentration at end
        !! of day QUAL2E section 3.4.2 equation III-25
        xx = 0.
        yy = 0.
        zz = 0.
        xx = Theta(ch_nut(jnut)%bc4,thbc4,wtmp) * orgpcon
        yy = Theta(ch_nut(jnut)%rs2,thrs2,wtmp) / (rchdep * 1000.)
        zz = ch_nut(jnut)%ai2 * Theta(gra,thgra,wtmp) * algcon
        dsolp  = 0.
        dsolp  = solpcon + (xx + yy - zz) * tday
        if (dsolp  < 1.e-6) dsolp  = 0.

!! end phosphorus calculations

         wtrtot = wtrin + rchwtr
         !! initialize inflow concentrations
         chlin = 0.
         algin = 0.
         orgnin = 0.
         ammoin = 0.
         nitritin = 0.
         nitratin = 0.
         orgpin = 0.
         dispin = 0.
         cbodin = 0.
         disoxin = 0.
         cinn = 0.

         if (wtrin > 0.001) then
         chlin = 1000. * ob(icmd)%hin%chla  / wtrin
         algin = 1000. * chlin / ch_nut(jnut)%ai0        !! QUAL2E equation III-1
         orgnin = 1000. * ob(icmd)%hin%orgn  / wtrin
         ammoin = 1000. * ob(icmd)%hin%nh3  / wtrin
         nitritin = 1000. * ob(icmd)%hin%no2  / wtrin
         nitratin = 1000. * ob(icmd)%hin%no3  / wtrin
         orgpin = 1000. * ob(icmd)%hin%sedp  / wtrin
         dispin = 1000. * ob(icmd)%hin%solp  / wtrin
         cbodin = 1000. * ob(icmd)%hin%cbod  / wtrin
         disoxin= 1000. * ob(icmd)%hin%dox  / wtrin
	     heatin = ob(icmd)%hin%temp     
         end if


	   ch(jrch)%wattemp =(heatin * wtrin + wtmp * rchwtr) / wtrtot
         ch(jrch)%algae = (algin * wtrin + dalgae * rchwtr) / wtrtot
         ch(jrch)%organicn = (orgnin * wtrin + dorgn * rchwtr) / wtrtot
         ch(jrch)%ammonian = (ammoin * wtrin +  dnh4 * rchwtr) / wtrtot
         ch(jrch)%nitriten = (nitritin * wtrin + dno2  * rchwtr) / wtrtot
         ch(jrch)%nitraten = (nitratin * wtrin + dno3  * rchwtr) / wtrtot
         ch(jrch)%organicp = (orgpin * wtrin +  dorgp * rchwtr) / wtrtot
         ch(jrch)%disolvp = (dispin * wtrin +  dsolp * rchwtr) / wtrtot
         ch(jrch)%rch_cbod = (cbodin * wtrin + dbod * rchwtr) / wtrtot
         ch(jrch)%rch_dox =(disoxin * wtrin +  ddisox * rchwtr) / wtrtot

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         ch(jrch)%chlora = 0.
         ch(jrch)%chlora = ch(jrch)%algae * ch_nut(jnut)%ai0 / 1000.

      else
        !! all water quality variables set to zero when no flow
        algin = 0.0
        chlin = 0.0
        orgnin = 0.0
        ammoin = 0.0
        nitritin = 0.0
        nitratin = 0.0
        orgpin = 0.0
        dispin = 0.0
        cbodin = 0.0
        disoxin = 0.0
        ch(jrch)%algae = 0.0
        ch(jrch)%chlora = 0.0
        ch(jrch)%organicn = 0.0
        ch(jrch)%ammonian = 0.0
        ch(jrch)%nitriten = 0.0
        ch(jrch)%nitraten = 0.0
        ch(jrch)%organicp = 0.0
        ch(jrch)%disolvp = 0.0
        ch(jrch)%rch_cbod = 0.0
        ch(jrch)%rch_dox = 0.0
	     dalgae = 0.0
         dchla = 0.0
         dorgn = 0.0
         dnh4 = 0.0
         dno2 = 0.0
         dno3 = 0.0
         dorgp= 0.0
         dsolp = 0.0
         dbod = 0.0
         ddisox = 0.0   
        soxy = 0.0
      endif

      return
      end subroutine ch_watqual2