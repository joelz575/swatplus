      subroutine sub_subbasin 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_wstr(:)   |none          |water stress factor which triggers auto
!!                                  |irrigation
!!    bio_e(:)       |(kg/ha)/      |biomass-energy ratio
!!                   |     (MJ/m**2)|The potential (unstressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    canev          |mm H2O        |amount of water evaporated from canopy
!!                                  |storage
!!    ep_day         |mm H2O        |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day         |mm H2O        |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    gw_q(:)        |mm H2O        |groundwater contribution to streamflow from
!!                                  |HRU on current day
!!    hru_ra(:)      |MJ/m^2        |solar radiation for the day in HRU
!!    iida           |julian date   |day being simulated (current julian date)
!!    inum1          |none          |subbasin number
!!    imp_trig(:)    |none          |release/impound action code:
!!                                  |0 begin impounding water
!!                                  |1 release impounded water
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    latq(:)        |mm H2O        |total lateral flow in soil profile for the
!!                                  |day in HRU
!!    nfert(:)       |none          |sequence number of fertilizer application
!!                                  |within the year
!!    nirr(:)        |none          |sequence number of irrigation application
!!                                  |within the year
!!    peakr          |m^3/s         |peak runoff rate
!!    pet_day        |mm H2O        |potential evapotranspiration on current
!!                                  |day in HRU
!!    phubase(:)     |heat units    |base zero total heat units (used when no
!!                                  |land cover is growing)
!!                                  |pesticide application occurs
!!    precipday      |mm H2O        |precipitation for the day in HRU
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    qtile          |mm H2O        |drainage tile flow in soil layer for the 
!!                                  |day
!!    sci(:)         |none          |retention coefficient for CN method based
!!                                  |on plant ET
!!    smx(:)         |none          |retention coefficient for CN method based
!!                                  |on soil moisture
!!    surfq(:)       |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo, the fraction of the solar radiation
!!                               |reflected at the soil surface back into
!!                               |space
!!    etday       |mm H2O        |actual evapotranspiration occuring on day
!!                               |in HRU
!!    ihru        |none          |HRU number
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    qdr(:)      |mm H2O        |total amount of water entering main channel
!!                               |for day from HRU
!!    sci(:)      |none          |retention coefficient for CN method based
!!                               |on plant ET
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d           |
!!    gma         |kPa/deg C     |psychrometric constant
!!    ho          |              |net radiation
!!    j           |none          |HRU number
!!    pet_alpha   |none          |alpha factor in Priestley-Taylor ET 
!!                               |equation
!!    tmpk        |deg K         |average temperature for the day in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: varinit, albedo, solt, surface, percmain, etpot, etact, fert
!!    SWAT: confert, graze, plantmod, nminrl, nitvol, pminrl, gwmod, apply
!!    SWAT: washp, decay, pestlch, enrsb, pesty, orgn, psed, nrain, nlch
!!    SWAT: solp, subwq, bacteria, urban, pothole, latsed, surfstor
!!    SWAT: substor, wetland, hrupond, irrsub, autoirr, watuse, watbal
!!    SWAT: sumv, virtual

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use climate_parms
      use time_module
      
      real :: hvol(10)
      integer :: sb
 
      sb = hru_sub(ihru)
      ri_pmpvol = 0; ri_totpvol = 0
      
      !continue counting no rainfall time from previous day
      hrnopcp(sb,0) = hrnopcp(sb,time%step)
        
      !! Count hours with no rainfall
      do ii = 1, time%step
        if(wst(iwst)%weat%ts(ii) / time%dtm > 0.017) then 
           !! effective pcp is > 0.017 mm/min (or 4/100 inches/hr)
           hrnopcp(sb,ii) = 0
           !! the potential for initial abstraction from paved surface is less than the user input initial abstraction
           abstinit = max(0.,abstinit - wst(iwst)%weat%ts(ii))
        else
           hrnopcp(sb,ii) = hrnopcp(sb,ii-1) + time%dtm / 60.
           !! the potential for initial abstraction from paved surface increases based on evaporation
  !         abstinit = min(bsn_cc%abstr,abstinit + pet_day / time%step)   <--commented line bc of 'bsn_cc%abstr' variables 020314 nbs
        end if
      end do
           
      if (sum(ri_fr(sb,:))>0) then
        hvol(1:num_ri(sb)) = ri_qi(sb,1:num_ri(sb))
        
        do ii = 1, time%step
           !! Estimate the amount irrigation
           !! Irrigation starts after 12 hours of no-rainfall period
           if (hrnopcp(sb,ii)<=12) then
             do kk=1,num_ri(sb)
	          hvol(kk) = hvol(kk) - ri_qloss(kk,1) - ri_qloss(kk,2) ! evaporation and transmission loss
                if (hvol(kk)<0) hvol(kk) = 0
             end do
           else
             do kk=1,num_ri(sb)
               !irrigate after the first 12hrs and before 72hrs
               ri_pmpvol(kk,ii) = min(hvol(kk),ri_pumpv(sb,kk))
                
               ! total amount of water that irrigate the subbasin
               ri_totpvol(ii) = ri_totpvol(ii) + ri_pmpvol(kk,ii)
               
               ! update water volume in the pond
               hvol(kk) = hvol(kk) - ri_pmpvol(kk,ii)                    & 
                          - ri_qloss(kk,1) - ri_qloss(kk,2)

               if (hvol(kk)<0) hvol(kk) = 0
             end do
           end if
           
       end do
      end if
      
      return
      end subroutine sub_subbasin