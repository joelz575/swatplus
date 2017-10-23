      subroutine varinit
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the 
!!    land phase of the hydrologic cycle (the subbasin command loop)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ihru        |none          |HRU number
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    al5         |none          |fraction of total rainfall that occurs
!!                               |during 0.5h of highest intensity rain
!!    albday      |none          |albedo for the day in HRU
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bioday      |kg            |biomass generated on current day in HRU
!!    bsprev      |mm H2O        |surface runoff lagged from prior day of
!!                               |simulation
!!    canev       |mm H2O        |amount of water evaporated from canopy storage
!!    cfertn      |kg N/ha       |amount of nitrogen added to soil in continuous
!!                               |fertilizer operation on day
!!    cfertp      |kg P/ha       |amount of phosphorus added to soil in continuous
!!                               |fertilizer operation on day
!!    crk         |mm H2O        |percolation due to crack flow
!!    enratio     |none          |enrichment ratio calculated for day in HRU
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et)
!!                               |that can occur on day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that 
!!                               |occurs on day in HRU
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on 
!!                               |current day
!!    hhqday(:)   |mm H2O        |surface runoff from HRU for every hour in day
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into
!!                               |soil (enters soil)
!!    lat_pst(:)  |kg pst/ha     |amount of pesticide in lateral flow in HRU for
!!                               |the day
!!    latlyr      |mm H2O        |amount of water in lateral flow in layer in 
!!                               |HRU for the day
!!    peakr       |m^3/s         |peak runoff rate for the day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel for 
!!                               |day in HRU
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow 
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    sepday      |mm H2O        |percolation from bottom of the soil layer on
!!                               |day in HRU
!!    snoev       |mm H2O        |amount of water in snow lost through 
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing 
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in 
!!                               |HRU
!!    sol_rd      |mm            |current rooting depth
!!    soxy        |mg/L          |saturation dissolved oxygen concentration
!!    sw_excess   |mm H2O        |amount of water in soil that exceeds field 
!!                               |capacity (gravity drained water)
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!                               |via transmission losses on day in HRU
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    usle_ei     |none          |USLE erodibility index on day for HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    ly          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use time_module
      use parm, only : soil, hhqday, ihru, al5, albday, auton, autop, bactrolp, bactrop, bactsedlp, bactsedp,  &
        bioday, bsprev, canev, cfertn, cfertp, ep_day, ep_max, es_day, fertn, fertp, grazn, grazp, gwseep,     &
        hhsedy, hmntl, hmptl, inflpcp, inflrout, irmmdt, latlyr, latqrunon, ls_overq, lyrtile, no3pcp, peakr,  &
        pet_day, qday, qtile, revapday, rmn2tl, rmp1tl, rmptl, roctl, rwntl, sepday, snoev, snofall, snomlt,   &
        sol_rd, soxy, sw_excess, tloss, ubnrunoff, ubntss, uno3d, usle, usle_ei, voltot, vpd, wdntl  

      integer :: j, ly

      j = ihru

      !! initialize hru variables - modular code
      do ly = 1, soil(j)%nly
        soil(j)%ly(ly)%prk = 0.
        soil(j)%ly(ly)%flat = 0.
      end do
      
      !!initialize variables NUBS - all these need to be checked
        al5 = 0.
        albday = 0.
        auton = 0.
        autop = 0.
        bactrolp = 0.
        bactrop = 0.
        bactsedlp = 0.
        bactsedp = 0.
        bioday = 0.
        bsprev = 0.
        canev = 0.
        cfertn = 0.
        cfertp = 0.
        crk = 0.
        enratio = 0.
        ep_day = 0.
        ep_max = 0.
        es_day = 0.
        etday = 0.
        fertn = 0.
        fertp = 0.
        fixn = 0.
        grazn = 0.
        grazp = 0.
        gwseep = 0.
        if (time%step > 0)  hhqday(j,:) = 0.
        hmntl = 0.
        hmptl = 0.
        inflpcp = 0.
        inflrout = 0.
        latlyr = 0.
        lyrtile = 0.
        no3pcp = 0.
        peakr = 0.
        pet_day = 0.
        qday = 0.
        qtile = 0.
        ls_overq = 0.
        over_flow = 0.
        latqrunon = 0.
        revapday = 0.
        rmn2tl = 0.
        rmp1tl = 0.
        rmptl = 0.
        roctl = 0.
        rwntl = 0.
        sepday = 0.
        snoev = 0.
        snofall = 0.
        snomlt = 0.
        sol_rd = 0.
        soxy = 0.
        sw_excess = 0.
        tloss = 0.
        uno3d = 0.
        usle = 0.
        usle_ei = 0.
        vpd = 0.
        voltot = 0.
        wdntl = 0.
        
	!! urban modeling by J.Jeong
	  sedprev = 0.
	  ubnrunoff = 0.
	  irmmdt = 0.
        hhsedy = 0.
        ubntss = 0.
        
        yield = 0.

       return
       end subroutine varinit