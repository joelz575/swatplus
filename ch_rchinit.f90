      subroutine ch_rchinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    channel routing command loop

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in whic HRU/reach is located
!!    ihout       |none          |outflow hydrograph storage location number
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    sub_pet(:)  |mm H2O        |potential evapotranspiration for day in
!!                               |subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    hdepth(:)   |m             |depth of flow during hour
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
!!    hhtime(:)   |hr            |travel time of flow in reach for hour
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of hour
!!    hrtwtr(:)   |m^3           |water leaving reach in hour
!!    hsdti(:)    |m^3/s         |flow rate in reach for hour
!!    peakr       |m^3/s         |peak rate of flow in channel
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    qdbank      |m^3 H2O       |streamflow contribution from bank storage
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rchwtr      |m^3 H2O       |water stored in reach at beginning of day
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |flow rate in reach for day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    soxy        |mg O2/L       |saturation oxygen concentration in water
!!    volatpst    |mg pst        |amount of pesticide lost from reach by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    kk          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!! initialize daily variables
      rchwtr = ch(jrch)%rchstor
      
      bury = 0.
      difus = 0.
!      halgae = 0.
!      hbod = 0.
!      hchla = 0.
!      hdepth = 0.
!      hdisox = 0.
!      hharea = 0.
!      hhstor = 0.
!      hhtime = 0.
!      hnh4 = 0.
!      hno2 = 0.
!      hno3 = 0.
!      horgn = 0.
!      horgp = 0.
!      hrchwtr = 0.
!      hrtwtr = 0.
!      hsdti = 0.
!      hsedst = 0.
!      hsedyld = 0.
!      hsolp = 0.
!      hsolpst = 0.
!      hsorpst = 0.
      peakr = 0.
      qdbank = 0.
      rcharea = 0.
      rchdep = 0.
      reactb = 0.
      reactw = 0.
      revapday = 0.
      resuspst = 0.
      rtevp = 0.
      rttime = 0.
      rttlc = 0.
      rtwtr = 0.
      sdti = 0.
      sedrch = 0.
      setlpst = 0.
      solpesto = 0.
      sorpesto = 0.
      soxy = 0.
      volatpst = 0.
      ch(jrch)%vel_chan = 0.
      sedrch = 0.
      rch_san = 0.
      rch_sil = 0.
      rch_cla = 0.
      rch_sag = 0.
      rch_lag = 0.
      rch_gra = 0.

      return
      end subroutine ch_rchinit