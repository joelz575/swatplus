      subroutine pl_pupd
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus demand

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_p1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    bio_p2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    ihru        |none           |HRU number
!!    nro(:)      |none           |sequence number of year in rotation
!!    pltpfr(1,:) |kg P/kg biomass|phosphorus uptake parameter #1: normal
!!                                |fraction of P in crop biomass at emergence
!!    pltpfr(3,:) |kg P/kg biomass|phosphorus uptake parameter #3: normal
!!                                |fraction of P in crop biomass at maturity
!!    uobp        |none           |phosphorus uptake normalization parameter
!!                                |This variable normalizes the phosphorus
!!                                |uptake so that the model can easily verify
!!                                |that uptake from the different soil layers
!!                                |sums to 1.0
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~    
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |mm            |lowest depth in layer from which phosphorus
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag for bottom of root zone
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layers)
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    uapl        |kg P/ha       |amount of phosphorus removed from layer
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module, only : pldb, plcp
      use parm, only : pcom, up2, uapd, ihru, ipl, idp 

      integer :: j, icrop, l, ir
      real :: uapl, gx

      j = 0
      j = ihru

      idp = pcom(j)%plcur(ipl)%idplt
      
      pcom(j)%plm(ipl)%p_fr = (pldb(idp)%pltpfr1 - pldb(idp)%pltpfr3) * &  
        (1. - pcom(j)%plcur(ipl)%phuacc / (pcom(j)%plcur(ipl)%phuacc +  &       
        Exp(plcp(idp)%pup1 - plcp(idp)%pup2 *                           &
        pcom(j)%plcur(ipl)%phuacc))) + pldb(idp)%pltpfr3

      up2(ipl) = 0.
      uapd(ipl) = 0.
      up2(ipl) = pcom(j)%plm(ipl)%p_fr * pcom(j)%plm(ipl)%mass
      if (up2(ipl)<pcom(ihru)%plm(ipl)%pmass)                            &
                  up2(ipl)=pcom(ihru)%plm(ipl)%pmass
      uapd(ipl) = up2(ipl) - pcom(ihru)%plm(ipl)%pmass
      uapd(ipl) = 1.5 * uapd(ipl)                     !! luxury p uptake
 
      return
      end subroutine pl_pupd