      subroutine bac_hrucontrol
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates bacteria growth, transport with runoff and
!!    loss due to percolation into soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blpq        |# cfu/m^2     |less persistent bacteria in soil solution at
!!                               |beginning of day
!!    blps        |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles at beginning of day
!!    bpq         |# cfu/m^2     |persistent bacteria in soil solution at
!!                               |beginning of day
!!    bps         |# cfu/m^2     |persistent bacteria attached to soil particles
!!                               |at beginning of day
!!    cbact       |
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor to convert kg/ha to g/t(ppm)
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm, only : soil, hru, pcom, ihru, bact, tmpav, sedyld, precipday, qday
      use constituent_mass_module, only : obcs
      use hydrograph_module, only : icmd

      integer :: j
      real :: bacsol_out, bacsor_out

      j = ihru

      mbac = obcs(icmd)%num_paths
      if (mbac == 0) return          
      do ib = 1, mbac
        ibtyp = bact(ibacdb)%bac(ib)%num_db
        if (soil(j)%ly(1)%bacsol(ib) < 1.e-6) soil(j)%ly(1)%bacsol(ib) =.0
        if (soil(j)%ly(1)%bacsor(ib) < 1.e-6) soil(j)%ly(1)%bacsor(ib) =.0
        if (pcom(j)%plg(1)%bac(ib) < 1.e-6) pcom(j)%plg(1)%bac(ib) = 0.0

        !! bacteria washoff, regrowth and dieoff in the soil layer and on the plant
        call bac_ls_process (ibtyp, pcom(j)%plg(1)%bac(ib),                  &
             soil(j)%ly(1)%bacsol(ib), soil(j)%ly(1)%bacsor(ib),             &
             precipday, tmpav(j), bacdiegrosol_out, bacdiegrosor_out,        &
             bacdiegroplt_out)
        
        !! bacteria in the surface runoff and sediment transported
        if (qday > 0.) then
        call bac_ls_runoff (ibtyp, pcom(j)%plg(1)%bac(ib),                   &  
             soil(j)%ly(1)%bacsol(ib), soil(j)%ly(1)%bacsor(ib),             &
             1.0, qday, soil(j)%phys(1)%bd, soil(j)%phys(1)%d,               &
             sedyld(j), hru(j)%area_ha, bacsol_out, bacsor_out)
        end if

        !! bacteria leached through a soil layer 
        call bac_ls_swrouting (ibtyp, pcom(j)%plg(1)%bac(ib),                &
                 soil(j)%ly(1)%prk, soil(j)%phys(1)%conv_wt, baclch_out)

      end do
      
      return
      end subroutine bac_hrucontrol