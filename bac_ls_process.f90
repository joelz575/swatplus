      subroutine bac_ls_process (ibtyp, pl_bac, sol_bacsol, sol_bacsor,     &
                                 precip, tmpav, bacdiegrosol_out,           &
                                  bacdiegrosor_out, bacdiegroplt_out)
      
!!    this subroutine calculates bacteria washoff, regrowth and dieoff in the 
!!    soil layer and on the plant

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ibtyp        |NA            |bacteria type from 'bact_parms.dat'
!!    pl_bac       |# cfu/m^2     |bacteria on plant
!!    sol_bacsol   |# cfu/m^2     |soluble bacteria in soil layer
!!    sol_bacsor   |# cfu/m^2     |sorbed bacteria in soil layer
!!    precip       |mm            |precipitation
!!    tmpav        |deg C         |average temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bacdiegrosol_out  |# cfu/m^2     |regrowth and dieoff of soluble bacteria in soil
!!    bacdiegrosol_out  |# cfu/m^2     |regrowth and dieoff of sorbed bacteria in soil
!!    bacdiegrosol_out  |# cfu/m^2     |regrowth and dieoff of bacteria on plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bpq         |# cfu/m^2     |bacteria (soluble) in soil at start of timestep
!!    bps         |# cfu/m^2     |bacteria (sorbed) in soil at start of timestep
!!    bpl         |# cfu/m^2     |bacteria on plant at start of timestep
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Theta

!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    bac_ls_parms   |type bacteria_db - contains  'bac_lsparms_read.f'

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use bacteria_module
      
      integer :: ibtyp
      real :: bpq, bps, bpl
      real :: precip, washoff, pl_bac, sol_bacsol, sol_bacsor, tmpav
      real :: bacdiegrosol_out, bacdiegrosor_out, bacdiegroplt_out
      
      !! compute bacteria wash off
      if (precip >= 2.54) then
        washoff = bac_db(ibtyp)%washoff * pl_bac
        if (washoff > pl_bac) washoff = pl_bac
        sol_bacsol = sol_bacsol + washoff
        pl_bac = pl_bac - washoff
      end if

      !! compute bacteria die-off and re-growth on foilage
      bpl = pl_bac
      pl_bac = pl_bac * Exp(-Theta(1.05,bac_db(ibtyp)%t_adj,tmpav)) -     &
                                              bac_db(ibtyp)%conc_min
      pl_bac = Max(0., pl_bac)
      if (pl_bac < bac_db(ibtyp)%conc_min)                                &           
                                     pl_bac = bac_db(ibtyp)%conc_min

      !! compute bacteria die-off and re-growth in surface soil layer
      bpq = sol_bacsol
      sol_bacsol=sol_bacsol* Exp(-Theta(1.05,bac_db(ibtyp)%t_adj,tmpav))   &
                                              - bac_db(ibtyp)%conc_min
      sol_bacsol = Max(0., sol_bacsol)
      if (sol_bacsol < bac_db(ibtyp)%conc_min)                             &    
                                   sol_bacsol = bac_db(ibtyp)%conc_min

      bps = sol_bacsol
      sol_bacsor=sol_bacsor* Exp(-Theta(1.05,bac_db(ibtyp)%t_adj,tmpav))   &
                                             - bac_db(ibtyp)%conc_min
      sol_bacsor = Max(0., sol_bacsor)
      if (sol_bacsor < bac_db(ibtyp)%conc_min)                             &
                                  sol_bacsor = bac_db(ibtyp)%conc_min

      bacdiegrosol_out = bpq - sol_bacsol
      bacdiegrosor_out = bps - sol_bacsor
      bacdiegroplt_out = bpl - pl_bac

      return
      end subroutine bac_ls_process