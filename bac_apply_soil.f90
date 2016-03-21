       subroutine bac_apply_soil (ibacdb, ibac, gc, frt_t,              &
                         sol_bacsol, sol_bacsor)
            
!!    this subroutine applies bacteria leached to the plants and soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ibtyp        |NA            |bacteria type from 'bact_parms.dat'
!!    sol_bacsol   |# cfu/m^2     |soluble bacteria in soil layer
!!    sol_prk      |mm            |water percolating through the soil layer
!!    sol_wt       |mm            |soil weight
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    baclch_out        |# cfu/m^2     |bacteria leached to the next layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    bac_ls_parms   |type bacteria_db - contains  'bac_lsparms_read.f'

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module

      gc1 = 1. - gc
      sol_bacsol = gc1 * sol_bacsol * frt_t * 100. + sol_bacsol
      sol_bacsol = bac_kd * sol_bacsol

      sol_bacsor = gc1 * sol_bacsol * frt_t * 100. + sol_bacsor
      sol_bacsor = (1. - bac_db(ibtyp)%kd) * sol_bacsor
      
      return
      
      end subroutine bac_apply_soil