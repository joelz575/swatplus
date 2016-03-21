       subroutine bac_apply_plant (ibacdb, ibac, gc, frt_t, bac_kd,          &
                                       pl_bac, sol_bacsol, sol_bacsor)
      
      !! calculate ground cover
      !! graze only if adequate biomass in HRU
            
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
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      !! add bacteria - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.
      pl_bac = gc * (sol_bacsol + sol_bacsor)* frt_t * 100. + pl_bac
      
      return
      
      end subroutine bac_apply_plant