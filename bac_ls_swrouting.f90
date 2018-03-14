      subroutine bac_ls_swrouting (ibtyp, sol_bacsol, sol_prk, sol_wt,       &
                                                          baclch_out)
      
!!    this subroutine calculates bacteria leached through a soil layer 

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
!!    bac_ls_parms   |type bacteria_db - contains  'bac_read_lsparms.f'

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use bacteria_module
            
      integer :: ibtyp
      real :: sol_bacsol, sol_prk, sol_wt, baclch_out
      
      !! compute bacteria incorporated into the soil
      baclch_out = sol_bacsol * sol_prk / ((sol_wt / 1000.) *          & 
                                          bac_db(ibtyp)%perco)
      baclch_out = Min(baclch_out, sol_bacsol)
      baclch_out = Max(baclch_out, 0.)
      sol_bacsol = sol_bacsol - baclch_out

      return
      end subroutine bac_ls_swrouting