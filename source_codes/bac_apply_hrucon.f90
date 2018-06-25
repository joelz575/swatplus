       subroutine bac_apply_hrucon
          
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

!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    bac_ls_parms   |type bacteria_db - contains  'bac_read_lsparms.f'

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : ipl, manure_kg
      use soil_module
      use plant_module
      use bacteria_module

      implicit none
            
      real :: frt_t        !          |
      real :: bioms_tot    !          |
      real :: bioms_min    !          |
      real :: gc           !none      |fraction of ground covered by plant foliage
      real :: gc1          !          |
      integer :: ibac      !none      |counter
      integer :: ibacdb    !          |
      integer :: ibtyp     !NA        |bacteria type from "bact_parms.dat"
      real :: j            !          |
            
      
      
      if (bioms_tot > bioms_min) then
        gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
        if (gc < 0.) gc = 0.
        gc1 = 1. - gc
        do ibac = 1, bact(ibacdb)%num
          ibtyp = bact(ibacdb)%bac(ibac)%num_db
          !frt_t = bac_db(ibtyp)%swf * manure_kg / 1000.
          do ipl = 1, pcom(j)%npl
            call bac_apply_plant (ibacdb, ibac, gc,frt_t,            &
                bac_db(ibtyp)%kd, pcom(j)%plg(ipl)%bac(ibac),        &
                soil(j)%ly(1)%bacsol(ibac), soil(j)%ly(1)%bacsor(ibac))
          end do
          call bac_apply_soil (ibacdb, ibac, gc, frt_t,              &             
                 soil(j)%ly(1)%bacsol(ibac), soil(j)%ly(1)%bacsor(ibac))
        end do
      end if
      
      return
      
      end subroutine bac_apply_hrucon