      subroutine bac_ls_runoff (ibtyp, pl_bac, sol_bacsol, sol_bacsor,          &
                                 enratio, surf_ro, sol_bd, sol_dep,             &
                                 sed_yld, da_ha,bacsol_out, bacsor_out)
      
!!    this subroutine calculates bacteria in the surface runoff and sediment transported

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    precip       |mm            |precipitation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name              |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bacdiegrosol_out  |# cfu/m^2       |regrowth and dieoff of soluble bacteria in soil
!!    bacdiegrosol_out  |# cfu/m^2     |regrowth and dieoff of sorbed bacteria in soil
!!    bacdiegrosol_out  |# cfu/m^2     |regrowth and dieoff of bacteria on plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bpq         |# cfu/m^2     |counter
!!    bps         |# cfu/m^2     |counter
!!    bpl         |# cfu/m^2     |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ MODULES USED ~ ~ ~
!!    bac_ls_parms   |type bacteria_db - contains  "bac_read_lsparms.f"

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use bacteria_module
      
      implicit none
      
      integer :: ibtyp                  !none          |bacteria type from "bact_parms.dat"
      real :: sol_bacsol                !# cfu/m^2     |soluble bacteria in soil layer
      real ::sol_bacsor                 !# cfu/m^2     |sorbed bacteria in soil layer
      real ::surf_ro                    !              |
      real ::sol_bd                     !              |
      real ::sol_dep                    !              |
      real ::wt1                        !none          |conversion factor to convert kg/ha to g/t(ppm)
      real :: cbact                     !              |
      real ::enratio                    !none          |enrichment ratio calculated for day in HRU
      real ::sed_yld                    !              |
      real ::da_ha                      !ha            |area of watershed in hectares
      real, intent (out) :: bacsol_out  !              |  
      real, intent (out) ::bacsor_out   !              | 
      real :: pl_bac                    !# cfu/m^2     |bacteria on plant
      
      !! compute soluble bacteria in the surface runoff
      bacsol_out = sol_bacsol * surf_ro /                        &
                 (sol_bd * sol_dep * bac_db(ibtyp)%kd)
      bacsol_out = Min(bacsol_out, sol_bacsol)
      bacsol_out = Max(sol_bacsol, 0.)
      sol_bacsol = sol_bacsol - bacsol_out

      !! compute bacteria transported with sediment
      if (enratio > 0.) then 
        wt1 = sol_bd * sol_dep / 1000.
        cbact = sol_bacsor * enratio / wt1
        bacsor_out = .0001 * cbact * sed_yld / (da_ha + 1.e-6)
        bacsor_out = Min(bacsor_out, sol_bacsor)
        sol_bacsor = sol_bacsor - bacsor_out
      end if

      return
      end subroutine bac_ls_runoff