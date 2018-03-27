      subroutine bac_ls_swrouting (ibtyp, sol_bacsol, sol_prk, sol_wt,       &
                                                          baclch_out)
      use bacteria_module
      
      implicit none
            
      integer :: ibtyp      !none          |bacteria type from 'bact_parms.dat'
      real :: sol_bacsol    !# cfu/m^2     |soluble bacteria in soil layer
      real :: sol_prk       !mm            |water percolating through the soil layer
      real :: sol_wt        !mm            |soil weight
      real :: baclch_out    !# cfu/m^2     |bacteria leached to the next layer
      
      !! compute bacteria incorporated into the soil
      baclch_out = sol_bacsol * sol_prk / ((sol_wt / 1000.) *          & 
                                          bac_db(ibtyp)%perco)
      baclch_out = Min(baclch_out, sol_bacsol)
      baclch_out = Max(baclch_out, 0.)
      sol_bacsol = sol_bacsol - baclch_out

      return
      end subroutine bac_ls_swrouting