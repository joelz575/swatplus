      subroutine mgt_tillmix (jj, bmix, idtill)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solutio
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    cnop          |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simulation
!!    deptil(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    mlyr          |none          |maximum number of soil layers
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!	  ranrns(:)     |mm            |random roughness of a given tillage operation 
!	  Drainmod  07/2006
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!	  Drainmod  08/2006
!!    ranrns_hru(:) |mm            |random roughness for a given HRU
!	  Drainmod  08/2006
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |biological mixing efficiency: this 
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soil layer
!!    dtil        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    rrns        |mm            |random roughness
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    l           |none          |counter
!!    nl          |none          |number of layers being mixed
!	  Drainmod  07/2006
!!	  ranrns_hru(:)|mm           |random roughness at time of a given tillage operation in HRU
!	  Drainmod  07/2006
!!    smix(:)     |varies        |amount of substance in soil profile
!!                               |that is being redistributed between 
!!                               |mixed layers
!!    thtill(:)   |none          |fraction of soil layer that is mixed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    SWAT: curno 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module

      integer, intent (in) :: jj, idtill
      real, intent (in) :: bmix
      integer :: l, k, nl
      real :: emix, dtil, rrns 
      real :: thtill(mlyr), smix(11+npmx)

      emix = 0.
      dtil = 0.
!	Drainmod  08/2006 
	rrns = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix
        dtil = Min(soil(jj)%phys(soil(jj)%nly)%d, 300.)
      else 
        !! tillage operation
        emix = tilldb(idtill)%effmix
        dtil = tilldb(idtill)%deptil
!	Drainmod  07/2006
      if (itill(jj) == 1) then
        cumei(jj) = 0.
        cumeira(jj) = 0.
        cumrt(jj) = 0.
        cumrai(jj) = 0.
        ranrns_hru(jj) = tilldb(idtill)%ranrns
      end if
!!    Drainmod 7/2006
      endif


      if (dtil > 10.) then

        !! incorporate bacteria - no mixing - lost from transport
        bactpq(jj) = bactpq(jj) * (1. - emix)
        bactps(jj) = bactps(jj) * (1. - emix)
        bactlpq(jj) = bactlpq(jj) * (1. - emix)
        bactlps(jj) = bactlps(jj) * (1. - emix)

        thtill = 0.
        smix = 0.
        nl = 0
        thtill(1) = 1.
        do l = 1, soil(jj)%nly
          if (l /= 1) then
            if (soil(jj)%phys(l)%d > dtil) then
              if (soil(jj)%phys(l-1)%d< dtil) then
                thtill(l) = (dtil - soil(jj)%phys(l-1)%d) /               &
                               (soil(jj)%phys(l)%d - soil(jj)%phys(l)%d)
                nl = l
              endif
            else
              thtill(l) = 1.
              nl = l
            endif
          endif
 
          !! calculate amount of each substance in the profile being
          !! redistributed between layers   
          if (thtill(l) > 0.) then
            smix(1) = smix(1) + thtill(l) * emix * soil1(jj)%mn(l)%no3
            smix(2) = smix(2) + thtill(l) * emix * soil1(jj)%hp(l)%n
            smix(3) = smix(3) + thtill(l) * emix * soil1(jj)%mn(l)%nh4
            smix(4) = smix(4) + thtill(l) * emix * soil1(jj)%mp(l)%lab
            smix(5) = smix(5) + thtill(l) * emix * soil1(jj)%hp(l)%p
            smix(6) = smix(6) + thtill(l) * emix * soil1(jj)%hs(l)%n
            smix(7) = smix(7) + thtill(l) * emix * soil1(j)%mp(l)%act
            smix(8) = smix(8) + thtill(l) * emix * soil1(jj)%tot(l)%n
            smix(9) = smix(9) + thtill(l) * emix * soil1(jj)%tot(l)%p
            smix(10) = smix(10) + thtill(l) * emix * soil1(jj)%mp(l)%sta
            smix(11) = smix(11) + thtill(l) * emix * soil(jj)%ly(l)%rsd
            do k = 1, npmx
            smix(11+k) = smix(11+k) + thtill(l) * emix *                &             
                                                  soil(jj)%ly(l)%pst(k)
            
            end do
          end if
        end do

        do l = 1, nl

          dg = 0.
          if (l == 1) then
            dg = soil(jj)%phys(1)%d
          else
            dg = soil(jj)%phys(l)%d - soil(jj)%phys(l-1)%d
          endif

          !! calculate new amount of each substance in each layer
          !! undergoing mixing
          soil1(jj)%mn(l)%no3 = soil1(jj)%mn(l)%no3 * (1. - thtill(l)) +    &
                          soil1(jj)%mn(l)%no3 * thtill(l) * (1. - emix)+    &
                          smix(1) * thtill(l) * dg / dtil

          soil1(jj)%hp(l)%n = soil1(jj)%hp(l)%n * (1. - thtill(l)) +        &
                          soil1(j)%hp(l)%n * thtill(l) * (1. - emix) +      &
                          smix(2) * thtill(l) * dg / dtil

          soil1(jj)%mn(l)%nh4 = soil1(jj)%mn(l)%nh4 * (1. - thtill(l)) +    &
                          soil1(jj)%mn(l)%nh4 * thtill(l) * (1. - emix)+    &
                          smix(3) * thtill(l) * dg / dtil

          soil1(jj)%mp(l)%lab = soil1(jj)%mp(l)%lab * (1.-thtill(l)) +      &
                          soil1(jj)%mp(l)%lab * thtill(l) * (1.-emix) +     &
                          smix(4) * thtill(l) * dg / dtil

          soil1(jj)%hp(l)%p = soil1(jj)%hp(l)%p * (1. - thtill(l))+         &
                      soil1(jj)%hp(l)%p * thtill(l) * (1. - emix) +         &
                      smix(5) * thtill(l) * dg / dtil

          soil1(jj)%hs(l)%n=soil1(jj)%hs(l)%n*(1. - thtill(l)) +            &
                      soil1(jj)%hs(l)%n * thtill(l) * (1. - emix) +         &
                      smix(6) * thtill(l) * dg / dtil

          soil1(j)%mp(l)%act = soil1(j)%mp(l)%act * (1. - thtill(l)) +      &
                      soil(j)%nut(l)%actp * thtill(l) * (1. - emix) +       &
                      smix(7) * thtill(l) * dg / dtil

          soil1(jj)%tot(l)%n = soil1(jj)%tot(l)%n * (1. - thtill(l)) +    &
                          soil1(jj)%tot(l)%n * thtill(l) * (1. - emix)+    &
                          smix(8) * thtill(l) * dg / dtil

          soil1(jj)%tot(l)%p = soil1(jj)%tot(l)%p * (1. - thtill(l)) +    &
                          soil1(jj)%tot(l)%p * thtill(l) * (1. - emix)+    &
                          smix(9) * thtill(l) * dg / dtil

          soil1(jj)%mp(l)%sta = soil1(jj)%mp(l)%sta * (1.-thtill(l))+       &
                          soil1(jj)%mp(l)%sta * thtill(l) * (1.-emix) +     &
                          smix(10) * thtill(l) * dg / dtil

          soil(jj)%ly(l)%rsd = soil(jj)%ly(l)%rsd * (1. - thtill(l)) +      &
                          soil(jj)%ly(l)%rsd * thtill(l) * (1. - emix) +    &
                          smix(11) * thtill(l) * dg / dtil
          soil(jj)%ly(l)%rsd = Max(soil(jj)%ly(l)%rsd,0.)

          if (hrupest(jj) > 0) then
          do k = 1, npmx
            soil(jj)%ly(l)%pst(k) =  soil(jj)%ly(l)%pst(k) *             &
                     (1. - thtill(l)) + soil(jj)%ly(l)%pst(k)  *         &
                     thtill(l) * (1. - emix) + smix(11+k) * thtill(l) *  & 
                     dg / dtil
          
          end do
          end if
        end do

        !! remove all residue from soil surface if mixing with moldboard
        !! plow (emix = 0.95 in default tillage database)
        if (emix > 0.9) then
          soil(jj)%ly(2)%rsd = soil(jj)%ly(2)%rsd + soil(jj)%ly(1)%rsd
          soil(jj)%ly(1)%rsd = 0.
        end if
      end if

      !! perform final calculations for tillage operation
      if (cnop > 1.e-4) then
        call curno(cnop,jj)
      end if
      ntil(jj) = ntil(jj) + 1

      return
      end subroutine mgt_tillmix