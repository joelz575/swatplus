      subroutine nut_solp
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none         |current year of simulation
!!    hru_dafr(:)   |none         |fraction of watershed area located in HRU
!!    ihru          |none         |HRU number
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    vap         |kg P/ha       |amount of P leached from soil layer
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: j
      real :: xx, vap

      j = 0
      j = ihru

!! compute soluble P lost in surface runoff
      xx = 0.
      xx = soil(j)%phys(1)%bd * soil(j)%phys(1)%d * bsn_prm%phoskd
      surqsolp(j) = soil(j)%nut(1)%solp  * surfq(j) / (xx + 1.)   !dont merge
        !!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
      surqsolp(j) = Min(surqsolp(j), soil(j)%nut(1)%solp)
      surqsolp(j) = Max(surqsolp(j), 0.)
      soil(j)%nut(1)%solp = soil(j)%nut(1)%solp - surqsolp(j)


!! compute soluble P leaching
      vap = 0.
      vap = soil(j)%nut(1)%solp * soil(j)%ly(1)%prk /                   &
        ((soil(j)%phys(1)%conv_wt/ 1000.) * bsn_prm%pperco + .1)   !dont merge
      vap = Min(vap, .5 * soil(j)%nut(1)%solp)
      soil(j)%nut(1)%solp = soil(j)%nut(1)%solp - vap
      if (hru(j)%sol%nly >= 2) then
        soil(j)%nut(2)%solp = soil(j)%nut(2)%solp + vap
      end if
   
      do ii = 2, hru(j)%sol%nly-1
        vap = 0.
	 if (ii/=i_sep(j)) then
       vap = soil(j)%nut(ii)%solp * soil(j)%ly(ii)%prk /                &
        ((soil(j)%phys(ii)%conv_wt / 1000.) * bsn_prm%pperco + .1)  !dont merge
	   vap = Min(vap, .2 * soil(j)%nut(ii)%solp)
	   soil(j)%nut(ii)%solp = soil(j)%nut(ii)%solp - vap
	   soil(j)%nut(ii+1)%solp = soil(j)%nut(ii+1)%solp + vap
           if (ii == ldrain(j)) then
             vap = soil(j)%nut(ii)%solp * qtile /                        &
              (soil(j)%phys(ii)%conv_wt / 1000. * bsn_prm%pperco + .1)  !dont merge
             soil(j)%nut(ii)%solp = soil(j)%nut(ii)%solp - vap
           endif
	 endif
	end do
	percp(j) = vap

      return
      end subroutine nut_solp