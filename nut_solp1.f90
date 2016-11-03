      subroutine nut_solp1
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation
!!!!!!!!!!!!!!!!!!!!!!!!
!!   CALC (was orginally "xx")  |none       |variable to hold intermediate calculation result
!!   SURFQ(J)                   |mm H2O     |surface runoff generated on day in HRU
!!   SURQSOLP(J)
!!   VAP                        |kg P/ha    |amount of P leached from soil layer
!!   I_SEP(J)
!!   PERCP(J)
!!!!!!!!!!!!!!!!!!!!!!!!!
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none         |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    calc        |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, only : hru, soil, surqsolp, percp, surfq, i_sep
      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module

      integer :: j
      real :: calc, vap
      

      j = ihru

!! compute soluble P lost in surface runoff
      calc = soil(j)%phys(1)%bd * soil(j)%phys(1)%d * bsn_prm%phoskd
      surqsolp(j) = soil1(j)%sta(1)%p  * surfq(j) / (calc + 1.)   !dont merge
        !!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
      surqsolp(j) = Min(surqsolp(j), soil1(j)%sta(1)%p)
      surqsolp(j) = Max(surqsolp(j), 0.)
      soil1(j)%sta(1)%p = soil1(j)%sta(1)%p - surqsolp(j)


!! compute soluble P leaching
      vap = soil1(j)%sta(1)%p * soil(j)%ly(1)%prk /                   &
        ((soil(j)%phys(1)%conv_wt/ 1000.) * bsn_prm%pperco + .1)   !dont merge
      vap = Min(vap, .5 * soil1(j)%sta(1)%p)
      soil1(j)%sta(1)%p = soil1(j)%sta(1)%p - vap
      if (soil(j)%nly >= 2) then
        soil1(j)%sta(2)%p = soil1(j)%sta(2)%p + vap
      end if
   
      do ii = 2, soil(j)%nly-1
	   if (ii/=i_sep(j)) then
         vap = soil1(j)%sta(ii)%p * soil(j)%ly(ii)%prk /                &
          ((soil(j)%phys(ii)%conv_wt / 1000.) * bsn_prm%pperco + .1)  !dont merge
	     vap = Min(vap, .2 * soil1(j)%sta(ii)%p)
	     soil1(j)%sta(ii)%p = soil1(j)%sta(ii)%p - vap
	     soil1(j)%sta(ii+1)%p = soil1(j)%sta(ii+1)%p + vap
           if (ii == hru(j)%lumv%ldrain) then
             vap = soil1(j)%sta(ii)%p * qtile /                        &
              (soil(j)%phys(ii)%conv_wt / 1000. * bsn_prm%pperco + .1)  !dont merge
             soil1(j)%sta(ii)%p = soil1(j)%sta(ii)%p - vap
           endif
	   endif
      end do
      
	  percp(j) = vap

      return
      end subroutine nut_solp1