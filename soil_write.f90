      subroutine soil_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use basin_module
      use organic_mineral_mass_module

      integer :: j, l
      real :: solp_t, solno3_t, solorgn_t, solorgp_t

        do j = 1, mhru
        solp_t = 0.
	  solno3_t = 0.
	  solorgn_t = 0.
	  solorgp_t = 0.
         do l = 1, soil(j)%nly
           solp_t = solp_t + soil1(j)%mp(l)%lab 
           solno3_t = solno3_t + soil1(j)%mn(l)%no3
	     !if (bsn_cc%cswat == 0) then
		   !	solorgn_t = solorgn_t + sol_orgn(l,j)
	     !else
		   !	solorgn_t = solorgn_t + sol_n(l,j)
		   !end if
		   
		   !!By Zhang
		   !!============
	     if (bsn_cc%cswat == 0) then
			solorgn_t = solorgn_t + soil1(j)%hp(l)%n
	     end if
	     if (bsn_cc%cswat == 1) then
			solorgn_t = solorgn_t + soil(j)%ly(l)%n
		   end if		   
		   if (bsn_cc%cswat ==2) then
		    solorgn_t = solorgn_t + soil1(j)%hs(l)%n + soil(j)%cbn(l)%hpn
		   end if
		   !!By Zhang
		   !!============		   
		   
           solorgp_t = solorgp_t + soil1(j)%hp(l)%p
         end do
       if (pco%solout == 'year') then
         write (121,1000) i,soil(j)%ly(1)%rsd,solp_t, solno3_t,         &
                  solorgn_t, solorgp_t, cnday(j)
           if (pco%csvout == 'yes' .and. pco%solout == 'year') then
             write (121,1000) i,soil(j)%ly(1)%rsd,solp_t, solno3_t,     &
                  solorgn_t, solorgp_t, cnday(j)
           end if
       endif
      end do
      
      return
 1000 format ('SOIL   ',i4,1x,6f10.2)
      end