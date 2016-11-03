      subroutine nut_denit1(k,j,cdg,wdn,void)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! VOF (local)
!! WDN (local)
!! VOID
!! CDG
!! VOF
!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    this subroutine computes denitrification 

    use parm, only : soil, cdg, wdn, void
    use basin_module
    use organic_mineral_mass_module

	integer :: k,j

	vof = 1. / (1. + (void/0.04)**5)
	wdn =  soil1(j)%no3(1) * (1. - Exp(-bsn_prm%cdn * cdg * vof *          &
              soil(j)%cbn(k)%cbn))
	soil1(j)%no3(1) = soil1(j)%no3(1) - wdn

	return
	end subroutine nut_denit1