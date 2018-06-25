	subroutine swr_origtile(d)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes tile drainage using basic tile equations developed by Saleh et al.(2005)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: 
!!    SWAT:

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use tiles_data_module
      use hru_module, only : hru, ihru, qtile, sw_excess, wt_shall
      use soil_module
      
      implicit none

      integer :: j             !none          |HRU number
      integer :: isdr          !              |
      real :: dmod_m           !              |
      real :: d                !m             |depth of flow

      j = ihru
      isdr = hru(j)%tiledrain

!!    compute tile flow using the original tile equations

      dmod_m = wt_shall - d
      
      if (soil(j)%sw > soil(j)%sumfc) then
        sw_excess = (dmod_m / wt_shall) * (soil(j)%sw - soil(j)%sumfc)
        qtile = sw_excess * (1. - Exp(-24. / sdr(isdr)%time))
        qtile = amin1(qtile, sdr(isdr)%drain_co)
      else
        qtile = 0.
      end if
     
	return
	end subroutine swr_origtile