	subroutine swr_origtile(d)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes tile drainage using basic tile equations developed by Saleh et al.(2005)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number

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
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: 
!!    SWAT:

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module

      integer :: j

      j = ihru
      isdr = isdr_no(j)

!!    compute tile flow using the original tile equations

      dmod_m = wt_shall - d
      
      if (hru(j)%sol%sw > hru(j)%sol%sumfc) then
        sw_excess = (dmod_m / wt_shall) * (hru(j)%sol%sw - hru(j)%sol%sumfc)
        qtile = sw_excess * (1. - Exp(-24. / sdr(isdr)%time))
        if (qtile > 30.) then
          xyz = 0.
        end if
        qtile = amin1(qtile, drain_co)
      else
        qtile = 0.
      end if
     
	return
	end subroutine swr_origtile