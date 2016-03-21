      function cli_tair(hr,jj)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function approximates hourly air temperature from daily max and
!!    min temperatures as documented by Campbell (1985)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tair        |deg C         |air temperature for hour in HRU
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hr          |none          |hour if the day
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Cos

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!    subroutine modified by SLN
       
      integer, intent (in) ::  jj
      real, intent(in) :: hr
      real :: cli_tair

!! update hi or lo temperature depending on hour of day
      if (hr == 3) tmp_lo(jj) = tmn(jj)
      if (hr == 15) tmp_hi(jj) = tmx(jj)

!! SWAT manual equation 2.3.1
      cli_tair = 0.
      cli_tair = 0.5 * (tmp_hi(jj) + tmp_lo(jj) + (tmp_hi(jj) -      &        
             tmp_lo(jj) * Cos(0.2618 * Real(hr - 15))))

      return
      end function