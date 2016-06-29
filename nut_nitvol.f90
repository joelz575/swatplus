      subroutine nut_nitvol

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily mineralization (NH3 to NO3)
!!    and volatilization of NH3

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none          |current year of simulation
!!    hru_dafr(:)   |km**2/km**2   |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    akn         |
!!    akv         |
!!    cecf        |none          |volatilization CEC factor
!!    dmidl       |
!!    dpf         |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    rnit        |kg N/ha       |amount of nitrogen moving from the NH3 to the
!!                               |NO3 pool (nitrification) in the layer
!!    rnv         |
!!    rvol        |kg N/ha       |amount of nitrogen lost from the NH3 pool due
!!                               |to volatilization
!!    sw25        |
!!    swf         |
!!    swwp        |
!!    tf          |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module
      
      integer :: j, k
      real :: sw25, swwp, swf, xx, dmidl, dpf, akn, akv, rnv, rnit, rvol
      real :: tf 
      real :: cecf = 0.15

      j = 0
      j = ihru 

      do k = 1, soil(j)%nly
        tf = 0.
        tf = .41 * (soil(j)%phys(k)%tmp - 5.) / 10.

        if (soil(j)%nut(k)%nh3 > 0. .and. tf >= 0.001) then
          sw25 = 0.
          swwp = 0.
          sw25 = soil(j)%phys(k)%wpmm + 0.25 * soil(j)%phys(k)%fc
          swwp = soil(j)%phys(k)%wpmm + soil(j)%phys(k)%st
          if (swwp < sw25) then
            swf = 0.
            swf=(swwp-soil(j)%phys(k)%wpmm)/(sw25-soil(j)%phys(k)%wpmm)
          else
            swf = 1.
          endif

          if (k == 1) then
            xx = 0.
          else
            xx = soil(j)%phys(k-1)%d
          endif

          dmidl = (soil(j)%phys(k)%d + xx) / 2.
          dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
          akn = tf * swf
          akv = tf * dpf * cecf
          rnv = soil(j)%nut(k)%nh3 * (1. - Exp(-akn - akv))
          rnit = 1. - Exp(-akn)
          rvol = 1. - Exp(-akv)

          !! calculate nitrification (NH3 => NO3)
	    !! apply septic algorithm only to active septic systems
          if(k/=i_sep(j).or.sep(isep)%opt /= 1) then  ! J.Jeong for septic, biozone layer
             if (rvol + rnit > 1.e-6) then
               rvol = rnv * rvol / (rvol + rnit)
               rnit = rnv - rvol
               if (rnit < 0.) rnit = 0.
               soil(j)%nut(k)%nh3 = Max(1.e-6, soil(j)%nut(k)%nh3-rnit)
             endif
             if (soil(j)%nut(k)%nh3 < 0.) then
               rnit = rnit + soil(j)%nut(k)%nh3
               soil(j)%nut(k)%nh3 = 0.
             endif
             soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 + rnit

             !! calculate ammonia volatilization
             soil(j)%nut(k)%nh3 = Max(1.e-6, soil(j)%nut(k)%nh3 - rvol)
             if (soil(j)%nut(k)%nh3 < 0.) then
               rvol = rvol + soil(j)%nut(k)%nh3
               soil(j)%nut(k)%nh3 = 0.
             endif
          end if
        end if

      end do

      return
      end subroutine nut_nitvol