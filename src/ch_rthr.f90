      subroutine ch_rthr
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach 
!!    using a constant storage coefficient  
!!	Routing method: Variable Storage routing   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    scoef       |none          |storage coefficient
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!	  Modified by Jeahak Jeong, Blackland Research, Temple, USA

      use basin_module
      use climate_module
      use channel_data_module
      use time_module
      use channel_module
      use hydrograph_module, only : ob, isdch, icmd
      use channel_velocity_module
      use sd_channel_module
      
      implicit none

      integer :: ii        !none          |counter (hour)
      integer :: jrch      !none          |reach number
      real :: c            !none          |inverse of channel side slope
      real :: p            !m             |wetted perimeter
      real :: scoef        !none          |storage coefficient
      real :: vol          !m^3 H2O       |volume of water in reach
      real :: topw         !m             |width of channel at water level
      real :: qman         !m^3/s or m/s  |flow rate or flow velocity
      real :: inflo        !m^3           |inflow water volume
      real :: xs_area      !m^2           |cross section area of channel
      real :: dep_flo      !m             |depth of flow
      real :: perim_wet    !m             |wetted perimeter
      real :: ttime        !hr            |travel time through the reach
      real :: t_inc        !hr            |time in routing step - 1/time%step
      real :: outflo       !m^3           |outflow water volume
      real :: tl           !m^3           |transmission losses during time step
      real :: trans_loss   !m^3           |transmission losses during day
      real :: rate_flo     !m^3/s         |flow rate
      real :: ev           !m^3           |evaporation during time step
      real :: evap         !m^3           |evaporation losses during day
      real :: rto          !              |ratio for interpolating rating curve
      real :: outflo_sum   !m^3           |total outflow for the day
      integer :: iwst
      integer :: ielev

      jrch = isdch
      jhyd = sd_dat(jrch)%hyd
      
      trans_loss = 0.
      evap = 0.
      tl = 0.
      ev = 0.
      outflo = 0.
      outflo_sum = 0.
      
      !! volume at start of day
      vol = sd_ch(jrch)%stor

      !! subdaily time step
      do ii = 1, time%step

        !! flow rate during time step m3/s = m3 / (24 / (1/day) * 3600) 
        inflo = ob(icmd)%tsin(ii) / (86400. / time%step)
        
        !! update volume of water in reach - m3
        vol = vol + ob(icmd)%tsin(ii) 
        vol = Max(vol, 1.e-14)

        !! find where flow fits in rating curve (0.1,0.5,1.0,1.2,1.8 * bankfull flow rate)
        do ielev = 1, 3
          if (vol < ch_rcurv(jrch)%elev(ielev)%vol) then
            if (ielev == 1) then
              rto = vol / ch_rcurv(jrch)%elev(ielev)%vol
              rcurv = ch_rcurv(jrch)%elev(ielev) * rto
              rcurv%ttime = ch_rcurv(jrch)%elev(ielev)%ttime
              exit
            end if
            if (ielev > 1) then
              rto = (vol - ch_rcurv(jrch)%elev(ielev-1)%vol) /     &
                (ch_rcurv(jrch)%elev(ielev)%vol - ch_rcurv(jrch)%elev(ielev-1)%vol)
              call chrc_interp (ch_rcurv(jrch)%elev(ielev-1), ch_rcurv(jrch)%elev(ielev), ielev, rto, rcurv)
              exit
            end if
          end if
          if (ielev == 3) then
            rto = 1. + (vol - ch_rcurv(jrch)%elev(ielev)%vol) / ch_rcurv(jrch)%elev(ielev)%vol
            rcurv = ch_rcurv(jrch)%elev(ielev) * rto
            exit
          end if
          
        end do

        !! interpolated flow rate
        rate_flo = rcurv%flo_rate
       
        if (rate_flo > 0.) then
          !! interpolated travel time
          ttime = rcurv%ttime
          
          !! Variable Storage Coefficient - calculate volume of water leaving reach on day
          t_inc = 24. / time%step
          scoef = t_inc / (ttime + t_inc)
          if (scoef > 1.) scoef = 1.
          outflo = scoef * vol
          if (outflo < 1.e-12) outflo = 0.

          !! calculate transmission losses
          tl = sd_chd(jhyd)%chk * sd_ch(jrch)%chl * rcurv%perim_wet * ttime   !mm/hr * km * mm * hr = m3       
          tl = Min(tl, outflo)
          outflo = outflo - tl
          trans_loss = trans_loss + tl

          !! calculate evaporation
          if (outflo > 0.) then
            !! calculate width of channel at water level
            if (rcurv%dep <= sd_ch(jrch)%chd) then
              topw = ch_rcurv(jrch)%wid_btm + 2. * rcurv%dep * sd_chd(jhyd)%chss
            else
              topw = 5. * sd_ch(jrch)%chw + 8. * (rcurv%dep - sd_ch(jrch)%chd)
            end if
            
            iwst = 1
            ev = bsn_prm%evrch * wst(iwst)%weat%pet * sd_ch(jrch)%chl * topw * ttime
            if (ev < 0.) ev = 0.
            ev = Min(ev, outflo)
            outflo = outflo - ev
            evap = evap + ev
          end if

          !! set volume of water in channel at end of hour
          write (2612,*) ii, ttime, scoef, vol, ob(icmd)%tsin(ii), outflo
          vol = vol - outflo !- tl - ev
          ob(icmd)%hyd_flo(1,ii) = outflo
          outflo_sum = outflo_sum + outflo
          
        end if          !! rate_flo > 0. 

      end do            !! end of sub-daily loop

      !! save storage volume for next day and set outflow for day
      !ch_stor(jrch)%flo = vol
      !ht2%flo = outflo_sum

      return
      end subroutine ch_rthr
