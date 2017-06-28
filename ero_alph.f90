      subroutine ero_alph
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes alpha, a dimensionless parameter that
!!    expresses the fraction of total rainfall that occurs during 0.5h

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    ihru        |none        |HRU number
!!    i_mo        |none        |month being simulated
!!    nstep       |none        |number of lines of rainfall data for each
!!                             |day
!!    ovrlnd(:)   |mm H2O      |overland flow onto HRU from upstream
!!                             |routing unit
!!    precipday   |mm H2O      |amount of water reaching soil surface in HRU
!!    wst(:)%weat%ts(:) |mm H2O      |precipitation in time step for HRU
!!    rndseed(:,:)|none        |random number generator seed
!!    snomlt      |mm H2O      |amount of snow melt in HRU on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    al5         |none        |fraction of total rainfall on day that occurs
!!                             |during 0.5h highest intensity rainfall
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ab          |mm H2O      |lowest value al5 can have
!!    ajp         |mm H2O      |highest value al5 can have
!!    j           |none        |HRU number
!!    jj          |none        |counter
!!    k           |none        |number of time steps equivalent to 30 minutes
!!    kk          |none        |counter
!!    preceff     |mm H2O      |amount of rainfall on day in HRU
!!    rainsum     |mm H2O      |sum of rainfall during 30 min period
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Expo, Atri

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use basin_module
      use hydrograph_module
      use time_module

      integer :: j, k, kk, jj
      real :: ab, ajp, preceff, rainsum

      j = ihru
      ab = 0.02083
      
      select case (bsn_cc%event)
        case(0, 1)                !! daily rainfall, estimate al5
          preceff = 0.
            !! HRU sediment calculations
            if (precipday > snomlt) then
              preceff = precipday - snomlt
            else
              preceff = 0.
            endif
            if (preceff > ovrlnd(j)) then
              preceff = preceff - ovrlnd(j)
            else
              preceff = 0.
            endif

          ajp = 0.
          ajp = 1. - expo(-125. / (preceff + 5.))
          if (bsn_cc%sed_det == 0) then
            !iwst = ob(icmd)%wst           !!!!!!LREW_TXTINOUT_LANDSCAPE and COON Creek wouldn't run unless uncommented OR SED_DET == 1
          iwgn = wst(iwst)%wco%wgn
          al5 = Atri(ab, wgn_pms(iwgen)%amp_r(i_mo),                     &
                   ajp,rndseed(idg(6),iwgn))
          else
            al5 = wgn_pms(iwgen)%amp_r(i_mo)
          end if

        case default            !! subdaily rainfall, get from pcp data
          if (time%step > 0) then
            k = 0
            k = 30 / time%dtm
            k = k - 1
            do kk = 1, time%step+1-k
              rainsum = 0.
              do jj = 0, k
                if (wst(iwst)%weat%ts(kk+jj) > (snomlt+ovrlnd(j))/time%step) then
                  rainsum = rainsum + wst(iwst)%weat%ts(kk+jj) -                  &               
                                           (snomlt + ovrlnd(j)) / time%step
                end if
              end do
              al5 = Max(al5,rainsum)
            end do
            if (wst(iwst)%weat%precip > 0.01) then
              al5 = al5 / wst(iwst)%weat%precip
              al5 = Min(al5,.99)
            else
              al5 = ab
            end if
          else
            preceff = 0.
              !! HRU sediment calculations
              if (precipday > snomlt) then
                preceff = precipday - snomlt
              else
                preceff = 0.
              endif
              if (preceff > ovrlnd(j)) then
                preceff = preceff - ovrlnd(j)
              else
                preceff = 0.
              endif

            ajp = 0.
            ajp = 1. - expo(-125. / (preceff + 5.))
            if (bsn_cc%sed_det == 0) then
              al5 = Atri(ab, wgn_pms(iwgn)%amp_r(i_mo), ajp,             &      
                                              rndseed(idg(6),iwst))
            else
              al5 = wgn_pms(iwgn)%amp_r(i_mo)
            end if
          end if

      end select

      return
      end subroutine ero_alph