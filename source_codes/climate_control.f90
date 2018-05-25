      subroutine climate_control
 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls weather inputs to SWAT. Precipitation and
!!    temperature data is read in and the weather generator is called to 
!!    fill in radiation, wind speed and relative humidity as well as 
!!    missing precipitation and temperatures. Adjustments for climate
!!    changes studies are also made in this subroutine.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |weather generator data
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum solar radiation for the day in HRU
!!    wst(:)%weat%ts(:) |mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min
!!    SWAT: pmeas, tmeas, smeas, hmeas, wmeas
!!    SWAT: pgen, tgen, weatgn, clgen, slrgen, rhgen, wndgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use basin_module
      use time_module
      use hydrograph_module
      use maximum_data_module
      
      implicit none
           
      integer :: k                !none          |counter
      integer :: inum3sprev       !none          |subbasin number of previous HRU
      integer :: ii               !none          |counter       
      integer :: iyp              !none          |year currently being simulated
      integer :: idap             !julain date   |day currently being simulated
      integer :: ib               !none          |counter
      real :: tdif                !deg C         |difference in temperature for station and
                                  !              |temperature for elevation band
      real :: pdif                !mm H2O        |difference in precipitation for station and
                                  !              |precipitation for elevation band
      real :: ratio               !none          |fraction change in precipitation due to 
                                  !              |elevation changes
      real :: petmeas             !mm H2O        |potential ET value read in for day 
      real :: half_hr_mn          !mm H2O        |lowest value half hour precip fraction can have
      real :: half_hr_mx          !mm H2O        |highest value half hour precip fraction can have
      integer :: iwgn             !              |
      integer :: ipg              !              | 
      integer :: ist              !none          |counter
      integer :: ig               !              |
      real :: ramm                !MJ/m2         |extraterrestrial radiation
      real :: xl                  !MJ/kg         |latent heat of vaporization
      real :: expo                !              | 
      real :: atri                !none          |daily value generated for distribution
      real :: ifirstpet           !none          |potential ET data search code
                                  !              |0 first day of potential ET data located in
                                  !              |file
                                  !              |1 first day of potential ET data not located
                                  !              |in file
      
      
      !! Precipitation:
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        ipg = wst(iwst)%wco%pgage
        if (wst(iwst)%wco_c%pgage == "sim") then
          !! simulated precip
          call cli_pgen(iwgn)
          if (time%step > 0) call cli_pgenhr(iwgn)
        else
          !! measured precip
          if (pcp(ipg)%tstep > 0) then
          !! subdaily precip
            wst(iwst)%weat%precip = 0.
            do ist = 1, time%step
              wst(iwst)%weat%ts(ist) = pcp(ipg)%tss(ist,time%day,time%yrs)
              if (wst(iwst)%weat%ts(ist) <= -97.) then
				!! simulate missing data
				call cli_pgen(iwgn)
				call cli_pgenhr(iwgn)
				exit
			  end if
			  wst(iwst)%weat%precip = wst(iwst)%weat%precip + wst(iwst)%weat%ts(ist)
            end do
          else
		  !! daily precip
            wst(iwst)%weat%precip = pcp(ipg)%ts(time%day,time%yrs)
            !! simulate missing data
            if (wst(iwst)%weat%precip <= -97.) then
              call cli_pgen(iwgn)
			end if
          end if
        end if
      end do
      
!! Temperature: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        call cli_weatgn(iwgn)
        if (wst(iwst)%wco_c%tgage == "sim") then
          call cli_tgen(iwgn)
        else
          ig = wst(iwst)%wco%tgage
          wst(iwst)%weat%tmax = tmp(ig)%ts(time%day,time%yrs)
          wst(iwst)%weat%tmin = tmp(ig)%ts2(time%day,time%yrs)
          if (wst(iwst)%weat%tmax <= -97. .or. wst(iwst)%weat%tmin <= -97.) then
            call cli_weatgn(iwgn)
            call cli_tgen(iwgn)
          end if
        end if
        wst(iwst)%weat%tave = (wst(iwst)%weat%tmax + wst(iwst)%weat%tmin) / 2.
      end do

!! Solar Radiation: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        call cli_clgen(iwgn)
        if (wst(iwst)%wco_c%sgage== "sim") then
          call cli_slrgen(iwgn)
        else
          ig = wst(iwst)%wco%sgage
          wst(iwst)%weat%solrad = slr(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%solrad <= -97.) then
            call cli_slrgen(iwgn)
          end if
        end if
      end do
        
!! Relative Humidity: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco_c%hgage == "sim") then
          call cli_rhgen(iwgn)
        else
          ig = wst(iwst)%wco%hgage
          wst(iwst)%weat%rhum = hmd(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%rhum <= -97.) then
            call cli_rhgen(iwgn)
          end if
        end if
        !! simple dewpoint eqn from Lawrence 2005. Bull. Amer. Meteor. Soc.
        wst(iwst)%weat%dewpt = wst(iwst)%weat%tave - (1. - wst(iwst)%weat%rhum) / 5.
      end do 

!! Wind Speed: 
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wst(iwst)%wco_c%wgage == "sim") then
          call cli_wndgen(iwgn)
        else
          ig = wst(iwst)%wco%wgage
          wst(iwst)%weat%windsp = wnd(ig)%ts(time%day,time%yrs)
          if (wst(iwst)%weat%windsp <= -97.) then
            call cli_wndgen(iwgn)
          end if
        end if
      end do 

!! Potential ET: Read in data !!
      if (bsn_cc%pet == 3) then
        if (ifirstpet == 0) then
          read (140,5100) petmeas
        else
          ifirstpet = 0
          do 
            iyp = 0
            idap = 0
            read (140,5000) iyp, idap, petmeas
            if (iyp == time%yrc .and. idap == time%day_start) exit
          end do
        end if
        do iwst = 1, db_mx%wst
          wst(iwst)%weat%pet = petmeas
        end do
      else
        !! HARGREAVES POTENTIAL EVAPOTRANSPIRATION METHOD
        !! extraterrestrial radiation
        !! 37.59 is coefficient in equation 2.2.6 !!extraterrestrial
        !! 30.00 is coefficient in equation 2.2.7 !!max at surface
        do iwst = 1, db_mx%wst
          ramm = wst(iwst)%weat%solradmx * 37.59 / 30. 
          if (wst(iwst)%weat%tmax > wst(iwst)%weat%tmin) then
            xl = 2.501 - 2.361e-3 * wst(iwst)%weat%tave
            wst(iwst)%weat%pet = .0023 * (ramm / xl) * (wst(iwst)%weat%tave      &
               + 17.8) * (wst(iwst)%weat%tmax - wst(iwst)%weat%tmin) ** 0.5
            wst(iwst)%weat%pet = Max(0., wst(iwst)%weat%pet)
          else
            wst(iwst)%weat%pet = 0.
          endif
        end do
      end if

!! Update Precip minus PET 30 day moving sum
      ppet_mce = ppet_mce + 1
      if (ppet_mce > ppet_ndays) ppet_mce = 1
      do iwst = 1, db_mx%wst
        !! subract the 30 day previous and add the current day precip-pet
        wgn_pms(iwgn)%p_minus_pet_sum = wgn_pms(iwgn)%p_minus_pet_sum + (wst(iwst)%weat%precip - wst(iwst)%weat%pet)  &
                                                                      - wgn_pms(iwgn)%p_minus_pet(ppet_mce)
        wgn_pms(iwgn)%p_minus_pet(ppet_mce) = wst(iwst)%weat%precip - wst(iwst)%weat%pet
      end do
            
!! Calculate maximum half-hour rainfall fraction
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        half_hr_mn = 0.02083
        half_hr_mx = 1. - expo(-125. / (wst(iwst)%weat%precip + 5.))
        wst(iwst)%weat%precip_half_hr = Atri(half_hr_mn, wgn_pms(iwgn)%amp_r(time%mo), half_hr_mx, rndseed(10,iwgn))
      end do

!! Base Zero Heat Units
      do iwst = 1, db_mx%wst
        iwgn = wst(iwst)%wco%wgn
        if (wgn_pms(iwgn)%phutot > 0.) then
          if (wst(iwst)%weat%tave > 0.) wst(iwst)%weat%phubase0 = wst(iwst)%weat%phubase0   &
                                            + wst(iwst)%weat%tave / wgn_pms(iwgn)%phutot
        else
          wst(iwst)%weat%phubase0 = 0.
        end if
        if (time%end_yr == 1) wst(iwst)%weat%phubase0 = 0.
      end do

!! Climate Change Adjustments !!
      do iwst = 1, db_mx%wst
        wst(iwst)%weat%precip = wst(iwst)%weat%precip * (1. + wst(iwst)%rfinc(time%mo) / 100.)
        if (wst(iwst)%weat%precip < 0.) wst(iwst)%weat%precip = 0.
        if (time%step > 0) then
          do ii = 1, time%step
            wst(iwst)%weat%ts(ii) = wst(iwst)%weat%ts(ii) * (1. + wst(iwst)%rfinc(time%mo) / 100.)
            if (wst(iwst)%weat%ts(ii) < 0.) wst(iwst)%weat%ts(ii) = 0.
          end do
        end if
        wst(iwst)%weat%tmax = wst(iwst)%weat%tmax + wst(iwst)%tmpinc(time%mo)
        wst(iwst)%weat%tmin = wst(iwst)%weat%tmin + wst(iwst)%tmpinc(time%mo)
        wst(iwst)%weat%solrad = wst(iwst)%weat%solrad + wst(iwst)%radinc(time%mo)
        wst(iwst)%weat%solrad = Max(0.,wst(iwst)%weat%solrad)
        wst(iwst)%weat%rhum = wst(iwst)%weat%rhum + wst(iwst)%huminc(time%mo)
        wst(iwst)%weat%rhum = Max(0.01,wst(iwst)%weat%rhum)
        wst(iwst)%weat%rhum = Min(0.99,wst(iwst)%weat%rhum)
      end do

      return
 5000 format (i4,i3,f5.1)
 5100 format (7x,f5.1)

      end subroutine climate_control