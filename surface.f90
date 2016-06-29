      subroutine surface

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models surface hydrology at any desired time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ovrlnd(:)   |mm H2O        |overland flow onto HRU from upstream
!!                               |routing unit
!!    peakr       |mm/hr         |peak runoff rate
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel 
!!                               |for day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU during
!!                               |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: canopyint, snom, crackvol, dailycn, volq, crackflow, surfst_h2o,
!!    SWAT: alph, pkq, tran, eiusle, ysed

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      
      use surfrunoff_module
      use erosion_module
      use basin_module
      use time_module
      use hydrograph_module, only: iwst

      integer :: j,sb,kk
      real :: precip_fr
      real :: irfr,hruvirr

      j = 0
      j = ihru
      ulu = hru(j)%luse%urb_lu
      sb = hru_sub(j)
      hruirrday = 0.
      irmmdt = 0.

      !!compute canopy interception
      call sq_canopyint

      !! compute snow melt
      call sq_snom

      !! compute crack volume
      if (bsn_cc%crk == 1) call sq_crackvol

      if (time%step > 0) then
        do ii = 1, time%step
          wst(iwst)%weat%ts(ii+1) = wst(iwst)%weat%ts(ii+1) + ovrlnd_dt(j,ii)
        end do
      end if
      
      !! add irrigation from retention-irrigation ponds to soil water
      if (ri_luflg(j)==1) then
        irfr = hru(j)%km * (1. - urbdb(ulu)%fimp) / ri_subkm(sb) 
        do ii=1,time%step
          !amount irrigated in hru
          hruvirr = ri_totpvol(ii) * irfr !m3
          irmmdt(ii) = hruvirr / (hru(j)%km                              &
             * (1.- urbdb(ulu)%fimp) * 1000.) !mm/dt
          
          !add irrigated water to soil water content
          do kk = 1, soil(j)%nly
            if(irmmdt(ii)<soil(j)%phys(kk)%ul-soil(j)%phys(kk)%st) then
               soil(j)%phys(kk)%st = soil(j)%phys(kk)%st + irmmdt(ii)
               exit
            else
               soil(j)%phys(kk)%st = soil(j)%phys(kk)%ul
               irmmdt(ii) = irmmdt(ii) - (soil(j)%phys(kk)%ul -          &
                    soil(j)%phys(kk)%st)
            end if
          end do
      
        end do
      end if

      !!calculate subdaily curve number value
      call sq_dailycn

        !! compute runoff - surfq in mm H2O
      if (precipday > 0.1) then
         call sq_volq 

        !! adjust runoff for loss into crack volume
         if (surfq(j) > 0. .and. bsn_cc%crk == 1) call sq_crackflow
      end if

      !! add irrigation runoff and surface runon runoff
      surfq(j) = surfq(j) + qird(j) + surfqout
      qird(j) = 0.

      !! calculate amount of surface runoff reaching main channel during day
      !! (qday) and store the remainder
      call sq_surfst

      !! calculate half-hour rainfall
      if (precipday > 0.01) call ero_alph

      if (qday > 0.0001) then
        !! compute peak rate - peakr in m3/s  
        call ero_pkq 
      end if  

      if (qday > 0.0001 .and. peakr > 0.) then
        call ero_eiusle

	!! calculate sediment erosion by rainfall and overland flow
		call ero_ovrsed
      end if

      call ero_cfactor
      if (surfq(j) > 1.e-6 .and. peakr > 1.e-6) call ero_ysed(0)

      if (qday < 0.) qday = 0.

1010  format (2(i4,1x),a5,a4,1x,10f8.3)
      return
      end subroutine surface