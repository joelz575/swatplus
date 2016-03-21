      subroutine sq_canopyint

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes canopy interception of rainfall
!!    used for methods other than curve number

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blai(:)     |none          |maximum (potential) leaf area index
!!    canmx(:)    |mm H2O        |maximum canopy storage
!!    canstor(:)  |mm H2O        |amount of water held in canopy storage
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    ihru        |none          |HRU number
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    wst(:)%weat%ts(:) |mm H2O        |precipitation in time step for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)  |mm H2O        |amount of water held in canopy storage
!!    precipday   |mm H2O        |precipitation reaching soil surface
!!    wst(:)%weat%ts(:) |mm H2O        |precipitation reaching soil surface in
!!                               |time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canmxl      |mm H2O        |maximum canopy storage at current day's leaf
!!                               |area
!!    canstori    |mm H2O        |initial canopy storage water content
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    xx          |mm H2O        |precipitation prior to canopy interception 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use time_module

      integer :: j, ii
      real :: xx, canmxl, canstori

      j = ihru

      if (blai_com(j) < 0.001) return

      if (time%step > 0) then
          canstori = canstor(j)
          canmxl = hru(j)%hyd%canmx * sumlai / blai_com(j)
          do ii = 2, time%step+1
            xx = 0.
            xx = wst(iwst)%weat%ts(ii)
            wst(iwst)%weat%ts(ii) = wst(iwst)%weat%ts(ii) - (canmxl - canstor(j))

            if (wst(iwst)%weat%ts(ii) < 0.) then
              canstor(j) = canstor(j) + xx
              wst(iwst)%weat%ts(ii) = 0.
            else
              canstor(j) = canmxl
            endif
          end do
          if (canstor(j) > canstori) then
            do ii = 1, time%step
              xx = 0.
              xx = wst(iwst)%weat%ts(ii)
              wst(iwst)%weat%ts(ii) = wst(iwst)%weat%ts(ii) - (canstor(j) - canstori)

              if (wst(iwst)%weat%ts(ii) < 0.) then
                canstori = canstori + xx
                wst(iwst)%weat%ts(ii) = 0.
              else
                canstori = canstor(j)
              endif
            end do
          end if

        else
          xx = 0.
          canmxl = 0.
          xx = precipday
          canmxl = hru(j)%hyd%canmx * sumlai / blai_com(j)
          precipday = precipday - (canmxl - canstor(j))
          if (precipday < 0.) then
            canstor(j) = canstor(j) + xx
            precipday = 0.
          else
            canstor(j) = canmxl
          endif
       end if

      return
      end subroutine sq_canopyint