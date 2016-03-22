      subroutine sq_crackflow
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine modifies surface runoff to account for crack flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff for the hour in HRUS
!!    ihru        |none          |HRU number
!!    surfq(:)    |mm H2O        |surface runoff in the HRU for the day
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff for the hour in HRU
!!    surfq(:)    |mm H2O        |surface runoff in the HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    voli        |none          |volume available for crack flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module

      integer :: j, ii
      real :: voli

      j = ihru

      !! subtract crack flow from surface runoff
      if (surfq(j) > voltot) then
        surfq(j) = surfq(j) - voltot
      else
        surfq(j) = 0.
      endif

      if (time%step > 0) then
        voli = 0.
        voli = voltot
        do ii = 1, time%step  !j.jeong 4/24/2009
          if (hhqday(j,ii) > voli) then
            hhqday(j,ii) = hhqday(j,ii) - voli
            voli = 0.
          else
            voli = voli - hhqday(j,ii)
            hhqday(j,ii) = 0.
          endif
        end do
      end if

      return
      end subroutine sq_crackflow