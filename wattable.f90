      subroutine wattable
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    watab       |mm            |water table based on 30 day antecedent
!!                               | climate (precip,et)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : rfqeo_30d,eo_30d, wtab, wtab_mx, wtab_mn, nd_30, ihru, pet_day, precip_eff,  &
        qday 

      integer :: j, j1

      j = 0
      j = ihru
      wtab_mn(j) = 0.
      wtab_mx(j) = 2.5

      !! compute 30 day sums
      rfqeo_30d(nd_30,j) = precip_eff - qday - pet_day
      eo_30d(nd_30,j) = pet_day
      rfqeo_sum = 0.
      eo_sum = 0.

      do i30 = 1, 30
        rfqeo_sum = rfqeo_sum + rfqeo_30d(i30,j)
        eo_sum = eo_sum + eo_30d(i30,j)
      end do

      if (eo_sum > 1.e-4) then
        w2 = rfqeo_sum / eo_sum
      else
        w2 = 0.
      end if
      w1 = amin1 (0.1,abs(w2))
      if (w2 > 0.) then
        wtl = wtab_mn(j)
      else
        wtl = wtab_mx(j)
      end if
      if (wtab(j) < 1.e-6) wtab(j) = 0.0
      wtab(j) = wtab(j) - w1 * (wtab(j) - wtl)
      
      return
      end subroutine wattable