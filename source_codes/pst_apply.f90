      subroutine pst_apply (jj, ipest, pest_kg, pestop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    driftco(:)   |none             |coefficient for pesticide drift directly
!!                                   |onto stream
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    plt_pst(:,:) |kg/ha            |pesticide on plant foliage
!!    pst_dep      |kg/ha          |depth of pesticide in soil
!!    sol_pst(:,:,1)|kg/ha           |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main 
!!                               |channel in subbasin
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use basin_module
      use hru_module, only : hru, driftco, pst_dep
      use soil_module
      use plant_module
      
      implicit none
      
      integer :: j                       !none          |HRU number
      integer, intent (in) :: jj         !none          |subbasin number
      integer :: k                       !none          |sequence number of pesticide
      real :: xx                         !kg/ha         |amount of pesticide applied to HRU
      real :: gc                         !none          |fraction of ground covered by plant foliage
      integer, intent (in) :: ipest      !none          |counter
      integer, intent (in) :: pestop     !              | 
      real, intent (in) :: pest_kg       !kg/ha         |amount of pesticide applied 
      real :: pst_kg                     !kg/ha         |amount of pesticide applied to HRU
      integer :: nly                     !none          |counter

      j = jj

      !! initialize local variables
      k = ipest                                     !! sequential hru pesticide number
      xx = pest_kg * chemapp_db(mgt%op4)%app_eff

      ! added for pesticide incorporation
      if (pst_dep > 1.e-6) then
       do nly = 1, soil(j)%nly
         if (nly == 1) then
           if (pst_dep < soil(j)%phys(nly)%d) then
             soil(j)%ly(1)%pst(k) =  soil(j)%ly(1)%pst(k) + xx
             exit
           endif
         else
         if (pst_dep>soil(j)%phys(nly-1)%d .and. pst_dep < soil(j)%phys(nly)%d)then
           soil(j)%ly(nly)%pst(k) = soil(j)%ly(nly)%pst(k) + xx
           exit
           endif
         endif
        enddo
      else

      !! calculate ground cover
      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      !! update pesticide levels on ground and foliage
      pcom(j)%pest(k) = pcom(j)%pest(k) + gc * xx
      soil(j)%ly(1)%pst(k) = soil(j)%ly(1)%pst(k) + (1. - gc) * xx
      
      !! added endif for pesticide incorporation 3/31/08 gsm
      endif

      return
      end subroutine pst_apply