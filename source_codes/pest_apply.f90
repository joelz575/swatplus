      subroutine pest_apply (jj, ipest, pest_kg, pestop)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    drift(:)     |kg               |amount of pesticide drifting onto main 
!!                                   |channel in subbasin
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main 
!!                               |channel in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use mgt_operations_module
      use basin_module
      use hru_module, only : hru
      use soil_module
      use plant_module
      use output_ls_pesticide_module
      use constituent_mass_module
      
      implicit none
      
      integer :: j                       !none          |HRU number
      integer, intent (in) :: jj         !none          |subbasin number
      integer :: k                       !none          |sequence number of pesticide
      real :: gc                         !none          |fraction of ground covered by plant foliage
      integer, intent (in) :: ipest      !none          |counter
      integer, intent (in) :: pestop     !              | 
      real, intent (in) :: pest_kg       !kg/ha         |amount of pesticide applied 
      real :: pst_dep                    !kg/ha          |depth of pesticide in soil
      integer :: nly                     !none          |counter

      j = jj

      !! initialize local variables
      k = ipest                                     !! sequential hru pesticide number

      hpestb_d(j)%pest(k)%apply_f = 0.
      hpestb_d(j)%pest(k)%apply_s = 0.
      ! added for pesticide incorporation
      if (pst_dep > 1.e-6) then
       do nly = 1, soil(j)%nly
         if (nly == 1) then
           if (pst_dep < soil(j)%phys(nly)%d) then
             cs_soil(j)%ly(1)%pest(k) =  cs_soil(j)%ly(1)%pest(k) + pest_kg
             exit
           endif
         else
           if (pst_dep>soil(j)%phys(nly-1)%d .and. pst_dep < soil(j)%phys(nly)%d)then
             cs_soil(j)%ly(nly)%pest(k) = cs_soil(j)%ly(nly)%pest(k) + pest_kg
             exit
           end if
         end if
        end do
      else

      !! calculate ground cover
      gc = (1.99532 - erfc(1.333 * pcom(j)%lai_sum - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      !! update pesticide levels on ground and foliage
      cs_pl(j)%pest(k) = cs_pl(j)%pest(k) + gc * pest_kg
      cs_soil(j)%ly(1)%pest(k) = cs_soil(j)%ly(1)%pest(k) + (1. - gc) * pest_kg
      hpestb_d(j)%pest(k)%apply_f = gc * pest_kg
      hpestb_d(j)%pest(k)%apply_s = (1. - gc) * pest_kg
      
      endif

      return
      end subroutine pest_apply