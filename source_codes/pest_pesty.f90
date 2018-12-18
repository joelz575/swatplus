      subroutine pest_pesty
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticide transported with suspended sediment 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    ihru          |none         |HRU number
!!    pst_enr(:,:)  |none         |pesticide enrichment ratio
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    zdb(:,:)      |mm           |division term from net pesticide equation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : hru, sedyld, ihru, enratio
      use soil_module
      use constituent_mass_module
      use output_ls_pesticide_module
      
      implicit none 

      real :: conc        !              |concentration of pesticide in soil
      real :: er          !none          |enrichment ratio for pesticides
      real :: zdb1        !              |
      integer :: j        !none          |HRU number
      integer :: k        !none          |counter
      integer :: ipest_db !none          |pesticide number from database
      real :: pest_init   !kg/ha         |amount of pesticide in soil
      integer :: icmd     !              | 

      j = ihru

      if (cs_db%num_pests == 0) return

      do k = 1, cs_db%num_pests
        ipest_db = cs_db%pest_num(k)
        if (ipest_db > 0) then
          pest_init = soil(j)%ly(1)%pst(k)
          
          if (pest_init >= .0001) then
            zdb1 = soil(j)%phys(1)%ul + soil(j)%ly(1)%kp(k) *                           &
                                            soil(j)%phys(1)%bd * soil(j)%phys(1)%thick
            !! units: mm + (m^3/ton)*(ton/m^3)*mm = mm
            conc = 100. * soil(j)%ly(1)%kp(k) * pest_init / (zdb1 + 1.e-10)

            if (hru(j)%hyd%erorgn > .001) then
              er = hru(j)%hyd%erorgn
            else
              er = enratio
            end if

            hpestb_d(j)%pest(k)%sed = .001* sedyld(j) * conc * er / hru(j)%area_ha
            if (hpestb_d(j)%pest(k)%sed < 0.) hpestb_d(j)%pest(k)%sed = 0.
            if (hpestb_d(j)%pest(k)%sed > pest_init) hpestb_d(j)%pest(k)%sed = pest_init
            soil(j)%ly(1)%pst(k) = pest_init - hpestb_d(j)%pest(k)%sed
          end if
        end if
      end do

      return
      end subroutine pest_pesty