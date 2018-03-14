      subroutine mgt_trop_gro

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine ends and initializes tropical plant growth

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_module
      use basin_module
      use hru_module, only : pcom, soil, ihru, ipl
      use plant_data_module

      real :: resnew
      integer :: j, idp

      j = ihru

      do ipl = 1, pcom(j)%npl
          if (pcom(j)%plcur(ipl)%monsoon_init == 1) then
              idp = pcom(j)%plcur(ipl)%idplt
              sol_st2 = soil(j)%phys(1)%st + soil(j)%phys(2)%st
              sol_fc2 = soil(j)%phys(1)%st + soil(j)%phys(2)%fc
              sol_stup2 = soil(j)%phys(1)%st + soil(j)%phys(2)%st
              sol_fcup2 = soil(j)%phys(1)%fc + soil(j)%phys(2)%fc
              if (sol_st2 >= pldb(idp)%frsw_gro * sol_fc2) then
                  pcom(j)%plcur(ipl)%gro = 1
                  pcom(j)%plcur(ipl)%phuacc = 0. 
                  pcom(j)%plcur(ipl)%idorm = 0
                  pcom(j)%plcur(ipl)%monsoon_init = 0
              end if
          endif
      end do
      
      return
      end subroutine mgt_trop_gro