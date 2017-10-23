      subroutine mgt_trop_gro

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine ends and initializes tropical plant growth

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
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

      use climate_parms
      use basin_module
      use parm, only : pcom, soil, ihru, idp, ipl
      use jrw_datalib_module, only:  pldb

      real :: resnew
      integer :: j

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