      subroutine pl_tstr
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     computes temperature stress for crop growth - strstmp

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    t_base(:)   |deg C         |minimum temperature for plant growth
!!    t_opt(:)    |deg C         |optimal temperature for plant growth
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    rto         |
!!    tgx         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use climate_parms
      use jrw_datalib_module, only : pldb
      use parm, only : pcom, tmpav, tmn, ihru, idp, ipl, iwgen, tmn, tmpav
      
      integer :: j
      real :: tgx, rto

      j = ihru

      tgx = tmpav(j) - pldb(idp)%t_base

      if (tgx <= 0.) then
        pcom(j)%plstr(ipl)%strst = 0.
      else
        if (tmpav(j) > pldb(idp)%t_opt) then
         tgx = 2. * pldb(idp)%t_opt - pldb(idp)%t_base - tmpav(j)
        end if

        rto = 0.
        rto = ((pldb(idp)%t_opt - tmpav(j)) / (tgx + 1.e-6)) ** 2

        if (rto <= 200. .and. tgx > 0.) then
          pcom(j)%plstr(ipl)%strst = Exp(-0.1054 * rto)
        else
          pcom(j)%plstr(ipl)%strst = 0.
        end if

        if(tmn(j)<=wgn_pms(iwgen)%tmp_an-15.)pcom(j)%plstr(ipl)%strst=0.

      end if

      return
      end subroutine pl_tstr