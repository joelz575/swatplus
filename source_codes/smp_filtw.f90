      subroutine smp_filtw      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of pollutants in surface runoff
!!    due to an edge of field filter or buffer strip

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year of simulation
!!    fsred(:)    |none          |reduction in bacteria loading from filter
!!                               |strip
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    trapeff(:)  |none          |filter strip trapping efficiency (used for
!!                               |everything but bacteria)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use hru_module, only : hru, fsred, sedorgn, surqno3, sedorgp, sedminpa, sedminps, surqsolp, sedyld, sanyld, &
        silyld, clayld, sagyld, lagyld, trapeff, hru_dafr, ihru
      use constituent_mass_module
      use output_ls_pesticide_module
      use time_module
      
      implicit none
      
      integer :: j               !none            |hru number
      integer :: k               !none            |counter 
      integer :: icmd            !                |

      j = 0
      j = ihru

!! compute filter strip reduction
      !bactrop = bactrop * fsred(j)
      !bactrolp = bactrolp * fsred(j)
      !bactsedp = bactsedp * fsred(j)
      !bactsedlp = bactsedlp * fsred(j)
      sedorgn(j) = sedorgn(j) * (1. - trapeff(j))
      surqno3(j) = surqno3(j) * (1. - trapeff(j))
      sedorgp(j) = sedorgp(j) * (1. - trapeff(j))
      sedminpa(j) = sedminpa(j) * (1. - trapeff(j))
      sedminps(j) = sedminps(j) * (1. - trapeff(j))
      surqsolp(j) = surqsolp(j) * (1. - trapeff(j))
      sedyld(j) = sedyld(j) * (1. - trapeff(j))
      sanyld(j) = sanyld(j) * (1. - trapeff(j))
      silyld(j) = silyld(j) * (1. - trapeff(j))
      clayld(j) = clayld(j) * (1. - trapeff(j))
      sagyld(j) = sagyld(j) * (1. - trapeff(j))
      lagyld(j) = lagyld(j) * (1. - trapeff(j))

        do k = 1, cs_db%num_pests
          hpestb_d(j)%pest(k)%surq = hpestb_d(j)%pest(k)%surq * (1. - trapeff(j))
          hpestb_d(j)%pest(k)%sed = hpestb_d(j)%pest(k)%sed * (1. - trapeff(j))
        end do

      return
      end subroutine smp_filtw