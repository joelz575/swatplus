      subroutine smp_filtw      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of pollutants in surface runoff
!!    due to an edge of field filter or buffer strip

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    curyr       |none          |current year of simulation
!!    fsred(:)    |none          |reduction in bacteria loading from filter
!!                               |strip
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hrupest(:)  |none          |pesticide use flag:
!!                               | 0: no pesticides used in HRU
!!                               | 1: pesticides used in HRU
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    sbactrolp   |# colonies/ha |average annual number of less persistent 
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sbactsedp   |# colonies/ha |average annual number of persistent bacteria   
!!                               |transported with sediment in surface runoff
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
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    sbactrolp   |# colonies/ha |average annual number of less persistent 
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface 
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sbactsedp   |# colonies/ha |average annual number of persistent bacteria
!!                               |transported with sediment in surface runoff
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
        silyld, clayld, sagyld, lagyld, trapeff, hrupest, hru_dafr, ihru, bactrolp, bactrop, bactsedlp,     &
        bactsedp, npmx, sbactrolp, sbactrop, sbactsedlp, sbactsedp 
      use constituent_mass_module
      use output_ls_constituent_module
      use time_module
      
      implicit none
      
      integer :: j               !none            |hru number
      integer :: k               !none            |counter 
      integer :: icmd            !                |

      j = 0
      j = ihru

!! compute filter strip reduction
      bactrop = bactrop * fsred(j)
      bactrolp = bactrolp * fsred(j)
      bactsedp = bactsedp * fsred(j)
      bactsedlp = bactsedlp * fsred(j)
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
      if (hrupest(j) == 1) then
        npmx = cs_db%num_pests
        do k = 1, npmx
          hpest_bal(j)%pest(k)%surq = hpest_bal(j)%pest(k)%surq * (1. - trapeff(j))
          hpest_bal(j)%pest(k)%sed = hpest_bal(j)%pest(k)%sed * (1. - trapeff(j))
        end do
      end if

!! summary calculations
      if (time%yrs > pco%nyskip) then
        sbactrop = sbactrop + bactrop * hru_dafr(j)
        sbactrolp = sbactrolp + bactrolp * hru_dafr(j)
        sbactsedp = sbactsedp + bactsedp * hru_dafr(j)
        sbactsedlp = sbactsedlp + bactsedlp * hru_dafr(j)
      end if

      return
      end subroutine smp_filtw