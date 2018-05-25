      subroutine smp_buffer
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of nitrates through a riparian 
!!    buffer system - developed for Sushama at NC State

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
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
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
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use hru_module, only : latno3, filterw, ihru
      
      implicit none
      
      integer :: j              !none             |hru number
      integer :: k              !none             |counter
      real :: reduc             !none             |fraction of water uptake by plants achieved 

      j = 0
      j = ihru

!! compute nitrate reduction as a function of distance to stream
      reduc = 2.1661 * filterw(j) - 5.1302
      if (reduc < 0.) reduc = 0.
      latno3(j) = latno3(j) * (1. - reduc / 100.)

      return
      end subroutine smp_buffer