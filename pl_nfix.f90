      subroutine pl_nfix
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates nitrogen fixation by legumes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year of simulation
!!    hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
!!    ihru        |none          |HRU number
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fxg         |
!!    fxn         |
!!    fxr         |
!!    fxw         |
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    sumn        |kg N/ha       |total amount of nitrate stored in soil profile
!!    uno3l       |kg N/ha       |plant nitrogen demand
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use basin_module
      use organic_mineral_mass_module

      integer :: j, l
      real :: uno3l, fxw, sumn, fxn, fxg, fxr

      j = 0
      j = ihru
 
!! compute the difference between supply and demand
      if (uno3d(ipl) > nplnt(j)) then
        uno3l = 0.
        uno3l = uno3d(ipl) - nplnt(j)
      else
        !! if supply is being met, fixation=0 and return
        fixn = 0.
        return
      endif


!! compute fixation as a function of no3, soil water, and growth stage

      !! compute soil water factor
      fxw = 0.
      fxw = soil(j)%sw / (.85 * soil(j)%sumfc)

      !! compute no3 factor
      sumn = 0.
      fxn = 0.
      do l = 1, soil(j)%nly
        sumn = sumn + soil1(j)%mn(l)%no3
      end do
      if (sumn > 300.) fxn = 0.
      if (sumn > 100. .and. sumn <= 300.) fxn = 1.5 - .0005 * sumn
      if (sumn <= 100.) fxn = 1.

      !! compute growth stage factor
      fxg = 0.
      if (pcom(j)%plcur(ipl)%phuacc>.15.and.                             &
               pcom(j)%plcur(ipl)%phuacc<=.30) then
          fxg = 6.67 * pcom(j)%plcur(ipl)%phuacc - 1.
      endif
      if(pcom(j)%plcur(ipl)%phuacc>.30.and.                              &
               pcom(j)%plcur(ipl)%phuacc<=.55)fxg=1.
      if (pcom(j)%plcur(ipl)%phuacc>.55.and.                             &
               pcom(j)%plcur(ipl)%phuacc<=.75) then
         fxg = 3.75 - 5. * pcom(j)%plcur(ipl)%phuacc
      endif

      fxr = Min(1., fxw, fxn) * fxg
      fxr = Max(0., fxr)

      fixn = Min(6., fxr * uno3l)
      fixn = bsn_prm%fixco * fixn + (1. - fixco) * uno3l
             !! if bsn_prm%fixco = 0 then fix the entire demand
      fixn = Min(fixn, uno3l)
      fixn = Min(bsn_prm%nfixmx, fixn)

      return
      end subroutine pl_nfix