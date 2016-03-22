      subroutine nut_nminrl

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily nitrogen and phosphorus
!!    mineralization and immobilization considering fresh organic
!!    material (plant residue) and active and stable humus material

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none          |current year of simulation
!!    hru_dafr(:)   |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    ihru          |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    rsdco_pl(:)   |none          |plant residue decomposition coefficient. The
!!                                 |fraction of residue which will decompose in
!!                                 |a day assuming optimal moisture,
!!                                 |temperature, C:N ratio, and C:P ratio
!!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ca          |
!!    cdg         |none          |soil temperature factor
!!    cnr         |
!!    cnrf        |
!!    cpr         |
!!    cprf        |
!!    csf         |none          |combined temperature/soil water factor
!!    decr        |
!!    hmn         |kg N/ha       |amount of nitrogen moving from active organic
!!                               |nitrogen pool to nitrate pool in layer
!!    hmp         |kg P/ha       |amount of phosphorus moving from the organic
!!                               |pool to the labile pool in layer
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    kk          |none          |soil layer used to compute soil water and
!!                               |soil temperature factors
!!    r4          |
!!    rdc         |
!!    rmn1        |kg N/ha       |amount of nitrogen moving from fresh organic
!!                               |to nitrate(80%) and active organic(20%)
!!                               |pools in layer
!!    rmp         |kg P/ha       |amount of phosphorus moving from fresh organic
!!                               |to labile(80%) and organic(20%) pools in layer
!!    rwn         |kg N/ha       |amount of nitrogen moving from active organic
!!                               |to stable organic pool in layer
!!    sut         |none          |soil water factor
!!    wdn         |kg N/ha       |amount of nitrogen lost from nitrate pool in
!!                               |layer due to denitrification
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Exp, Sqrt, Min, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      integer :: j, k, kk
      real :: rmn1, rmp, xx, csf, rwn, hmn, hmp, r4, cnr, cnrf, cpr
      real :: cprf, ca, decr, rdc, wdn, cdg, sut

      j = ihru
      nactfr = .02

      do k = 1, hru(j)%sol%nly

        if (k == 1) then
          kk = 2
        else
          kk = k
        end if

        !! mineralization can occur only if temp above 0 deg
        if (soil(j)%phys(kk)%tmp > 0.) then
          !! compute soil water factor
          sut = 0.
	!! change for domain error 1/29/09 gsm check with Jeff !!!
	    if (soil(j)%phys(kk)%st < 0.) soil(j)%phys(kk)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(kk)%st/soil(j)%phys(kk)%fc)
!          sut = Min(1., sut)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(kk)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)

          !! compute flow from active to stable pools
          rwn = .1e-4 * (soil(j)%nut(k)%aorgn * (1. / nactfr - 1.) -     &
                                                  soil(j)%nut(k)%orgn)
          if (rwn > 0.) then
            rwn = Min(rwn, soil(j)%nut(k)%aorgn)
          else
            rwn = -(Min(Abs(rwn), soil(j)%nut(k)%orgn))
          endif
          soil(j)%nut(k)%orgn = Max(1.e-6, soil(j)%nut(k)%orgn + rwn)
          soil(j)%nut(k)%aorgn = Max(1.e-6, soil(j)%nut(k)%aorgn - rwn)

          !! compute humus mineralization on active organic n
          hmn = bsn_prm%cmn * csf * soil(j)%nut(k)%aorgn
          hmn = Min(hmn, soil(j)%nut(k)%aorgn)
          !! compute humus mineralization on active organic p
          xx = soil(j)%nut(k)%orgn + soil(j)%nut(k)%aorgn
          if (xx > 1.e-6) then
            hmp = 1.4 * hmn * soil(j)%nut(k)%orgp / xx
          else
            hmp = 0.
          end if
          hmp = Min(hmp, soil(j)%nut(k)%orgp)
          !! move mineralized nutrients between pools
          soil(j)%nut(k)%aorgn = amax1(1.e-6, soil(j)%nut(k)%aorgn - hmn)
          soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 + hmn
          soil(j)%nut(k)%orgp = soil(j)%nut(k)%orgp - hmp
          soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + hmp

          !! compute residue decomp and mineralization of 
          !! fresh organic n and p (upper two layers only)
          rmn1 = 0.
          rmp = 0.
          if (k <= 2) then
            r4 = .58 * soil(j)%ly(k)%rsd
            if (soil(j)%nut(k)%fon + soil(j)%nut(k)%no3 > 1.e-4) then
              cnr = r4 / (soil(j)%nut(k)%fon  + soil(j)%nut(k)%no3)
              if (cnr > 500.) cnr = 500.
              cnrf = Exp(-.693 * (cnr - 25.) / 25.)
            else
              cnrf = 1.
            end if
            
            if (soil(j)%nut(k)%fop + soil(j)%nut(k)%solp > 1.e-4) then
              cpr = r4 / (soil(j)%nut(k)%fop + soil(j)%nut(k)%solp)
              if (cpr > 5000.) cpr = 5000.
              cprf = Exp(-.693 * (cpr - 200.) / 200.)
            else
              cprf = 1.
            end if

            ca = 0.
            decr = 0.
            rdc = 0.
            ca = Min(cnrf, cprf, 1.)
            
            !! compute residue decomp and mineralization for each plant
            decr = .05
            !do ipl = 1, npl(j)        !! we need to decomp each plant**
              
            if (npl(j) > 0) then
              decr = rsdco_plcom(j) * ca * csf
            else
              decr = 0.05
            end if
            decr = Max(bsn_prm%decr_min, decr)
            decr = Min(decr, 1.)
            soil(j)%ly(k)%rsd = amax1(1.e-6, soil(j)%ly(k)%rsd)
            rdc = decr * soil(j)%ly(k)%rsd
            soil(j)%ly(k)%rsd = soil(j)%ly(k)%rsd - rdc
            if (soil(j)%ly(k)%rsd < 0.) soil(j)%ly(k)%rsd = 0.
            rmn1 = decr * soil(j)%nut(k)%fon 
            soil(j)%nut(k)%fop = amax1(1.e-6,soil(j)%nut(k)%fop)
            rmp = decr * soil(j)%nut(k)%fop

            soil(j)%nut(k)%fop = soil(j)%nut(k)%fop - rmp
            soil(j)%nut(k)%fon  = amax1(1.e-6,soil(j)%nut(k)%fon)
            soil(j)%nut(k)%fon  = soil(j)%nut(k)%fon  - rmn1
            soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 + .8 * rmn1
            soil(j)%nut(k)%aorgn = soil(j)%nut(k)%aorgn + .2 * rmn1
            soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + .8 * rmp
            soil(j)%nut(k)%orgp = soil(j)%nut(k)%orgp + .2 * rmp
          end if
!! septic changes 1/28/09 gsm
!!  compute denitrification
        wdn = 0.   
	  if (i_sep(j) /= k .or. sep(isep)%opt  /= 1) then
	    if (sut >= bsn_prm%sdnco) then
	      wdn = soil(j)%nut(k)%no3 * (1.-Exp(-bsn_prm%cdn * cdg *           &
                   soil(j)%cbn(k)%cbn))
	    else
	      wdn = 0.
	    endif
	    soil(j)%nut(k)%no3 = soil(j)%nut(k)%no3 - wdn
	  end if
! septic changes 1/28/09 gsm

!			call nut_denit(k,j,cdg,wdn,0.05)

        end if
      end do
      return
      end subroutine nut_nminrl