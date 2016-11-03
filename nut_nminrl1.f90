      subroutine nut_nminrl1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  NPL 
!!  RSDCO_PCOM
!!  RSDCO_PLCOM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

      use parm, only : hru, soil, pcom, rsdco_plcom, i_sep
      use jrw_datalib_module
      use basin_module
      use organic_mineral_mass_module

      integer :: j, k, kk
      real :: rmn1, rmp, xx, csf, rwn, hmn, hmp, r4, cnr, cnrf, cpr
      real :: cprf, ca, decr, rdc, wdn, cdg, sut
      
      j = ihru
      nactfr = .02

      !! mineralization can occur only if temp above 0 deg
      if (soil(j)%phys(1)%tmp > 0.) then
      !! compute residue decomp and mineralization of fresh organic n and p of flat residue
        do ipl = 1, pcom(j)%npl        !! we need to decompose each plant
          rmn1 = 0.
          rmp = 0.
          r4 = .58 * hru(j)%rsd_flt(ipl)%mass
          if (hru(j)%rsd_flt(ipl)%nmass + soil1(j)%no3(1) > 1.e-4) then
            cnr = r4 / (hru(j)%rsd_flt(ipl)%nmass  + soil1(j)%no3(1))
            if (cnr > 500.) cnr = 500.
            cnrf = Exp(-.693 * (cnr - 25.) / 25.)
          else
            cnrf = 1.
          end if
            
          if (hru(j)%rsd_flt(ipl)%pmass + soil1(j)%sta(1)%p > 1.e-4) then
            cpr = r4 / (hru(j)%rsd_flt(ipl)%pmass + soil1(j)%sta(1)%p)
            if (cpr > 5000.) cpr = 5000.
            cprf = Exp(-.693 * (cpr - 200.) / 200.)
          else
            cprf = 1.
          end if
        
          !! compute soil water factor
          sut = 0.
	      !! change for domain error 1/29/09 gsm check with Jeff !!!
	      if (soil(j)%phys(1)%st < 0.) soil(j)%phys(1)%st = .0000001
          sut = .1 + .9 * Sqrt(soil(j)%phys(1)%st/soil(j)%phys(1)%fc)
          sut = Max(.05, sut)

          !!compute soil temperature factor
          xx = soil(j)%phys(1)%tmp
          cdg = .9 * xx / (xx + Exp(9.93 - .312 * xx)) + .1
          cdg = Max(.1, cdg)

          !! compute combined factor
          xx = cdg * sut
          if (xx < 0.) xx = 0.
          if (xx > 1.e6) xx = 1.e6
          csf = Sqrt(xx)
          ca = Min(cnrf, cprf, 1.)
          !! compute residue decomp and mineralization for each plant
          if (pcom(j)%npl > 0) then
            idp = pcom(j)%plcur(ipl)%idplt
            decr = pldb(idp)%rsdco_pl * ca * csf
          else
            decr = 0.05
          end if
          decr = Max(bsn_prm%decr_min, decr)
          decr = Min(decr, 1.)
          hru(j)%rsd_flt(ipl)%mass = Max(1.e-6, hru(j)%rsd_flt(ipl)%mass)
          rdc = decr * hru(j)%rsd_flt(ipl)%mass
          hru(j)%rsd_flt(ipl)%mass = hru(j)%rsd_flt(ipl)%mass - rdc
          if (hru(j)%rsd_flt(ipl)%mass < 0.) hru(j)%rsd_flt(ipl)%mass = 0.
          rmn1 = decr * hru(j)%rsd_flt(ipl)%nmass 
          hru(j)%rsd_flt(ipl)%pmass = Max(1.e-6,hru(j)%rsd_flt(ipl)%pmass)
          rmp = decr * hru(j)%rsd_flt(ipl)%pmass

          hru(j)%rsd_flt(ipl)%pmass = hru(j)%rsd_flt(ipl)%pmass - rmp
          hru(j)%rsd_flt(ipl)%nmass  = Max(1.e-6,hru(j)%rsd_flt(ipl)%nmass)
          hru(j)%rsd_flt(ipl)%nmass  = hru(j)%rsd_flt(ipl)%nmass  - rmn1
          soil1(j)%no3(1) = soil1(j)%no3(1) + .8 * rmn1
          soil1(j)%act(1)%n = soil1(j)%act(1)%n + .2 * rmn1
          soil1(j)%sta(1)%p = soil1(j)%sta(1)%p + .8 * rmp
          soil(j)%nut(1)%orgp = soil(j)%nut(1)%orgp + .2 * rmp
        end do
      end if
          
      !! compute residue decomp and mineralization of fresh organic n and p
      !! root and incorporated residue 
      do k = 1, soil(j)%nly

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
          rwn = .1e-4 * (soil1(j)%act(k)%n * (1. / nactfr - 1.) - soil1(j)%sta(k)%n)
          if (rwn > 0.) then
            rwn = Min(rwn, soil1(j)%act(k)%n)
          else
            rwn = -(Min(Abs(rwn), soil1(j)%sta(k)%n))
          endif
          soil1(j)%sta(k)%n = Max(1.e-6, soil1(j)%sta(k)%n + rwn)
          soil1(j)%act(k)%n = Max(1.e-6, soil1(j)%act(k)%n - rwn)

          !! compute humus mineralization on active organic n
          hmn = bsn_prm%cmn * csf * soil1(j)%act(k)%n
          hmn = Min(hmn, soil1(j)%act(k)%n)
          !! compute humus mineralization on active organic p
          xx = soil1(j)%sta(k)%n + soil1(j)%act(k)%n
          if (xx > 1.e-6) then
            hmp = 1.4 * hmn * soil1(j)%sta(k)%p / xx
          else
            hmp = 0.
          end if
          hmp = Min(hmp, soil1(j)%sta(k)%p)
          !! move mineralized nutrients between pools
          soil1(j)%act(k)%n = Max(1.e-6, soil1(j)%act(k)%n - hmn)
          soil1(j)%no3(k) = soil1(j)%no3(k) + hmn
          soil1(j)%sta(k)%p = soil1(j)%sta(k)%p - hmp
          soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + hmp

          !! compute residue decomp and mineralization of 
          !! fresh organic n and p (upper two layers only)
          rmn1 = 0.
          rmp = 0.
          if (k <= 2) then
            r4 = .58 * soil(j)%ly(k)%rsd
            if (rsd(j)%rsd_tfl%n + soil1(j)%no3(k) > 1.e-4) then
              cnr = r4 / (rsd(j)%rsd_tfl%n  + soil1(j)%no3(k))
              if (cnr > 500.) cnr = 500.
              cnrf = Exp(-.693 * (cnr - 25.) / 25.)
            else
              cnrf = 1.
            end if
            
            if (rsd(j)%rsd_tfl%p + soil(j)%nut(k)%solp > 1.e-4) then
              cpr = r4 / (rsd(j)%rsd_tfl%p + soil(j)%nut(k)%solp)
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
            !do ipl = 1, pcom(j)%npl        !! we need to decomp each plant**
              
            if (pcom(j)%npl > 0) then
              decr = rsdco_plcom(j) * ca * csf
            else
              decr = 0.05
            end if
            decr = Max(bsn_prm%decr_min, decr)
            decr = Min(decr, 1.)
            soil(j)%ly(k)%rsd = Max(1.e-6, soil(j)%ly(k)%rsd)
            rdc = decr * soil(j)%ly(k)%rsd
            soil(j)%ly(k)%rsd = soil(j)%ly(k)%rsd - rdc
            if (soil(j)%ly(k)%rsd < 0.) soil(j)%ly(k)%rsd = 0.
            rmn1 = decr * rsd(j)%rsd_tfl%n 
            rsd(j)%rsd_tfl%p = Max(1.e-6,rsd(j)%rsd_tfl%p)
            rmp = decr * rsd(j)%rsd_tfl%p

            rsd(j)%rsd_tfl%p = rsd(j)%rsd_tfl%p - rmp
            rsd(j)%rsd_tfl%n  = Max(1.e-6,rsd(j)%rsd_tfl%n)
            rsd(j)%rsd_tfl%n  = rsd(j)%rsd_tfl%n  - rmn1
            soil1(j)%no3(k) = soil1(j)%no3(k) + .8 * rmn1
            soil1(j)%act(k)%n = soil1(j)%act(k)%n + .2 * rmn1
            soil(j)%nut(k)%solp = soil(j)%nut(k)%solp + .8 * rmp
            soil1(j)%sta(k)%p = soil1(j)%sta(k)%p + .2 * rmp
          end if
!! septic changes 1/28/09 gsm
!!  compute denitrification
        wdn = 0.   
	  if (i_sep(j) /= k .or. sep(isep)%opt  /= 1) then
	    if (sut >= bsn_prm%sdnco) then
	      wdn = soil1(j)%no3(k) * (1.-Exp(-bsn_prm%cdn * cdg * soil(j)%cbn(k)%cbn))
	    else
	      wdn = 0.
	    endif
	    soil1(j)%no3(k) = soil1(j)%no3(k) - wdn
	  end if
! septic changes 1/28/09 gsm

!			call nut_denit(k,j,cdg,wdn,0.05)

        end if
      end do
      return
      end subroutine nut_nminrl1