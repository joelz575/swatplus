      subroutine swr_percmain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drainmod tile equations   08/2006
!!	dep_imp(:)	|mm            |depth to impervious layer
!!    drainmod tile equations   08/2006
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    new water table depth  equations   01/2009
!!	c			|none		   |a factor used to convert airvol to wtd
!!	deep_p      |mm			   |total thickness of soil profile in HRU
!!    dg          |mm			   |soil layer thickness in HRU
!!    new water table depth  equations   01/2009
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    new water table depth  equations   01/2009
!!	ne_p		|mm/hr		   |effective porosity in HRU for all soil profile layers 
!!	ne_w		|mm/hr		   |effective porosity in HRU for soil layers above wtd 
!!    new water table depth  equations   01/2009
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sepday      |mm H2O        |micropore percolation from soil layer
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    new water table depth  equations   01/2009
!!    wat		    |mm H2O        |shallow water table depth below the soil surface to up to impervious layer
!!    new water table depth  equations   01/2009
!!    wt_shall    |mm H2O        |shallow water table depth above the impervious layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!	w2			|mm			   |
!!	y1			|mm 		   |dummy variable for wat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro, drains(wt_shall), origtile(wt_shall,d)

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use jrw_datalib_module
      use basin_module

      integer :: j, j1, nn, k

      j = ihru
      isdr = isdr_no(j)

      !! initialize water entering first soil layer

      if (bsn_cc%crk == 1) then
        sepday = Max(0., inflpcp - voltot)
      else
        sepday = inflpcp
      end if

!!  add irrigation water
	if (aird(j)>0) then
	  j=j
      end if
      !ht1%flo is infiltration from overland flow routing
      sepday = inflpcp + aird(j) + pot(j)%seep + ht1%flo
      pot(j)%seep = 0.

      !! calculate crack flow 
      if (bsn_cc%crk == 1) then 
	    call swr_percmacro
	    sepday = sepday - sepcrktot
	  endif

      !back to 4 mm slug for soil routing- keeps moisture above fc
      slug = 1000.   !this should be an input in parameters.bsn
      sep_left = sepday
      do                  !slug loop
        if (sep_left < 1.e-6) exit
        sepday = amin1(sepday, slug)
        sep_left = sep_left - sepday
        if (sep_left > slug) then
          sepday = sep_left
          sep_left = 0.
        end if
      do j1 = 1, hru(j)%sol%nly
        !! add water moving into soil layer from overlying layer
        soil(j)%phys(j1)%st = soil(j)%phys(j1)%st + sepday
        
 	  !! septic tank inflow to biozone layer  J.Jeong
	  ! STE added to the biozone layer if soil temp is above zero. 
	  if (j1 == i_sep(j) .and. soil(j)%phys(j1)%tmp > 0. .and.          &
              sep(isep)%opt  /= 0) then
		soil(j)%phys(j1)%st = soil(j)%phys(j1)%st + qstemm(j)  ! in mm
        end if

       !! determine gravity drained water in layer
        sw_excess = soil(j)%phys(j1)%st - soil(j)%phys(j1)%fc

        !! initialize variables for current layer
        sepday = 0.
        latlyr = 0.
        lyrtile = 0.

        if (sw_excess > 1.e-5) then
          !! calculate tile flow (lyrtile), lateral flow (latlyr) and
          !! percolation (sepday)
          call swr_percmicro(j1)

          soil(j)%phys(j1)%st=soil(j)%phys(j1)%st-sepday-latlyr-lyrtile
          soil(j)%phys(j1)%st = Max(1.e-6,soil(j)%phys(j1)%st)

          !! redistribute soil water if above field capacity (high water table)
          call swr_satexcess(j1)
        end if

        !! summary calculations
        if (j1 == hru(j)%sol%nly) then
          sepbtm(j) = sepbtm(j) + sepday
        endif
        latq(j) = latq(j) + latlyr
        qtile = qtile + lyrtile
        soil(j)%ly(j1)%flat = latlyr + lyrtile
        soil(j)%ly(j1)%prk = soil(j)%ly(j1)%prk + sepday
	  if (latq(j) < 1.e-6) latq(j) = 0.
        if (qtile < 1.e-6) qtile = 0.
        if (soil(j)%ly(j1)%flat < 1.e-6) soil(j)%ly(j1)%flat = 0.
      end do
      end do                    !slug loop

      
      !! update soil profile water
      hru(j)%sol%sw = 0.
      do j1 = 1, hru(j)%sol%nly
        hru(j)%sol%sw = hru(j)%sol%sw + soil(j)%phys(j1)%st
      end do

      !! compute shallow water table depth and tile flow
      qtile = 0.
      wt_shall = 0.    !CB 8/24/09
      wt_shall = hru(j)%hyd%dep_imp
      !! drainmod tile equations   08/11/2006
      if (soil(j)%phys(2)%tmp > 0.) then   !Daniel 1/29/09
        por_air = 0.5
        !! d = hru(j)%hyd%dep_imp - sdr(isdr)%depth
        !! drainmod wt_shall equations   10/23/2006
        if (bsn_cc%wtdn == 0) then !compute wt_shall using original eq-Daniel 10/23/06
          if (hru(j)%sol%sw > hru(j)%sol%sumfc) then
            yy = hru(j)%sol%sumul * por_air
            if (yy < 1.1 * hru(j)%sol%sumfc) then
              yy = 1.1 * hru(j)%sol%sumfc
            end if
            xx = (hru(j)%sol%sw-hru(j)%sol%sumfc)/(yy-hru(j)%sol%sumfc)
            if (xx > 1.) xx = 1.
            wt_shall = xx * hru(j)%hyd%dep_imp
		    wat = hru(j)%hyd%dep_imp - wt_shall
			if(wat > hru(j)%hyd%dep_imp) wat = hru(j)%hyd%dep_imp
          end if
        else
          !compute water table depth using Daniel's modifications
          do j1 = 1, hru(j)%sol%nly
            if (hru(j)%sol%wat_tbl < soil(j)%phys(j1)%d) then
              sw_del = hru(j)%sol%swpwt - hru(j)%sol%sw
              wt_del = sw_del * hru(j)%ly(j1)%vwt
              hru(j)%sol%wat_tbl = hru(j)%sol%wat_tbl + wt_del
	        if(hru(j)%sol%wat_tbl > hru(j)%hyd%dep_imp)              &
                  hru(j)%sol%wat_tbl = hru(j)%hyd%dep_imp
	        wt_shall = hru(j)%hyd%dep_imp - hru(j)%sol%wat_tbl
	        hru(j)%sol%swpwt = hru(j)%sol%sw
	        exit
	      end if
	    end do
        end if
        !! drainmod wt_shall equations   10/23/2006
        
        if (isdr > 0) then
        if (sdr(isdr)%depth > 0.) then
          if (wt_shall <= d) then
            qtile = 0.
          else
            !! Start Daniel's tile equations modifications  01/2006
            if (bsn_cc%tdrn == 1) then
              call swr_drains     ! compute tile flow using drainmod tile equations 
              !! drainmod tile equations   01/2006
            else !! compute tile flow using existing tile equations
              call swr_origtile(d)! existing tile equations 
	        if(qtile < 0.) qtile=0.
            end if 
          end if
        end if
        end if
      end if
      !! End Daniel's tile equations modifications  01/2006

      if (qtile > 0.) then
        !! update soil profile water after tile drainage
        sumqtile = qtile
        do j1 = 1, hru(j)%sol%nly
          xx = soil(j)%phys(j1)%st - soil(j)%phys(j1)%fc
          if (xx > 0.) then
            if (xx > sumqtile) then
              soil(j)%phys(j1)%st = soil(j)%phys(j1)%st - sumqtile
              sumqtile = 0.
            else
              sumqtile = sumqtile - xx
              soil(j)%phys(j1)%st = soil(j)%phys(j1)%fc
            end if
          end if
        end do
        if (sumqtile > 0.) then
          qtile = qtile - sumqtile
          qtile = amax1(0., qtile)
        end if
      end if

      !! update soil profile water
      hru(j)%sol%sw = 0.
      do j1 = 1, hru(j)%sol%nly
        hru(j)%sol%sw = hru(j)%sol%sw + soil(j)%phys(j1)%st
      end do

      return
      end subroutine swr_percmain