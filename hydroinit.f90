      subroutine hydroinit 

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine computes variables related to the watershed hydrology:
!!    the time of concentration for the subbasins, lagged surface runoff,
!!    the coefficient for the peak runoff rate equation, and lateral flow travel
!!    time.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~1
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ch_n(1,:)   |none          |Manning's "n" value for the tributary channels
!!    ch_s(1,:)   |m/m           |average slope of tributary channels
!!    gdrain(:)   |hrs           |drain tile lag time: the amount of time
!!                               |between the transfer of water from the soil
!!                               |to the drain tile and the release of the
!!                               |water from the drain tile to the reach.
!!    hru_dafr(:) |km2/km2       |fraction of total watershed area contained
!!                               |in HRU
!!    hru_km(:)   |km2           |area of HRU in square kilometers
!!    hru_slp(:)  |m/m           |average slope steepness
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    lat_ttime(:)|days          |lateral flow travel time
!!   tile_ttime(:)|none          |Exponential of the tile flow travel time
!!    ldrain(:)   |none          |soil layer where drainage tile is located
!!    nhru        |none          |number of HRUs in watershed
!!    slsoil(:)   |m             |slope length for lateral subsurface flow
!!    slsubbsn(:) |m             |average slope length for subbasin
!!    sub_fr(:)   |none          |fraction of total watershed area contained in
!!                               |subbasin
!!    tconc(:)     |hr           |time of concentration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    brt(:)      |none          |fraction of surface runoff within the subbasin
!!                               |which takes 1 day or less to reach the 
!!                               |subbasin outlet
!!    lat_ttime(:)|none          |Exponential of the lateral flow travel time
!!    tile_ttime(:)|none         |Exponential of the tile flow travel time
!!    tconc(:)   |hr             |time of concentration for hru
!!    t_ov(:)     |hr            |time for flow from farthest point in subbasin
!!                               |to enter a channel
!!    tconc(:)    |hr            |time of concentration for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |counter
!!    l           |none          |counter
!!    scmx        |mm/hr         |maximum soil hydraulic conductivity
!!    t_ch        |hr            |time for flow entering the farthest upstream 
!!                               |channel to reach the subbasin outlet
!!    xx          |none          |variable to hold calculation result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~  
!!    SWAT: Ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      use climate_parms
      use jrw_datalib_module
      use basin_module
      use channel_module
      use time_module
      

      integer :: j, l
      real :: t_ch, scmx, xx, tsoil

      do j = 1, mhru
       iob = hru(j)%obj_no
       iwst = ob(iob)%wst
       iwgn = wst(iwst)%wco%wgn
       
       usle_mult(j) = soil(j)%phys(1)%rock * soil(j)%ly(1)%usle_k *       &
                                 hru(j)%luse%usle_p * usle_ls(j) * 11.8

!      if (rsdin(j) > 0.) soil(j)%ly(1)%rsd = rsdin(j)

      tsoil = (wgn(iwgn)%tmpmx(12) + wgn(iwgn)%tmpmx(12)) / 2.
      !! should be beginning month of simulation and not 12 (December)

!!    set fraction of field capacity in soil
      if (bsn_prm%ffcb <= 0.) then
       sffc = wgn_pms(iwgn)%pcp_an / (wgn_pms(iwgn)%pcp_an + Exp(9.043 -   &
                                     .002135 * wgn_pms(iwgn)%pcp_an))
                         !!S-curve equation Jeff made up.
      else
        sffc = bsn_prm%ffcb
      end if
      
      !! set initial soil water and temperature for each layer
      nly = hru(j)%sol%nly
      hru(j)%sol%sw = 0.
      do k = 1, nly
        soil(j)%phys(k)%tmp = tsoil
        soil(j)%phys(k)%st = sffc * soil(j)%phys(k)%fc
        hru(j)%sol%sw = hru(j)%sol%sw + soil(j)%phys(k)%st
      end do
      
      !! set day length threshold for dormancy
      dormhr(j) = wgn_pms(iwgn)%daylth

!!    compare maximum rooting depth in soil to maximum rooting depth of plant
      if (hru(j)%sol%zmx<= 0.001) hru(j)%sol%zmx = soil(j)%phys(nly)%d
      plt_zmx = 0.
      do ipl = 1, npl(j)
        idp = pcom(j)%plcur(ipl)%idplt
	    if (idp > 0) then
          if (pldb(idp)%idc > 0) then
            !! set initial residue by summing each plant
            soil(j)%ly(1)%rsd = soil(j)%ly(1)%rsd + hru(j)%rsd_flt(ipl)%mass
            plt_zmxp = plt_zmx
            plt_zmx = 1000. * pldb(idp)%rdmx
            plt_zmx = amax1(plt_zmx,plt_zmxp)
          end if
        end if
      end do
      if (hru(j)%sol%zmx > 1. .and. plt_zmx > 1.) then
         hru(j)%sol%zmx = Min(hru(j)%sol%zmx,plt_zmx)
      else
         !! if one value is missing it will set to the one available
         hru(j)%sol%zmx = Max(hru(j)%sol%zmx,plt_zmx)
      end if

!! create a biozone layer in septic HRUs
      isep = iseptic(j)
      if (sep(isep)%opt  /= 0) then 
	 if (sep(isep)%z + sep(isep)%thk > soil(j)%phys(nly)%d) then
	   if (soil(j)%phys(nly)%d > sep(isep)%thk + 10.) then !min. soil thickness for biozone layer (10mm top+biozone layer thickness)
	      sep(isep)%z = soil(j)%phys(nly)%d - sep(isep)%thk
	   else
	      sep(isep)%z = soil(j)%phys(nly)%d
	      soil(j)%phys(nly)%d = soil(j)%phys(nly)%d + sep(isep)%thk
	   endif
       endif 
       if (sep(isep)%z > 0.) then 
         call layersplit (sep(isep)%z)
         dep_new = sep(isep)%z + sep(isep)%thk
         call layersplit (dep_new)  
         i_sep(j) = isep_ly
       endif    
      endif
          
!!    calculate sol_kp as function of koc and sol_cbn
!!    and set initial pesticide in all layers equal to value given for
!!    upper layer
      if (hrupest(j) == 1) then
      do k = 1, npmx
        jj = 0
        jj = npno(k)
        if (jj > 0) then
          solpst = 0.
          solpst = hru(j)%ly(1)%pst(k)  !!concentration of pesticide in soil
          
          xx = 0.
          do n = 1, nly
            dg = 0.
            wt1 = 0.
            dg = (soil(j)%phys(n)%d - xx)
            xx = soil(j)%phys(n)%d 
            wt1 = soil(j)%phys(n)%bd * dg / 100.      !! mg/kg => kg/ha
!!            sol_kp(k,j,n) = pestdb(jj)%skoc * soil(j)%cbn(n)%cbn / 100.
            hru(j)%ly(n)%kp(k) = pestdb(jj)%skoc *                         &
                                             soil(j)%cbn(n)%cbn / 100.
            hru(j)%ly(n)%pst(k) = solpst * wt1
            
          end do
        end if
      end do
      end if
  
      isol = hru(j)%dbs%soil
      ilu = hru(j)%luse%cn_lu
      select case (sol(isol)%s%hydgrp)
      case ('A')
        cn2(j) = cn(ilu)%cn(1)
      case ('B')
        cn2(j) = cn(ilu)%cn(2)
      case ('C')
        cn2(j) = cn(ilu)%cn(3)
      case ('D')
        cn2(j) = cn(ilu)%cn(4)
      end select

      do ly = 1, hru(j)%sol%nly
        if (soil(j)%ly(ly)%pperco_sub <= 1.e-6)                          &
             soil(j)%ly(ly)%pperco_sub = bsn_prm%pperco
      end do

!!    compute lateral flow travel time
        if (hru(j)%hyd%lat_ttime <= 0.) then
            scmx = 0.
            do l = 1, hru(j)%sol%nly
              if (soil(j)%phys(l)%k > scmx) then
                scmx = soil(j)%phys(l)%k
              endif
            end do
            !! unit conversion:
            !! xx = m/(mm/h) * 1000.(mm/m)/24.(h/d) / 4.
            xx = 0.
            xx = 10.4 * hru(j)%topo%lat_len / scmx
            if (xx < 1.) xx = 1.
            hru(j)%hyd%lat_ttime = 1. - Exp(-1./xx)
        else
          hru(j)%hyd%lat_ttime = 1. -                                   & 
                     Exp(-1./hru(j)%hyd%lat_ttime)
        end if

        if (ldrain(j) > 0 .and. sdr(isdr)%lag > 0.01) then
            tile_ttime(j) = 1. - Exp(-24. / sdr(isdr)%lag)
        else
            tile_ttime(j) = 0.
        end if
      end do

      return
      end