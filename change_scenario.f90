      subroutine change_scenario (chg, ihru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida           |julian date   |day being simulated (current julian date)
!!    inum1          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer :: j, plant_no , zz
      real:: b, xcont_cn
      type (change_parms) :: chg

      select case(chg%name)
     
      case ("terrace")
        terr_p = terrace_db(istr)%p
        terr_cn = terrace_db(istr)%cn2
        terr_sl = terrace_db(istr)%sl_len
        xm = 0.6 * (1. - Exp(-35.835 * hru(j)%topo%slope))    
        sin_sl = Sin(Atan(hru(j)%topo%slope))
        usle_ls(ihru) = (terr_sl / 22.128) ** xm * (65.41 * sin_sl *    &
                                     sin_sl + 4.56 * sin_sl + .065)
        usle_mult(ihru) = soil(ihru)%phys(1)%rock*hru(ihru)%sol%usle_k  &
                                       * terr_p * usle_ls(ihru) * 11.8
        if (terr_cn > 1.e-6) then
           call curno(terr_cn,ihru)
        end if

      case ("tile")
        isdr_no(ihru) = istr

        !! define soil layer that the drainage tile is in
        if (sdr(istr)%depth > 0) then
          do jj = 1, hru(ihru)%sol%nly
            if (sdr(istr)%depth < soil(ihru)%phys(jj)%d)                & 
               ldrain(ihru) = jj
            if (sdr(istr)%depth < soil(ihru)%phys(jj)%d) exit
          end do
        else
          ldrain(ihru) = 0
        endif
        !! setting tile lage time
      if (ldrain(j) > 0 .and. sdr(istr)%lag > 0.01) then
            tile_ttime(j) = 1. - Exp(-24. /sdr(istr)%lag)
         else
            tile_ttime(j) = 0.
         end if

      case ("contour")
        cont_cn = contour_db(istr)%cont_cn
        cont_p = contour_db(istr)%cont_p
        usle_mult(ihru) = usle_mult(ihru) * cont_p /                     &
             hru(j)%luse%usle_p
        call curno(cont_cn,ihru)

      case ("filter")
        vfsi(ihru) = filtstrip_db(istr)%vfsi
        vfsratio(ihru) = filtstrip_db(istr)%vfsratio
        vfscon(ihru) = filtstrip_db(istr)%vfscon
        vfsch(ihru) = filtstrip_db(istr)%vfsch

        !! Set some defaults if needed
        if (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
        !! minimum value for vfsratio is 0 max is 300
        if (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
        if (vfsratio(ihru) > 300) vfsratio(ihru) = 300
        !! minimum value for vfscon is 0.1 default is 0.5 max is 0.95
        if (vfscon(ihru) <= 0) vfscon(ihru) = 0.5
        if (vfscon(ihru) <= 0.1) vfscon(ihru) = 0.1
        if (vfscon(ihru) > 0.95) vfscon(ihru) = 0.95
        !! minimum value for vfsch is 0 max is .95
        if (vfsch(ihru) <= 0.) vfsch(ihru) = 0.
        if (vfsch(ihru) > .95) vfsch(ihru) = .95

      case ("stripcrop")
        strip_n = stripcrop_db(istr)%strip_n
        strip_cn = stripcrop_db(istr)%strip_cn
        strip_c = stripcrop_db(istr)%strip_c
        strip_p = stripcrop_db(istr)%strip_p

         usle_mult(ihru) = usle_mult(ihru) * strip_p /                  &  
           hru(j)%luse%usle_p
         tover = .0556 * (hru(j)%topo%slope_len *                       &
           strip_n) ** .6 / hru(j)%topo%slope ** .3  

         tconc(ihru) = tconc(ihru) + tover - t_ov(ihru)
         call curno(strip_cn,ihru)

      case ("fire")
        fire_cn = fire_db(istr)%fire_cn
        call curno (fire_cn,ihru)

      case ("grassww")
        if (ngrwat(ihru) < 0)  ngrwat(ihru) = 0
 !         ngrwat(ihru) = ngrwat(ihru) + 1
          grwat_i(ihru) = grwaterway_db(istr)%grwat_i
          grwat_n(ihru) = grwaterway_db(istr)%grwat_n
          grwat_spcon(ihru) = grwaterway_db(istr)%grwat_spcon
          grwat_d(ihru) = grwaterway_db(istr)%grwat_d
          grwat_w(ihru) = grwaterway_db(istr)%grwat_w
          grwat_l(ihru) = grwaterway_db(istr)%grwat_l
          grwat_s(ihru) = grwaterway_db(istr)%grwat_s
          !! Set defaults - Mannings via Fiener, 2006
          if (grwat_n(ihru) <=0.) grwat_n(ihru) = 0.35 
          !! length based on one side of a square HRU
          if (grwat_l(ihru) <=0.) grwat_l(ihru) = hru(ihru)%km**.5
          !! default to a workable depth based on with and 8:1 sideslopes
          if (grwat_d(ihru) <= 0.) then
            grwat_d(ihru) = 3. / 64. * grwat_w(ihru)
          end if
          !! Default to 3/4 HRU slope
          if (grwat_s(ihru) <=0.) grwat_s(ihru) = hru_slp(ihru)*.75
          !! default sed coeff to 0.005
          if (grwat_spcon(ihru)<= 0.) grwat_spcon(ihru) = 0.005

          !! Calculate time of concentration for waterway similar to hydroinit.f
          tch = .62 * grwat_l(ihru) * grwat_n(ihru) ** .6 /              &
             (hru(ihru)%km ** .125 * grwat_s(ihru) ** .375)
          tc_gwat(ihru) = tch + t_ov(ihru)
          !! Set counter
          k = mhru + ngrwat(ihru)
          !!Check the channel to make sure the enter width and depth will work with 8:1 trap channel, assume width is correct
          b = grwat_w(ihru) - 2. * grwat_d(ihru) * 8
          !! Depth and Width not possible with 8:1 sideslope and trapazoidal channel assume b =.25*width
          if (b <= 0.) grwat_d(ihru) = 3. / 64. * grwat_w(ihru)

          call ttcoef_wway
              
      case ("plantup")
          plant_no = plparmup_db(istr)%plant_no
          pldb(plant_no)%hvsti = plparmup_db(istr)%hvsti
          pldb(plant_no)%blai = plparmup_db(istr)%blai
          
      case ("resman")
        !! Implement Residue Management MJW
        if (str%op1 == 1)  then
          min_res(ihru) = rsdmgt_db(irm)%min_res
        else
          min_res(ihru) = 0.
        end if

      case ("user_def")                 !user defined Upland CP removal MJW
          bmp_flag(ihru) = 1
          bmp_sed(ihru) = bmpuser_db(istr)%bmp_sed !! Sediment
          bmp_pp(ihru) = bmpuser_db(istr)%bmp_pp   !! Particulate P
          bmp_sp(ihru) = bmpuser_db(istr)%bmp_sp   !! Soluble P
          bmp_pn(ihru) =  bmpuser_db(istr)%bmp_pn  !! Particulate N
          bmp_sn(ihru) = bmpuser_db(istr)%bmp_sn   !! Soluble N
          bmp_bac(ihru) = bmpuser_db(istr)%bmp_bac !! Bacteria
      
      case ("septic")
        iseptic(ihru) = istr

      end select

      return
      end subroutine change_scenario