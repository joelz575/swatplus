      subroutine structure_set_parms (str_name, istr, j)

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

      use jrw_datalib_module
      use parm
    
      character (len=16), intent (in) :: str_name
      integer, intent (in) :: istr, j

      select case(str_name)
     
      case ("terrace")
        terr_p = terrace_db(istr)%p
        terr_cn = terrace_db(istr)%cn2
        terr_sl = terrace_db(istr)%sl_len
        xm = 0.6 * (1. - Exp(-35.835 * hru(j)%topo%slope))
        sin_sl = Sin(Atan(hru(j)%topo%slope))
        usle_ls(j) = (terr_sl / 22.128) ** xm * (65.41 * sin_sl *    &
                                     sin_sl + 4.56 * sin_sl + .065)
        usle_mult(j) = soil(j)%phys(1)%rock*hru(j)%sol%usle_k  &
                                       * terr_p * usle_ls(j) * 11.8
        if (terr_cn > 1.e-6) then
           call curno(terr_cn,j)
        end if

      case ("tiledrain")
        isdr_no(j) = istr
        hru(j)%sdr_dep = sdr(istr)%depth
        !! define soil layer that the drainage tile is in
        if (sdr(istr)%depth > 0) then
          do jj = 1, hru(j)%sol%nly
            if (hru(j)%sdr_dep < soil(j)%phys(jj)%d) ldrain(j) = jj
            if (hru(j)%sdr_dep < soil(j)%phys(jj)%d) exit
          end do
        else
          ldrain(j) = 0
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
        usle_mult(j) = usle_mult(j) * cont_p / hru(j)%luse%usle_p
        call curno(cont_cn,j)

      case ("filter")
        vfsi(j) = filtstrip_db(istr)%vfsi
        vfsratio(j) = filtstrip_db(istr)%vfsratio
        vfscon(j) = filtstrip_db(istr)%vfscon
        vfsch(j) = filtstrip_db(istr)%vfsch

        !! Set some defaults if needed
        if (vfsratio(j) <= 0.) vfsratio(j) = 0.
        !! minimum value for vfsratio is 0 max is 300
        if (vfsratio(j) <= 0.) vfsratio(j) = 0.
        if (vfsratio(j) > 300) vfsratio(j) = 300
        !! minimum value for vfscon is 0.1 default is 0.5 max is 0.95
        if (vfscon(j) <= 0) vfscon(j) = 0.5
        if (vfscon(j) <= 0.1) vfscon(j) = 0.1
        if (vfscon(j) > 0.95) vfscon(j) = 0.95
        !! minimum value for vfsch is 0 max is .95
        if (vfsch(j) <= 0.) vfsch(j) = 0.
        if (vfsch(j) > .95) vfsch(j) = .95

      case ("stripcrop")
        strip_n = stripcrop_db(istr)%strip_n
        strip_cn = stripcrop_db(istr)%strip_cn
        strip_c = stripcrop_db(istr)%strip_c
        strip_p = stripcrop_db(istr)%strip_p

         usle_mult(j) = usle_mult(j) * strip_p / hru(j)%luse%usle_p
         tover = .0556 * (hru(j)%topo%slope_len * strip_n) ** .6 / hru(j)%topo%slope ** .3  

         tconc(j) = tconc(j) + tover - t_ov(j)
         call curno(strip_cn,j)

      case ("fire")
        fire_cn = fire_db(istr)%fire_cn
        call curno (fire_cn,j)

      case ("grassww")
        if (ngrwat(j) < 0)  ngrwat(j) = 0
 !         ngrwat(j) = ngrwat(j) + 1
          grwat_i(j) = grwaterway_db(istr)%grwat_i
          grwat_n(j) = grwaterway_db(istr)%grwat_n
          grwat_spcon(j) = grwaterway_db(istr)%grwat_spcon
          grwat_d(j) = grwaterway_db(istr)%grwat_d
          grwat_w(j) = grwaterway_db(istr)%grwat_w
          grwat_l(j) = grwaterway_db(istr)%grwat_l
          grwat_s(j) = grwaterway_db(istr)%grwat_s
          !! Set defaults - Mannings via Fiener, 2006
          if (grwat_n(j) <=0.) grwat_n(j) = 0.35 
          !! length based on one side of a square HRU
          if (grwat_l(j) <=0.) grwat_l(j) = hru(j)%km**.5
          !! default to a workable depth based on with and 8:1 sideslopes
          if (grwat_d(j) <= 0.) then
            grwat_d(j) = 3. / 64. * grwat_w(j)
          end if
          !! Default to 3/4 HRU slope
          if (grwat_s(j) <=0.) grwat_s(j) = hru_slp(j)*.75
          !! default sed coeff to 0.005
          if (grwat_spcon(j)<= 0.) grwat_spcon(j) = 0.005

          !! Calculate time of concentration for waterway similar to hydroinit.f
          tch = .62 * grwat_l(j) * grwat_n(j) ** .6 /              &
             (hru(j)%km ** .125 * grwat_s(j) ** .375)
          tc_gwat(j) = tch + t_ov(j)
          !! Set counter
          k = mhru + ngrwat(j)
          !!Check the channel to make sure the enter width and depth will work with 8:1 trap channel, assume width is correct
          b = grwat_w(j) - 2. * grwat_d(j) * 8
          !! Depth and Width not possible with 8:1 sideslope and trapazoidal channel assume b =.25*width
          if (b <= 0.) grwat_d(j) = 3. / 64. * grwat_w(j)

          call ttcoef_wway
              
      case ("plantup")
          iplant_no = plparmup_db(istr)%plant_no
          pldb(plant_no)%hvsti = plparmup_db(istr)%hvsti
          pldb(plant_no)%blai = plparmup_db(istr)%blai
          
      case ("resman")
        !! Implement Residue Management MJW
        if (str%op1 == 1)  then
          min_res(j) = rsdmgt_db(irm)%min_res
        else
          min_res(j) = 0.
        end if

      case ("user_def")                 !user defined Upland CP removal MJW
          bmp_flag(j) = 1
          bmp_sed(j) = bmpuser_db(istr)%bmp_sed !! Sediment
          bmp_pp(j) = bmpuser_db(istr)%bmp_pp   !! Particulate P
          bmp_sp(j) = bmpuser_db(istr)%bmp_sp   !! Soluble P
          bmp_pn(j) =  bmpuser_db(istr)%bmp_pn  !! Particulate N
          bmp_sn(j) = bmpuser_db(istr)%bmp_sn   !! Soluble N
          bmp_bac(j) = bmpuser_db(istr)%bmp_bac !! Bacteria
      
      case ("septic")
        iseptic(j) = istr

      end select

      return
      end subroutine structure_set_parms