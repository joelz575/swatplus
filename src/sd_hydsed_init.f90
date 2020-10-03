     subroutine sd_hydsed_init

      use input_file_module
      use sd_channel_module
      use channel_velocity_module
      use maximum_data_module
      use hydrograph_module
      use constituent_mass_module
      use pesticide_data_module

      implicit none

      real :: kh
      integer :: idb                  !             |
      integer :: i                    !none         |counter
      integer :: icon, iob, ichdat
      integer :: ich_ini                !none      |counter
      integer :: iom_ini                !none      |counter
      integer :: ipest_ini              !none      |counter
      integer :: ipest_db               !none      |counter
      integer :: ipath_ini              !none      |counter
      integer :: ipest                  !none      |counter
      integer :: ipath                  !none      |counter

      real :: aa                      !none         |area/area=1 (used to calculate velocity with
                                      !             |Manning"s equation)
      real :: a                       !m^2          |cross-sectional area of channel
      real :: b                       !m            |bottom width of channel
      real :: d                       !m            |depth of flow
      real :: p                       !m            |wetting perimeter
      real :: chside                  !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on channel side
                                      !             |slopes; always set to 2 (slope=1/2)
      real :: fps                     !none         |change in horizontal distance per unit
                                      !             |change in vertical distance on floodplain side
                                      !             |slopes; always set to 4 (slope=1/4)
      integer :: max                  !             |
      real :: rh                      !m            |hydraulic radius
      real :: qman                    !m^3/s or m/s |flow rate or flow velocity
      real :: tt1                     !km s/m       |time coefficient for specified depth
      real :: tt2                     !km s/m       |time coefficient for bankfull depth
      real :: qq1                     !m^3/s        |flow rate for a specified depth
      real :: bedvol                    !m^3       |volume of river bed sediment

      do i = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + i - 1
        idb = ob(icmd)%props
        sd_ch(i)%name = sd_chd(idb)%name
        sd_ch(i)%order = sd_chd(idb)%order
        sd_ch(i)%chw = sd_chd(idb)%chw
        sd_ch(i)%chd = sd_chd(idb)%chd
        sd_ch(i)%chs = sd_chd(idb)%chs
        sd_ch(i)%chl = sd_chd(idb)%chl
        sd_ch(i)%chn = sd_chd(idb)%chn
        sd_ch(i)%cherod = sd_chd(idb)%cherod
        sd_ch(i)%cov = sd_chd(idb)%cov
        sd_ch(i)%shear_bnk = sd_chd(idb)%shear_bnk
        sd_ch(i)%hc_len = sd_chd(idb)%hc_ini
        sd_ch(i)%hc_hgt = sd_chd(idb)%hc_hgt

        !! compute headcut parameters
        kh = sd_chd(idb)%hc_kh
        if (kh > 1.e-6) then
          sd_ch(i)%hc_co = .37 * (17.83 + 16.56 * kh - 15. * sd_chd(idb)%hc_cov)
          sd_ch(i)%hc_co = amax1 (0., sd_ch(i)%hc_co)
        else
          sd_ch(i)%hc_co = 0.
        end if

        !! compute travel time coefficients
        aa = 1.
        b = 0.
        d = 0.
        chside = sd_chd(idb)%chss
        fps = 4.
        b = sd_ch(i)%chw - 2. * sd_ch(i)%chd * chside

        !! check IF bottom width (b) is < 0
        if (b <= 0.) then
            b = .5 * sd_ch(i)%chw
            b = Max(0., b)
            chside = (sd_ch(i)%chw - b) / (2. * sd_ch(i)%chd)
        end if
        sd_ch_vel(i)%wid_btm = b
        sd_ch_vel(i)%dep_bf = sd_ch(i)%chd

        !! compute flow and travel time at bankfull depth
        p = b + 2. * sd_ch(i)%chd * Sqrt(chside * chside + 1.)
        a = b * sd_ch(i)%chd + chside * sd_ch(i)%chd * sd_ch(i)%chd
        rh = a / p
        sd_ch_vel(i)%area = a
        sd_ch_vel(i)%vel_bf = Qman(a, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        sd_ch_vel(i)%velav_bf = Qman(aa, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        sd_ch_vel(i)%celerity_bf = sd_ch_vel(i)%velav_bf * 5. / 3.
        sd_ch_vel(i)%st_dis = sd_ch(i)%chl / sd_ch_vel(i)%celerity_bf / 3.6
        tt2 = sd_ch(i)%chl * a / sd_ch_vel(i)%vel_bf

        !! compute flow and travel time at 1.2 bankfull depth
        d = 1.2 * sd_ch(i)%chd
        a = a + (sd_ch(i)%chw * sd_ch(i)%chd + fps * (sd_ch(i)%chd - sd_ch(i)%chd) ** 2)
        p = p + 4. * sd_ch(i)%chw + (0.4 * sd_ch(i)%chd * Sqrt(fps * fps + 1.))
        rh = a / p
        qq1 = Qman(a, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        tt1 = sd_ch(i)%chl * a / qq1

        !! compute flow and travel time at 0.1 bankfull depth
        d = 0.1 * sd_ch(i)%chd
        p = b + 2. * sd_ch(i)%chd * Sqrt(chside * chside + 1.)
        a = b * sd_ch(i)%chd + chside * sd_ch(i)%chd * sd_ch(i)%chd
        rh = a / p
        qq1 = Qman(a, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        tt1 = sd_ch(i)%chl * a / qq1
        sd_ch_vel(i)%vel_1bf = Qman(aa, rh, sd_ch(i)%chn, sd_ch(i)%chs)
        sd_ch_vel(i)%celerity_1bf = sd_ch_vel(i)%vel_1bf * 5. / 3.
        sd_ch_vel(i)%stor_dis_1bf = sd_ch(i)%chl / sd_ch_vel(i)%celerity_1bf / 3.6
      end do


      ! initialize organics-minerals in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        ! only initialize storage for real channels (length > 1 m)
        if (sd_ch(ich)%chl > 1.e-3) then
          iob = sp_ob1%chandeg + ich - 1
          ichdat = ob(iob)%props
          ich_ini = sd_dat(ichdat)%init
          iom_ini = sd_init(ich_ini)%org_min
          ch_stor(ich) = om_init_water(iom_ini)
          ch_om_water_init(ich) = ch_stor(ich)
        else
          ch_stor(ich) = hz
        end if
      end do

      ! initialize pesticides in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ich - 1
        ichdat = ob(iob)%props
        ich_ini = sd_dat(ichdat)%init
        ipest_ini = sd_init(ich_ini)%pest
        do ipest = 1, cs_db%num_pests
          ipest_db = cs_db%pest_num(ipest)
          ! mg = mg/kg * m3*1000. (kg=m3*1000.)
          ch_water(ich)%pest(ipest) = pest_water_ini(ipest_ini)%water(ipest) * ch_stor(ich)%flo * 1000.
          !! calculate volume of active river bed sediment layer - m3
          bedvol = sd_ch(ich)%chw *sd_ch(ich)%chl * 1000.* pestdb(ipest_ini)%ben_act_dep
          ch_benthic(ich)%pest(ipest) = pest_water_ini(ipest_ini)%benthic(ipest) * bedvol * 1000.   ! mg = mg/kg * m3*1000.
          !! calculate mixing velocity using molecular weight and porosity
          sd_ch(ich)%aq_mix(ipest) = pestdb(ipest_db)%mol_wt ** (-.6666) * (1. - sd_chd(ich)%bd / 2.65) * (69.35 / 365)
        end do
      end do

      ! initialize pathogens in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + ich - 1
        ichdat = ob(iob)%props
        ich_ini = sd_dat(ichdat)%init
        ipath_ini = sd_init(ich_ini)%path
        do ipath = 1, cs_db%num_paths
          ch_water(ich)%path(ipath) = path_water_ini(ipest_ini)%water(ipath)
          ch_benthic(ich)%path(ipath) = path_water_ini(ipest_ini)%benthic(ipath)
        end do
      end do

      return
      end subroutine sd_hydsed_init