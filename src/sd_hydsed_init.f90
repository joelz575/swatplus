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
      integer :: idat
      integer :: i_dep                  !none      |counter
      integer :: ifp_dep                !none      |counter
      
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
      real :: bedvol                  !m^3          |volume of river bed sediment
      
      real :: dep                     !             |
      real :: area                    !             |
      real :: a_bf                    !             |
      real :: p_bf                    !             |
      real :: flo_rate                !             |
      real :: vel                     !             |
      
      do i = 1, sp_ob%chandeg
        icmd = sp_ob1%chandeg + i - 1
        idat = ob(icmd)%props
        idb = sd_dat(idat)%hyd
        sd_ch(i)%name = sd_chd(idb)%name
        sd_ch(i)%order = sd_chd(idb)%order
        sd_ch(i)%chw = sd_chd(idb)%chw
        sd_ch(i)%chd = sd_chd(idb)%chd
        sd_ch(i)%chs = sd_chd(idb)%chs
        sd_ch(i)%chl = sd_chd(idb)%chl
        sd_ch(i)%chn = sd_chd(idb)%chn
        sd_ch(i)%chk = sd_chd(idb)%chk
        sd_ch(i)%cov = sd_chd(idb)%cov
        sd_ch(i)%wd_rto = sd_chd(idb)%wd_rto
        sd_ch(i)%chseq = sd_chd(idb)%chseq
        sd_ch(i)%d50 = sd_chd(idb)%d50
        sd_ch(i)%ch_clay = sd_chd(idb)%ch_clay
        sd_ch(i)%carbon = sd_chd(idb)%carbon
        sd_ch(i)%ch_bd = sd_chd(idb)%ch_bd
        sd_ch(i)%chss = sd_chd(idb)%chss
        sd_ch(i)%bedldcoef = sd_chd(idb)%bedldcoef
        sd_ch(i)%fps = sd_chd(idb)%fps
        sd_ch(i)%fpn = sd_chd(idb)%fpn
        sd_ch(i)%hc_kh = sd_chd(idb)%hc_kh
        sd_ch(i)%hc_hgt = sd_chd(idb)%hc_hgt
        sd_ch(i)%hc_ini = sd_chd(idb)%hc_ini      
        sd_ch(i)%cherod = sd_chd(idb)%cherod
          
        !! compute headcut parameters
        kh = sd_ch(i)%hc_kh
        if (kh > 1.e-6) then
          sd_ch(i)%hc_co = .37 * (17.83 + 16.56 * kh - 15. * sd_ch(i)%cov)
          sd_ch(i)%hc_co = amax1 (0., sd_ch(i)%hc_co)
        else
          sd_ch(i)%hc_co = 0.
        end if

        !! compute travel time coefficients
        aa = 1.
        b = 0.
        d = 0.
        chside = sd_ch(i)%chss
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

        !! compute rating curve
        call sd_rating_curve (i)
        
      end do    !end of channel loop
 
      ! initialize organics-minerals in channel water and benthic from input data
      do ich = 1, sp_ob%chandeg
        ! only initialize storage for real channels (length > 1 m)
        if (sd_ch(ich)%chl > 1.e-3) then
          iob = sp_ob1%chandeg + ich - 1
          ichdat = ob(iob)%props
          ich_ini = sd_dat(ichdat)%init
          iom_ini = sd_init(ich_ini)%org_min
          ch_stor(ich) = om_init_water(iom_ini)
          
          !! initial volume is frac of flow depth - frac*m*m*km*1000. = m3
          ch_stor(ich)%flo = om_init_water(iom_ini)%flo * sd_ch(ich)%chd * sd_ch(ich)%chw * sd_ch(ich)%chl * 1000.
          
          !! convert concentration to mass
          call hyd_convert_conc_to_mass (ch_stor(ich))
          
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
          sd_ch(ich)%aq_mix(ipest) = pestdb(ipest_db)%mol_wt ** (-.6666) * (1. - sd_ch(ich)%ch_bd / 2.65) * (69.35 / 365)
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
