      module sd_channel_module

      use hydrograph_module
      use time_module
      use climate_module
    
      integer :: maxint                           !number of intervals in hydrograph for degredation
      real :: peakrate, sed_reduc_t, no3_reduc_kg, tp_reduc_kg, tp_reduc, srp_reduc_kg
      real, dimension(:), allocatable :: hyd_rad  !m^2        |hydraulic radius for each hydrograph time step
      real, dimension(:), allocatable :: timeint  !days       |time spent in each hydrograph time step
      
      type swatdeg_channel_data
        character(len=13) :: name
        integer :: route_db = 0 !         |pointer to routing_nut_data
        real :: chw             !m        |channel width
        real :: chd             !m        |channel depth
        real :: chs             !m/m      |channel slope
        real :: chl             !km       |channel length
        real :: chn             !         |channel Manning's n
        real :: chk             !mm/h     |channel bottom conductivity
        real :: cherod          !         |channel erodibility
        real :: cov             !0-1      |channel cover factor
        real :: chwdr           !         |channel width depth ratio
        real :: chseq           !m/m      |equilibrium channel slope
        real :: d50             !mm       |channel median sediment size
        real :: clay            !%        |clay percent of bank and bed
        real :: bd              !t/m3     |dry bulk density
        real :: chss            !         |channel side slope
        real :: bedldcoef       !         |percent of sediment entering the channel that is bed material
        real :: tc              !         |time of concentration
        real :: hc_kh           !         |headcut erodibility
        real :: hc_hgt          !m        |headcut height
        real :: hc_ini          !km       |initial channel length for gullies
      end type swatdeg_channel_data
      type (swatdeg_channel_data), dimension (:), allocatable :: sd_chd
      
      type swatdeg_channel_dynamic
        character(len=13) :: name = "default"
        real :: chw = 3.     !m        |channel width
        real :: chd = .5     !m        |channel depth
        real :: chs = .01    !m/m      |channel slope
        real :: chl = .1     !km       |channel length
        real :: hc_co = 0.   !m/m      |proportionality coefficient for head cut
        real :: attack0 = 0. !km       |attack threshold for movement of head cut
        real :: hc_len = 0.  !km       |length of head cut
        character (len=2) :: overbank  !         |0=inbank; 1=overbank flood
        real, dimension(13) :: phi
      end type swatdeg_channel_dynamic
      type (swatdeg_channel_dynamic),dimension (:), allocatable :: sd_ch
              
      type sd_ch_output
        real :: flo = 0.              ! (m^3/s)      !ave flow rate
        real :: peakr = 0.            ! (m^3/s)      |peak runoff rate
        real :: sed_in = 0.           ! (tons)       !total sed in
        real :: sed_out = 0.          ! (tons)       !total sed out
        real :: washld = 0.           ! (tons)       !wash load
        real :: bedld = 0.            ! (tons)       !bed load
        real :: dep = 0.              ! (tons)       !deposition
        real :: deg_btm = 0.          ! (tons)       !bottom erosion
        real :: deg_bank = 0.         ! (tons)       !bank erosion
        real :: hc_sed = 0.           ! (tons)       !headcut erosion
      end type sd_ch_output
      
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_a
      type (sd_ch_output) :: chsdz
            
      type sdch_header
          character (len=6) :: yrs =          ' time '
          character (len=6) :: yrc =          ' year '
          character (len=8) :: isd =        '   unit '
                                            
          character(len=15) :: flo =       '    floin_m^3/s'        ! (m^3/s)
          character(len=15) :: peakr =     '       pr_m^3/s'        ! (m^3/s)
          character(len=15) :: sed_in =    '     sedin_tons'        ! (tons)
          character(len=15) :: sed_out=    '    sedout_tons'        ! (tons)
          character(len=15) :: washld =    '    washld_tons'        ! (tons)
          character(len=15) :: bedld =     '     bedld_tons'        ! (tons)
          character(len=15) :: dep =       '       dep_tons'        ! (tons)
          character(len=15) :: deg_btm =   '   deg_btm_tons'        ! (tons)
          character(len=15) :: deg_bank =  '  deg_bank_tons'        ! (tons)
          character(len=15) :: hc_sed =    '     hcsed_tons'        ! (tons)
          character(len=15) :: width =     '        width_m'        ! (m)
          character(len=15) :: depth =     '        depth_m'        ! (m)
          character(len=15) :: slope =     '      slope_m/m'        ! (m/m)
          character(len=15) :: hc_len =    '        hclen_m'        ! (m)
      end type sdch_header
      type (sdch_header) :: sdch_hdr
     
      interface operator (+)
        module procedure chsd_add
      end interface
      
      interface operator (/)
        module procedure chsd_div
      end interface
        
      interface operator (*)
        module procedure chsd_mult
      end interface 
             
      contains
!! routines for swatdeg_hru module
      include 'sd_channel_read.f90'
      include 'sd_channel_control.f90'
      include 'sd_channel_output.f90'
      include 'sd_channel_nutrients.f90'

      function chsd_add(cho1,cho2) result (cho3)
      type (sd_ch_output),  intent (in) :: cho1
      type (sd_ch_output),  intent (in) :: cho2
      type (sd_ch_output) :: cho3
       cho3%flo = cho1%flo + cho2%flo
       cho3%peakr = cho1%peakr + cho2%peakr
       cho3%sed_in = cho1%sed_in + cho2%sed_in
       cho3%sed_out = cho1%sed_out + cho2%sed_out
       cho3%washld = cho1%washld + cho2%washld
       cho3%bedld = cho1%bedld + cho2%bedld
       cho3%dep = cho1%dep + cho2%dep
       cho3%deg_btm = cho1%deg_btm + cho2%deg_btm
       cho3%deg_bank = cho1%deg_bank + cho2%deg_bank
       cho3%hc_sed = cho1%hc_sed + cho2%hc_sed
      end function
      
      function chsd_div (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo = ch1%flo / const
        ch2%peakr = ch1%peakr / const
        ch2%sed_in = ch1%sed_in / const
        ch2%sed_out = ch1%sed_out / const
        ch2%washld = ch1%washld / const
        ch2%bedld = ch1%bedld / const
        ch2%dep = ch1%dep / const
        ch2%deg_btm = ch1%deg_btm / const
        ch2%deg_bank = ch1%deg_bank / const
        ch2%hc_sed = ch1%hc_sed / const
      end function chsd_div
      
      function chsd_mult (const, chn1) result (chn2)
        type (sd_ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (sd_ch_output) :: chn2
        chn2%flo = const * chn1%flo
        chn2%peakr = const * chn1%peakr
        chn2%sed_in = const * chn1%sed_in
        chn2%sed_out = const * chn1%sed_out
        chn2%washld = const * chn1%washld
        chn2%bedld = const * chn1%bedld
        chn2%dep = const * chn1%dep
        chn2%deg_btm = const * chn1%deg_btm
        chn2%deg_bank = const * chn1%deg_bank
        chn2%hc_sed = const * chn1%hc_sed     
      end function chsd_mult
      
      end module sd_channel_module