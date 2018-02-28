      module climate_module
      
      integer :: ifirsts, ifirsth, ifirstw, ifirstpet
 !     real, dimension (:), allocatable :: rmeas
 !    real, dimension (:), allocatable :: txmeas
 !     real, dimension (:), allocatable :: tnmeas
 !     real, dimension (:), allocatable :: slrmeas
 !     real, dimension (:), allocatable :: rhmeas
 !     real, dimension (:), allocatable :: wndmeas
      real, dimension (:,:), allocatable :: frad
      real, dimension (:,:), allocatable :: wgncur,wgnold
      integer, dimension (:), allocatable :: elevp,elevt
      integer, dimension (:), allocatable :: idg
      integer, dimension (:,:), allocatable :: rndseed
      real, dimension (:), allocatable :: rnd2,rnd3,rnd8,rnd9
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      !! this should go in the weather module

      type weather_generator_db      
        real :: lat =  0.0                        !! degrees       |latitude of weather station used to compile data
        real :: long = 0.0                        !! degrees       |longitude of weather station 
        real :: elev = 0.0                        !!               |elevation of weather station used to compile weather generator data
        real :: rain_yrs = 10.0                   !! none          |number of years of recorded maximum 0.5h rainfall used to calculate values for rainhhmx(:)
        real, dimension (12) :: tmpmx = (/-0.032,2.9,9.95,17.78,23.73,    &
             28.74,30.28,28.91,25.85,19.39,10.81,2.96/)      !! deg C  |avg monthly maximum air temperature
        real, dimension (12) :: tmpmn = (/-9.94,-7.31,-1.23,4.73,10.36,   &
             15.47,17.67,16.23,12.34,6.23,0.39,-6.06/)       !! deg C  |avg monthly minimum air temperature
        real, dimension (12) :: tmpstdmx = (/6.76,6.63,6.88,6.12,5.33,    &
             3.74,3.21,3.27,4.36,5.76,6.39,6.53/)            !! deg C  |standard deviation for avg monthly maximum air temperature 
        real, dimension (12) :: tmpstdmn = (/7.74,7.39,5.84,5.34,5.01,    &
             3.96,3.49,3.79,5.02,5.52,5.69,7.21/)            !! deg C  |standard deviation for avg monthly minimum air temperature
        real, dimension (12) :: pcpmm = (/49.6,38.8,74.0,94.3,72.9,90.7,  &
             104.6,91.1,79.6,57.9,57.2,71.5/)                !! mm     |amount of precipitation in month
        real, dimension (12) :: pcpstd = (/8.9,7.6,9.4,13.7,8.6,12.7,     &
             17.8,17.5,12.7,13.5,8.9,11.9/)                  !! mm/day |standard deviation for the average daily
        real, dimension (12) :: pcpskw = (/0.85,1.87,0.61,1.0,-1.26,      &
             0.01,0.71,1.68,0.09,2.28,0.88,2.51/)            !! none   |skew coefficient for the average daily precipitation
        real, dimension (12) :: pr_wd = (/0.17,0.18,0.21,0.22,0.18,0.20,  &
             0.18,0.19,0.18,0.14,0.19,0.22/)                 !! none   |probability of wet day after dry day in month 
        real, dimension (12) :: pr_ww = (/0.36,0.36,0.45,0.42,0.48,0.41,  &
             0.34,0.32,0.37,0.36,0.38,0.42/)                 !! none   |probability of wet day after wet day in month
        real, dimension (12) :: pcpd = (/6.51,6.37,8.57,8.25,7.97,7.59,   &
             6.64,6.77,6.67,5.56,7.04,8.52/)                 !! days   |average number of days of precipitation in the month
        real, dimension (12) :: rainhmx = (/11.4,11.2,13.2,29.2,19.3,     &
             51.3,45.0,41.7,27.2,12.7,8.4,8.1/)              !! mm     |maximum 0.5 hour rainfall in month
        real, dimension (12) :: solarav = (/5.61,8.34,12.15,16.05,19.94,  &
             21.79,22.0,19.57,16.13,11.27,7.12,4.99/)        !! MJ/m^2/day    |average daily solar radiation for the month
        real, dimension (12) :: dewpt = (/-5.89,-5.05,-1.29,4.51,9.05,    &
             15.45,17.81,17.38,12.52,6.71,0.04,-4.49/)       !! deg C  |average dew point temperature for the month
        real, dimension (12) :: windav = (/5.39,5.43,5.78,5.79,5.02,      &
             4.33,3.71,3.51,3.97,4.37,5.18,5.21/)            !! m/s    |average wind speed for the month
      end type weather_generator_db
      type (weather_generator_db), dimension(:),allocatable :: wgn
      type (weather_generator_db), dimension(:),allocatable :: wgn_orig
      type (weather_generator_db), dimension(:),allocatable :: fwgn
      
      type wgn_parms
        real, dimension (12) :: pr_wdays = 0.   !! none          |proportion of wet days in a month
        real, dimension (12) :: pcpmean = 0.    !! mm/day        |average amount of precipitation falling in one day for the month
        real :: daylmn = 0.                     !!               |minimum day length
        real :: daylth = 0.                     !!               |day length threshhold to trigger dormancy
        real :: latsin = 0.                     !!               |sine of latitude
        real :: latcos = 0.                     !!               |cosine of latitude
        real :: phutot = 0.                     !!               |total base zero heat units for year
        real :: pcpdays = 0.                    !!               |days of precip in year
        real :: tmp_an = 0.                     !!               |average annual air temperature
        real :: pcp_an = 0.                     !!               |average annual precipitation
        real, dimension (12) :: pcf = 0.        !!               |normalization factor for precipitation
        real, dimension (12) :: amp_r = 0.      !!               |alpha factor for rain(mo max 0.5h rain)
        integer :: ireg = 0                     !!               |annual precip category-1 <= 508 mm; 2 > 508 and <= 1016 mm; 3 > 1016 mm/yr
      end type wgn_parms
      type (wgn_parms), dimension(:),allocatable :: wgn_pms
          
      type wind_direction_db
        character(len=16) :: name = 'default-uniform'
        real, dimension (12,16) :: dir = 1.     !! 1-16         |avg monthly wind direstion
      end type wind_direction_db
      type (wind_direction_db), dimension(:),allocatable :: wnd_dir
      
      type weather_daily
        real :: precip
        real :: tmax
        real :: tmin
        real :: tave
        real :: solrad
        real :: solradmx
        real :: rhum
        real :: dewpt
        real :: windsp
        real :: pet
        real :: wndir
        real :: phubase0                                    !! deg C        |base 0 heat units from Jan 1
        real :: daylength                                   !! hr           |day length
        real :: precip_half_hr                              !! frac         |fraction of total rainfall on day that occurs
                                                            !!              |during 0.5h highest intensity rainfall
        character(len=3) :: precip_prior_day = 'dry'        !!              |'dry' or 'wet'
        real, dimension(:), allocatable :: ts               !! mm           |subdaily precip
      end type weather_daily
            
      type weather_codes_station
        integer :: wgn = 1        !!  weather generator station number
        integer :: pgage = 0      !!  gage number for rainfall (sim if generating)
        integer :: tgage = 0      !!  gage number fo temperature (sim if generating)
        integer :: sgage = 0      !!  gage number for solar radiation (sim if generating) 
        integer :: hgage = 0      !!  gage number for relative humidity (sim if generating)
        integer :: wgage = 0      !!  gage number for windspeed (sim if generating)
        integer :: wndir = 0      !!  number of wind direction gage (.dir) files used in sim
        integer :: atmodep = 0    !!  atmospheric depostion data file locator
      end type weather_codes_station
      
      type weather_codes_station_char
        !character (len=50) ::  wst = ""      !!  weather station name
        character (len=50) ::  wgn = ""       !!  weather generator name
        character (len=50) :: pgage = ""      !!  gage name for rainfall 
        character (len=50) :: tgage = ""      !!  gage name for temperature
        character (len=50) :: sgage = ""      !!  gage name for solar radiation
        character (len=50) :: hgage = ""      !!  gage name for relative humidity
        character (len=50) :: wgage = ""      !!  gage name for windspeed
        character (len=50) :: wndir = ""      !!  name of wind direction gage (.dir) files used in sim
        character (len=50) :: atmodep = ""    !!  atmospheric depostion data file locator
      end type weather_codes_station_char

      type weather_station
        character(len=50) :: name = "Farmer Branch IL"
        real :: lat                          ! degrees    |latitude
        type (weather_codes_station_char) :: wco_c
        type (weather_codes_station) :: wco 
        type (weather_daily) :: weat
        real, dimension(12) :: rfinc = 0     ! deg C      |monthly precipitation adjustment
        real, dimension(12) :: tmpinc = 0    ! deg C      |monthly temperature adjustment
        real, dimension(12) :: radinc = 0    ! MJ/m^2     |monthly solar radiation adjustment
        real, dimension(12) :: huminc = 0    ! none       |monthly humidity adjustment
      end type weather_station
      type (weather_station), dimension(:),allocatable :: wst
         
      type climate_measured_data
        character (len=50) :: filename
        real :: lat                   !! latitude of raingage         
        real :: long                  !! longitude of raingage
        real :: elev                  !! elevation of raingage
        integer :: nbyr               !! number of years of daily rainfall
        integer :: tstep              !! timestep of precipitation
        real, dimension (:,:), allocatable :: ts
        real, dimension (:,:), allocatable :: ts2
        real, dimension (:,:,:), allocatable :: tss
      end type climate_measured_data
      type (climate_measured_data), dimension(:), allocatable :: pcp
      type (climate_measured_data), dimension(:), allocatable :: tmp    
      type (climate_measured_data), dimension(:), allocatable :: slr
      type (climate_measured_data), dimension(:), allocatable :: hmd
      type (climate_measured_data), dimension(:), allocatable :: wnd 
      
      type atmospheric_deposition
        real :: nh4_rf = 1.         !! ave annual ammonia in rainfall - mg/l
        real :: no3_rf = .2         !! ave annual nitrate in rainfall - mg/l
        real :: nh4_dry = 0.        !! ave annual ammonia dry deposition - kg/ha/yr
        real :: no3_dry = 0.        !! ave annual nitrate dry deposition - kg/ha/yr
        character(len=50) :: name
        real, dimension(:), allocatable :: nh4_rfmo
        real, dimension(:), allocatable :: no3_rfmo
        real, dimension(:), allocatable :: nh4_drymo
        real, dimension(:), allocatable :: no3_drymo
        real, dimension(:), allocatable :: nh4_rfyr
        real, dimension(:), allocatable :: no3_rfyr
        real, dimension(:), allocatable :: nh4_dryyr 
        real, dimension(:), allocatable :: no3_dryyr     
      end type atmospheric_deposition
      type (atmospheric_deposition),dimension(:), allocatable :: atmodep
 
      type atmospheric_deposition_control
        integer :: num_sta = 0
        character(len=2) :: timestep
        integer :: ts = 0
        integer :: mo_init = 0
        integer :: yr_init = 0
        integer :: num = 0
        integer :: first = 1
      end type atmospheric_deposition_control
      type (atmospheric_deposition_control), save :: atmodep_cont
      
      character(len=50), dimension(:), allocatable :: wst_n
      character(len=50), dimension(:), allocatable :: wgn_n
      character(len=50), dimension(:), allocatable :: pcp_n
      character(len=50), dimension(:), allocatable :: tmp_n 
      character(len=50), dimension(:), allocatable :: slr_n
      character(len=50), dimension(:), allocatable :: hmd_n
      character(len=50), dimension(:), allocatable :: wnd_n   
      character(len=50), dimension(:), allocatable :: atmo_n       
          
      end module climate_module