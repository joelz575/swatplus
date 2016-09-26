      module time_module

      integer :: int_print
      integer, dimension (13) :: ndays =                                 &                                
                     (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
      integer, dimension (13) :: ndays_leap =                            &
                     (/0,31,60,91,121,152,182,213,244,274,305,335,366/)
      integer, dimension (13) :: ndays_noleap =                          &
                     (/0,31,59,90,120,151,181,212,243,273,304,334,365/)
      integer, dimension (12) :: ndmo =                          &
                     (/0,0,0,0,0,0,0,0,0,0,0,0/)
      !!    ndmo(:)     |days          |cumulative number of days accrued in the
      !!                               |month since the simulation began where the
      !!                               |array location number is the number of the
      !!                               |month
      type time_current
        integer :: day = 0            !! current day of simulation
        integer :: mo = 0             !! current month of simulation
        integer :: yrc = 2014         !! current calendar year
        integer :: yrc_start = 2014   !! starting calendar year
        integer :: yrs = 0            !! current sequential year
        integer :: day_mo = 0         !! day of month (1-31)  
        integer :: end_mo = 0         !! set to 1 if end of month
        integer :: end_yr = 0         !! set to 1 if end of year
        integer :: end_sim = 0        !! set to 1 if end of simulation
        integer :: idaf = 0           !! beginning julian day of simulation
        integer :: idal = 0           !! ending julian day of simulation for each year
        integer :: idal_in = 0        !! input ending julian day of simulation
        integer :: nbyr = 3           !! number of years of simulation run
        integer :: step = 0           !! number of time steps in a day for rainfall, runoff and routing
                                      !! 0 = daily; 1=increment(12 hrs); 24=hourly; 96=15 mins; 1440=minute;
        real :: dtm = 0.              !! time step in minutes for rainfall, runoff and routing
        real :: yrs_prt = 0.          !! number of years for average annual printing
      end type time_current
      type (time_current) :: time

      contains

      !! routines for time_module
      include 'icl.f90'
      include 'jdt.f90'
      include 'xmon.f90'

      end module time_module