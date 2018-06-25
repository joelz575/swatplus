      module fertilizer_data_module
     
      implicit none
          
      type fertilizer_db
        character(len=16) :: fertnm = " "
        real :: fminn = 1.            !! kg minN/kg frt     |fract of fert which is mineral nit (NO3+NH3)
        real :: fminp = 0.            !! kg minN/kg frt     |frac of fert which is mineral phos
        real :: forgn = 0.            !! kg orgN/kg frt     |frac of fert which is org n
        real :: forgp = 0.            !! kg orgP/kg frt     |frac of fert which is org p
        real :: fnh3n = 0.            !! kg NH3-N/kg N      |frac of mineral N content of fert which is NH3
        real :: bactpdb = 0.          !! # bact/kg frt      |conc of persistent bacteria in fert
        real :: bactlpdb = 0.         !! # bact/kg frt      |conc of less persistent bacteria in fert
        real :: bactkddb = 0.         !! none               |frac of bacteria in solution (remaining is sorbed to soil part)
      end type fertilizer_db
      type (fertilizer_db), dimension(:),allocatable, save :: fertdb
      
      end module fertilizer_data_module 