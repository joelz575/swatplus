      module constituent_mass_module

      implicit none
      
      type constituents
        integer :: num_tot = 0                                  !number of total constituents simulated
        integer :: num_pests = 0                                    !number of pesticides simulated
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides- points to pesticide database
        !!need to crosswalk pests to get pest_num for database - use sequential for object
        integer, dimension(:), allocatable :: pest_num              !number of the pesticides- points to pesticide database
        integer :: num_paths = 0                                    !number of pathogens simulated
        character (len=16), dimension(:), allocatable :: paths      !name of the pathogens- points to pathogens database
        integer, dimension(:), allocatable :: path_num              !number of the pathogens- points to pathogens database
        integer :: num_metals = 0                                   !number of heavy metals simulated
        character (len=16), dimension(:), allocatable :: metals     !name of the heavy metals- points to heavy metals database
        integer, dimension(:), allocatable :: metals_num            !number of the heavy metals- points to heavy metals database
        integer :: num_salts = 0                                    !number of salts simulated
        character (len=16), dimension(:), allocatable :: salts      !name of the salts - points to salts database
        integer, dimension(:), allocatable :: salts_num             !number of the alts - points to salts database
      end type constituents
      type (constituents) :: cs_db

      type constituent_mass
        real :: sol = 0.    !kg/ha or kg        |soluble constituent mass
        real :: sor = 0.    !kg/ha or kg        |sorbed constituent mass
      end type constituent_mass

      type exco_pesticide
        type (constituent_mass), dimension (:), allocatable :: pest         !pesticide hydrographs
      end type exco_pesticide
      type (exco_pesticide), dimension (:), allocatable :: exco_pest        !export coefficients
      
      type dr_pesticide
        type (constituent_mass), dimension (:), allocatable :: pest         !pesticide delivery
      end type dr_pesticide
      type (dr_pesticide), dimension (:), allocatable :: dr_pest            !delivery ratios
      
      type exco_pathogens
        type (constituent_mass), dimension (:), allocatable :: path         !pesticide hydrographs
      end type exco_pathogens
      type (exco_pathogens), dimension (:), allocatable :: exco_path        !export coefficients
      
      type dr_pathogens
        type (constituent_mass), dimension (:), allocatable :: path         !pathogen delivery
      end type dr_pathogens
      type (dr_pathogens), dimension (:), allocatable :: dr_path            !delivery ratios
      
      type exco_heavy_metals
        type (constituent_mass), dimension (:), allocatable :: hmet         !heavy metals hydrographs
      end type exco_heavy_metals
      type (exco_heavy_metals), dimension (:), allocatable :: exco_hmet     !export coefficients
      
      type dr_heavy_metals
        type (constituent_mass), dimension (:), allocatable :: hmet         !heavy metals delivery
      end type dr_heavy_metals
      type (dr_heavy_metals), dimension (:), allocatable :: dr_hmet         !delivery ratios
      
      type exco_salts
        type (constituent_mass), dimension (:), allocatable :: salt         !salts hydrographs
      end type exco_salts
      type (exco_salts), dimension (:), allocatable :: exco_salt            !export coefficients
      
      type dr_salts
        type (constituent_mass), dimension (:), allocatable :: salt         !salts delivery
      end type dr_salts
      type (dr_salts), dimension (:), allocatable :: dr_salt                !delivery ratios
      
      ! constituent hydrographs - dimension to spatial object hyd number - i.e. 1=tot, 2=surf, etc.
      type constituent_hydrograph
        type (constituent_mass), dimension (:), allocatable :: pest      !pesticide hydrographs
        type (constituent_mass), dimension (:), allocatable :: path      !pathogen hydrographs
        type (constituent_mass), dimension (:), allocatable :: hmet      !heavy metal hydrographs
        type (constituent_mass), dimension (:), allocatable :: salt      !salt hydrographs
      end type constituent_hydrograph
      ! hydrographs used in command for adding incoming hyds
      type (constituent_hydrograph) :: hcs1, hcs2
      ! set zero constituent hydrograph
      type (constituent_hydrograph) :: hin_csz
      
      ! hydrographs for all constituents - dimension to number of each constituent
      type all_constituent_hydrograph
        type (constituent_hydrograph), dimension (:), allocatable :: hd
        type (constituent_hydrograph) :: hin
        type (constituent_hydrograph) :: hin_s
        type (constituent_hydrograph), dimension(:), allocatable :: hcsin_d
        type (constituent_hydrograph), dimension(:), allocatable :: hcsin_m
        type (constituent_hydrograph), dimension(:), allocatable :: hcsin_y
        type (constituent_hydrograph), dimension(:), allocatable :: hcsin_a
        type (constituent_hydrograph), dimension(:), allocatable :: hcsout_m
        type (constituent_hydrograph), dimension(:), allocatable :: hcsout_y
        type (constituent_hydrograph), dimension(:), allocatable :: hcsout_a
      end type all_constituent_hydrograph
      type (all_constituent_hydrograph), dimension (:), allocatable :: obcs
      
      ! set zero all constituent hydrograph
      type (all_constituent_hydrograph) :: hcsz
      
      !recall pesticide inputs
      type recall_pesticide_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (constituent_mass), dimension (:,:), allocatable :: hd_pest
      end type recall_pesticide_inputs
      type (recall_pesticide_inputs),dimension(:),allocatable:: rec_pest
      
      type pestinit
        real :: plt           !! kg/ha    |amount of pesticide on plant at start of simulation
        real :: soil          !! kg/ha    |amount of pesticide in soil at start of simulation
      end type pestinit
      
      type pestinit_db
        character(len=16) :: name        !!      |name of initial pesticide
        type (pestinit), dimension (:), allocatable :: pesti
      end type pestinit_db
      type (pestinit_db), dimension (:), allocatable :: pesti_db

      type pathinit
        real :: plt           !! kg/ha    |amount of pathogen on plant at start of simulation
        real :: soil          !! kg/ha    |amount of pathogen in soil at start of simulation
      end type pathinit
      
      type pathinit_db
        character(len=16) :: name        !!      |name of initial pathogens
        type (pathinit), dimension (:), allocatable :: pathi
      end type pathinit_db
      type (pathinit_db), dimension (:), allocatable :: pathi_db

      type hmetinit
        real :: plt           !! kg/ha    |amount of heavy metals on plant at start of simulation
        real :: soil          !! kg/ha    |amount of heavy metals in soil at start of simulation
      end type hmetinit
      
      type hmetinit_db
        character(len=16) :: name        !!      |name of initial heavy metals
        type (hmetinit), dimension (:), allocatable :: hmeti
      end type hmetinit_db
      type (hmetinit_db), dimension (:), allocatable :: hmeti_db
      
      type saltinit
        real :: plt           !! kg/ha    |amount of salt on plant at start of simulation
        real :: soil          !! kg/ha    |amount of salt in soil at start of simulation
      end type saltinit
      
      type saltinit_db
        character(len=16) :: name        !!      |name of initial salt
        type (saltinit), dimension (:), allocatable :: salti
      end type saltinit_db
      type (saltinit_db), dimension (:), allocatable :: salti_db
      
      end module constituent_mass_module