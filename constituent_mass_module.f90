      module constituent_mass_module
      
      type constituents
        integer :: num_pests                                        !number of pesticides simulated
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides- points to pesticide database
        !!need to crosswalk pests to get pest_num for database - use sequential for object
        integer, dimension(:), allocatable :: pest_num              !number of the pesticides- points to pesticide database
        integer :: num_paths                                        !number of pathogens simulated
        character (len=16), dimension(:), allocatable :: paths      !name of the pathogens- points to pathogens database
        integer, dimension(:), allocatable :: path_num              !number of the pathogens- points to pathogens database
        integer :: num_metals                                       !number of heavy metals simulated
        character (len=16), dimension(:), allocatable :: metals     !name of the heavy metals- points to heavy metals database
        integer, dimension(:), allocatable :: metals_num            !number of the heavy metals- points to heavy metals database
        integer :: num_salts                                        !number of salts simulated
        character (len=16), dimension(:), allocatable :: salts      !name of the salts - points to salts database
        integer, dimension(:), allocatable :: salts_num             !number of the alts - points to salts database
      end type constituents
      type (constituents) :: cs_db

      type pesticide_initialization_data
        character(len=13) :: name
        integer :: num_db       !!          |pesticide number in pesticide.pst
        real :: plt_sol         !! kg/ha    |amount of pesticide on plant at start of simulation
        real :: soil_sol        !! kg/ha    |amount of pesticide in soil at start of simulation
        real :: plt_sor         !! kg/ha    |amount of pesticide on plant at start of simulation
        real :: soil_sor        !! kg/ha    |amount of pesticide in soil at start of simulation
        real :: enr             !!          | pesticide enrichment ratio
      end type pesticide_initialization_data
      type (pesticide_initialization_data), dimension (:,:), allocatable :: pest_init
      
      type pesticide_community_data
        character(len=13) :: name        !!      |name of pesticide community
        character (len=16) :: init_df    !!      |name of initialization file for pesticide community
        character (len=16) :: recall_df  !!      |name of recall file for pesticide community
        character (len=16) :: exco_df    !!      |name of export coefficient file for pesticide community
        character (len=16) :: dr_df      !!      |name of delivery ratio file for pesticide community
        integer :: num                   !!      |number of pesticides in community
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides in community
        integer, dimension(:), allocatable :: num_db                !number in pesticide.pst (xwalked with pests)
      end type pesticide_community_data
      type (pesticide_community_data), dimension (:), allocatable :: pestcom_db
      
      type pathogen_community_data
        character(len=13) :: name        !!      |name of pathogen community
        character (len=16) :: init_df    !!      |name of initialization file for pathogen community
        character (len=16) :: recall_df  !!      |name of recall file for pathogen community
        character (len=16) :: exco_df    !!      |name of export coefficient file for pathogen community
        character (len=16) :: dr_df      !!      |name of delivery ratio file for pathogen community
        integer :: num                   !!      |number of pathogens in community
        character (len=16), dimension(:), allocatable :: paths      !name of the pathogens in the community
      end type pathogen_community_data
      type (pathogen_community_data), dimension (:), allocatable :: pathcom_db
      
      type heavymetal_community_data
        character(len=13) :: name        !!      |name of heavy metal community
        character (len=16) :: init_df    !!      |name of initialization file for heavy metal community
        character (len=16) :: recall_df  !!      |name of recall file for heavy metal community
        character (len=16) :: exco_df    !!      |name of export coefficient file for heavy metal community
        character (len=16) :: dr_df      !!      |name of delivery ratio file for heavy metal community
        integer :: num                   !!      |number of heavy metals in community
        character (len=16), dimension(:), allocatable :: metals     !name of the metals in the community
      end type heavymetal_community_data
      type (heavymetal_community_data), dimension (:), allocatable :: hmetcom_db
      
      type salt_community_data
        character(len=13) :: name        !!      |name of salt community
        character (len=16) :: init_df    !!      |name of initialization file for salt community
        character (len=16) :: recall_df  !!      |name of recall file for salt community
        character (len=16) :: exco_df    !!      |name of export coefficient file for salt community
        character (len=16) :: dr_df      !!      |name of delivery ratio file for salt community
        integer :: num                   !!      |number of salts in community
        character (len=16), dimension(:), allocatable :: salts      !name of the salts in the community
      end type salt_community_data
      type (salt_community_data), dimension (:), allocatable :: saltcom_db

      type spatial_object_pesticide
        real :: pest_sol      !kg/ha or kg       |soluble pesticide mass
        real :: pest_sor      !kg/ha or kg       |sorbed pesticide mass
      end type spatial_object_pesticide
      
      type spatial_object_pathogen
        real :: path_sol      !kg/ha or kg       |soluble pesticide mass
        real :: path_sor      !kg/ha or kg       |sorbed pesticide mass
      end type spatial_object_pathogen
      
      type spatial_object_heavymetal
        real :: hmet_sol      !kg/ha or kg       |soluble pesticide mass
        real :: hmet_sor      !kg/ha or kg       |sorbed pesticide mass
      end type spatial_object_heavymetal
      
      type spatial_object_salt
        real :: salt_sol      !kg/ha or kg       |soluble pesticide mass
        real :: salt_sor      !kg/ha or kg       |sorbed pesticide mass
      end type spatial_object_salt
      
      !recall pesticide inputs
      type recall_pesticide_inputs
         character (len=16) :: name
         integer :: num = 0                    !number of elements
         integer :: typ                        !recall type - 1=day, 2=mon, 3=year
         character(len=13) :: filename         !filename
         !hyd_output units are in cms and mg/L
         type (spatial_object_pesticide), dimension (:,:), allocatable :: hd_pest     !export coefficients
      end type recall_pesticide_inputs
      type (recall_pesticide_inputs),dimension(:),allocatable:: rec_pest

      end module constituent_mass_module