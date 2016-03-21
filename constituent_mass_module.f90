      module constituent_mass_module

      type spatial_object_constituents_data
        character(len=13) :: name = "default"
        integer :: units = 1                        !1=mass, 2=mass/area, 3=frac for dr, 4=cms and concentration
        character (len=16) :: pest_com = "null"     !name of pesticide community (pesticide.com)
        character (len=16) :: pest_dat = "null"     !name of pesticide data (filename for recall, and object in file for exco/dr)
        character (len=16) :: path_com = "null"     !name of pathogen community (pathogen.com)
        character (len=16) :: path_dat = "null"     !name of pathogen data (filename for recall, and object in file for exco/dr)
        character (len=16) :: hmet_com = "null"     !name of heavy metal community (heavy_metal_.com)
        character (len=16) :: hmet_dat = "null"     !name of heavy metal data (filename for recall, and object in file for exco/dr)
        character (len=16) :: salt_com = "null"     !name of salt ion community (salt_ion.com)
        character (len=16) :: salt_dat = "null"     !name of salt ion data (filename for recall, and object in file for exco/dr)
      end type spatial_object_constituents_data
      !each spatial object will point to constituent data - need to crosswalk to get obcs integer pointers
      type (spatial_object_constituents_data), dimension(:),allocatable :: cs_db
      
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
      
      type spatial_object_constituents
        character (len=16) :: name                                  !should match the object_connectivity object
        integer :: num_pests                                        !number of pesticides simulated by this object
        character (len=16), dimension(:), allocatable :: pests      !name of the pesticides- points to pesticide database
        !!need to crosswalk pests to get pest_num for database - use sequential for object
        integer, dimension(:), allocatable :: pest_num              !number of the pesticides- points to pesticide database
        integer :: num_paths                                        !number of pesticides simulated by this object
        character (len=16), dimension(:), allocatable :: paths      !name of the pesticides- points to pesticide database
        integer :: num_metals                                       !number of pesticides simulated by this object
        character (len=16), dimension(:), allocatable :: metals     !name of the pesticides- points to pesticide database
        integer :: num_salts                                        !number of pesticides simulated by this object
        character (len=16), dimension(:), allocatable :: salts      !name of the pesticides- points to pesticide database
      end type spatial_object_constituents
      !track spatial_object_hydrographs with ob - use same pointer
      type (spatial_object_constituents), dimension(:),allocatable :: obcs
      
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

      !export coefficient and delivery ratio pesticides
      type (spatial_object_pesticide), dimension(:,:), allocatable :: exco_pest
      
      !export coefficient and delivery ratio pesticides
      type (spatial_object_pesticide), dimension(:,:), allocatable :: dr_pest
      
      !point to subbasin element objects - same as sub_elem
      type (spatial_object_constituents), dimension(:), allocatable :: sub_e_cs

      !point to channel-surface objects - same as ch_sur
      type (spatial_object_constituents), dimension(:), allocatable :: ch_sur_cs
      
      !objects needed for operators
      type (spatial_object_constituents) :: cs1, cs2, cs3
   
      end module constituent_mass_module