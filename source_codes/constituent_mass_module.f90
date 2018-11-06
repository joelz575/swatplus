      module constituent_mass_module

      implicit none

      character(len=16), dimension(:), allocatable :: pest_init_name
      character(len=16), dimension(:), allocatable :: path_init_name
      character(len=16), dimension(:), allocatable :: hmet_init_name
      character(len=16), dimension(:), allocatable :: salt_init_name

      
      type constituents
        integer :: num_tot = 0                                      !number of total constituents simulated
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
      
      ! intial water and benthic pesticides for channels and reservoirs
      type pesticide_initial
        type (constituent_mass), dimension (:), allocatable :: pest         !pesticide mass
      end type pesticide_initial
      type (pesticide_initial), dimension (:), allocatable :: pest_water_ini
      type (pesticide_initial), dimension (:), allocatable :: pest_benthic_ini
            
      ! intial water and benthic pathogens for channels and reservoirs
      type pathogen_initial
        type (constituent_mass), dimension (:), allocatable :: path         !pathogen mass
      end type pathogen_initial
      type (pathogen_initial), dimension (:), allocatable :: path_water_ini
      type (pathogen_initial), dimension (:), allocatable :: path_benthic_ini
      
      ! storing water and benthic constituents in channels and reservoirs
      type (constituent_hydrograph), dimension (:), allocatable :: ch_water
      type (constituent_hydrograph), dimension (:), allocatable :: ch_benthic
      type (constituent_hydrograph), dimension (:), allocatable :: res_water
      type (constituent_hydrograph), dimension (:), allocatable :: res_benthic
      type (constituent_hydrograph), dimension (:), allocatable :: wet_water
      type (constituent_hydrograph), dimension (:), allocatable :: wet_benthic
      
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
      
      ! initialize hru constituents
      type constituent_hru_init
        character (len=16) :: name                      !name of the constituent - points to constituent database
        real :: plt           !! kg/ha or #cfu/m^2      |amount of constituent on plant at start of simulation
        real :: soil          !! kg/ha or #cfu/m^2      |amount of constituent in soil at start of simulation
      end type constituent_hru_init
      
      type pestinit_db
        character(len=16) :: name        !!      |name of initial pesticide
        type (constituent_hru_init), dimension (:), allocatable :: pesti
      end type pestinit_db
      type (pestinit_db), dimension (:), allocatable :: pesti_db

      type pathinit_db
        character(len=16) :: name        !!      |name of initial pathogen
        type (constituent_hru_init), dimension (:), allocatable :: pathi
      end type pathinit_db
      type (pathinit_db), dimension (:), allocatable :: pathi_db
 
      type hmetinit_db
        character(len=16) :: name        !!      |name of initial heavy metal
        type (constituent_hru_init), dimension (:), allocatable :: hmeti
      end type hmetinit_db
      type (hmetinit_db), dimension (:), allocatable :: hmeti_db

      type saltinit_db
        character(len=16) :: name        !!      |name of initial salt
        type (constituent_hru_init), dimension (:), allocatable :: salti
      end type saltinit_db
      type (saltinit_db), dimension (:), allocatable :: salti_db
      
     type constituents_header_in          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "     obtypin"
        character (len=12) :: obno_out = "  obtyp_noin"
        character (len=12) :: htyp_out = "     htyp_in"
        character (len=12) :: frac     = "     frac_in"
        character (len=12) :: sol      = "      sol_in"
        character (len=12) :: sor      = "      sor_in"
      end type constituents_header_in
      type (constituents_header_in) :: csin_hyd_hdr
          
      type constituents_header_out          
        character (len=11) :: day      = "       jday "
        character (len=12) :: mo       = "         mon"
        character (len=12) :: day_mo   = "         day"
        character (len=12) :: yrc      = "          yr"
        character (len=12) :: name     = "         iob"
        character (len=12) :: otype    = "     gis_id "
        character (len=12) :: type     = "        type"
        character (len=12) :: num      = "         num"
        character (len=12) :: obout    = "    obtypout"
        character (len=12) :: obno_out = " obtyp_noout"
        character (len=12) :: htyp_out = "    htyp_out"
        character (len=12) :: frac     = "    frac_out"
      end type constituents_header_out
      type (constituents_header_out) :: csout_hyd_hdr
      
      type sol_sor
        character (len=12) :: sol =      "     sol_out"
        character (len=12) :: sor =      "     sor_out"
      end type sol_sor
      type (sol_sor), dimension (:), allocatable :: cs_pest_solsor
      type (sol_sor), dimension (:), allocatable :: cs_path_solsor
      type (sol_sor), dimension (:), allocatable :: cs_hmet_solsor
      type (sol_sor), dimension (:), allocatable :: cs_salt_solsor
     
      interface operator (+)
        module procedure hydcsout_add
      end interface
      
      interface operator (*)
        module procedure hydcsout_mult_const
      end interface

      contains
      
      function hydcsout_add (hydcs1, hydcs2) result (hydcs3)
        type (constituent_hydrograph), intent (in) :: hydcs1
        type (constituent_hydrograph), intent (in) :: hydcs2
        type (constituent_hydrograph) :: hydcs3
        integer :: ipest, ipath, ihmet, isalt
        allocate (hydcs3%pest(cs_db%num_pests))
        allocate (hydcs3%path(cs_db%num_paths))
        allocate (hydcs3%hmet(cs_db%num_metals))
        allocate (hydcs3%salt(cs_db%num_salts))

        do ipest = 1, cs_db%num_pests
          hydcs3%pest(ipest)%sol =  hydcs2%pest(ipest)%sol + hydcs1%pest(ipest)%sol
          hydcs3%pest(ipest)%sor =  hydcs2%pest(ipest)%sor + hydcs1%pest(ipest)%sol
        end do
        do ipath = 1, cs_db%num_paths
          hydcs3%path(ipath)%sol =  hydcs2%path(ipath)%sol + hydcs1%path(ipath)%sol
          hydcs3%path(ipath)%sor =  hydcs2%path(ipath)%sor + hydcs1%path(ipath)%sol
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs3%hmet(ihmet)%sol =  hydcs2%hmet(ihmet)%sol + hydcs1%hmet(ihmet)%sol
          hydcs3%hmet(ihmet)%sor =  hydcs2%hmet(ihmet)%sor + hydcs1%hmet(ihmet)%sol
        end do
        do isalt = 1, cs_db%num_salts
          hydcs3%salt(isalt)%sol =  hydcs2%salt(isalt)%sol + hydcs1%salt(isalt)%sol
          hydcs3%salt(isalt)%sor =  hydcs2%salt(isalt)%sor + hydcs1%salt(isalt)%sol
        end do
      return
      end function hydcsout_add
      
      function hydcsout_mult_const (const, hydcs1) result (hydcs2)
        type (constituent_hydrograph), intent (in) :: hydcs1
        type (constituent_hydrograph) :: hydcs2
        real, intent (in) :: const
        integer :: ipest, ipath, ihmet, isalt
        allocate (hydcs2%pest(cs_db%num_pests))
        allocate (hydcs2%path(cs_db%num_paths))
        allocate (hydcs2%hmet(cs_db%num_metals))
        allocate (hydcs2%salt(cs_db%num_salts))

        do ipest = 1, cs_db%num_pests
          hydcs2%pest(ipest)%sol =  const * hydcs1%pest(ipest)%sol
          hydcs2%pest(ipest)%sor =  const * hydcs1%pest(ipest)%sor
        end do
        do ipath = 1, cs_db%num_paths
          hydcs2%path(ipath)%sol =  const * hydcs1%path(ipath)%sol
          hydcs2%path(ipath)%sor =  const * hydcs1%path(ipath)%sor
        end do
        do ihmet = 1, cs_db%num_metals
          hydcs2%hmet(ihmet)%sol =  const * hydcs1%hmet(ihmet)%sol
          hydcs2%hmet(ihmet)%sor =  const * hydcs1%hmet(ihmet)%sor
        end do
        do isalt = 1, cs_db%num_salts
          hydcs2%salt(isalt)%sol =  const * hydcs1%salt(isalt)%sol
          hydcs2%salt(isalt)%sor =  const * hydcs1%salt(isalt)%sor
        end do
        return
      end function hydcsout_mult_const
      
      end module constituent_mass_module