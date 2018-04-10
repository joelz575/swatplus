      module exco_module
          
      type export_coefficient_datafiles       
        character(len=16) :: name
        character(len=16) :: om_file
        character(len=16) :: pest_file
        character(len=16) :: path_file
        character(len=16) :: hmet_file
        character(len=16) :: salts_file 
      end type export_coefficient_datafiles
      type (export_coefficient_datafiles), dimension(:),allocatable, save :: exco_db
      
      end module exco_module 