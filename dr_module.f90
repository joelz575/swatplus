      module dr_module
          
      type delivery_ratio_datafiles       
        character(len=16) :: name
        character(len=16) :: om_file
        character(len=16) :: pest_file
        character(len=16) :: path_file
        character(len=16) :: hmet_file
        character(len=16) :: salts_file 
      end type delivery_ratio_datafiles
      type (delivery_ratio_datafiles), dimension(:),allocatable, save :: dr_db
      
      end module dr_module 