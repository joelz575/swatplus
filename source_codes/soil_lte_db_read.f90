        subroutine soil_lte_db_read
      
        use input_file_module
        use maximum_data_module
        use hru_lte_module
        use soil_data_module
        
        implicit none
        
        character (len=80) :: titldum   !           |title of file
        character (len=80) :: header    !           |header of file
        character (len=16) :: name      !
        integer :: eof                  !           |end of file
        integer :: k                    !           |texture counter
    
        eof = 0
        
        allocate (soil_lte(12))
        
        do
          open (107,file="soils_lte.sol")
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*,iostat=eof) header
          if (eof < 0) exit
        
          do k = 1, 12          
            read (107,*,iostat=eof) soil_lte(k)
            if (eof < 0) exit
          end do
        end do
     
        close (107)
        
      return
      end subroutine soil_lte_db_read