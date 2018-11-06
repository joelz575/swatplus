      subroutine salt_hru_init
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: isalt
      integer :: isalti
      integer :: eof, imax

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%salt_soil, exist=i_exist)
      if (i_exist /= 0 .or. in_init%salt_soil /= "null") then
        do
          open (107,file=in_init%salt_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum     !name
            do isalt = 1, cs_db%num_salts
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%salt_ini = imax
          
          allocate (salti_db(imax))
          do isalt = 1, imax
            allocate (salti_db(isalt)%salti(cs_db%num_salts))
          end do
           
          rewind (107)
          read (107,*) titldum

          do isalti = 1, imax
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) salti_db(isalti)%name
            do ipest = 1, cs_db%num_salts
              read (107,*,iostat=eof) salti_db(isalti)%salti(isalt)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine salt_hru_init