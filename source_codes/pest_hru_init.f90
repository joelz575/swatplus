      subroutine pest_hru_init
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%pest_soil, exist=i_exist)
      if (i_exist /= 0 .or. in_init%pest_soil /= "null") then
        do
          open (107,file=in_init%pest_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum     !name
            if (eof < 0) exit
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%pest_ini = imax
          
          allocate (pesti_db(imax))
          allocate (cs_pest_solsor(cs_db%num_pests))
          
          do ipest = 1, imax
            allocate (pesti_db(ipest)%pesti(cs_db%num_pests))
          end do
          
          rewind (107)
          read (107,*) titldum
          
          do ipesti = 1, imax
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) pesti_db(ipesti)%name
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) pesti_db(ipesti)%pesti(ipest)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine pest_hru_init