      subroutine hmet_hru_init
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: ihmet
      integer :: ihmeti
      integer :: eof, imax

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%hmet_soil, exist=i_exist)
      if (i_exist /= 0 .or. in_init%hmet_soil /= "null") then
        do
          open (107,file=in_init%hmet_soil)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) header
            if (eof < 0) exit
            read (107,*,iostat=eof) titldum     !name
            do ihmet = 1, cs_db%num_metals
              read (107,*,iostat=eof) titldum
              if (eof < 0) exit
            end do
            imax = imax + 1
          end do
          
          db_mx%hmet_ini = imax
          
          allocate (hmeti_db(imax))
          allocate (cs_hmet_solsor(cs_db%num_metals))
          
          do ihmet = 1, imax
            allocate (hmeti_db(ihmet)%hmeti(cs_db%num_metals))
          end do
           
          rewind (107)
          read (107,*) titldum

          do ihmeti = 1, imax
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) hmeti_db(ihmeti)%name
            do ipest = 1, cs_db%num_metals
              read (107,*,iostat=eof) hmeti_db(ihmeti)%hmeti(ihmet)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine hmet_hru_init