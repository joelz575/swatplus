      subroutine pest_water_init
    
      use constituent_mass_module
      use input_file_module
      use maximum_data_module
      use channel_data_module
      use hydrograph_module
      use sd_channel_module
      use organic_mineral_mass_module
 
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, imax

      eof = 0
      
      !read all export coefficient data
      inquire (file=in_init%pest_water, exist=i_exist)
      if (i_exist /= 0 .or. in_init%pest_water /= "null") then
        do
          open (107,file=in_init%pest_water)
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
          
          db_mx%pestw_ini = imax
          
          allocate (pest_water_ini(imax))
          allocate (pest_benthic_ini(imax))
          allocate (pest_init_name(imax))

          do ipesti = 1, imax
            allocate (pest_water_ini(ipesti)%pest(cs_db%num_pests))
            allocate (pest_benthic_ini(ipesti)%pest(cs_db%num_pests))
          end do
          
          rewind (107)
          read (107,*) titldum
          
          do ipesti = 1, imax
            read (107,*,iostat=eof) header
            read (107,*,iostat=eof) pest_init_name(ipesti)
            do ipest = 1, cs_db%num_pests
              read (107,*,iostat=eof) titldum, pest_water_ini(ipesti)%pest(ipest), pest_benthic_ini(ipesti)%pest(ipest)
              if (eof < 0) exit
            end do
          end do
          close (107)
          exit
        end do
      end if

      return
      end subroutine pest_water_init