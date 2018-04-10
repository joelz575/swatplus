      subroutine dr_db_read
    
      use dr_module   
      !use hydrograph_module
      use input_file_module
      !use organic_mineral_mass_module
      !use maximum_data_module
 
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      integer :: eof, imax, ob1, ob2

      eof = 0
      
      !read all delivery ratio data
      inquire (file=in_delr%del_ratio, exist=i_exist)
      if (i_exist /= 0 .or. in_delr%del_ratio /= 'null') then
        do
          open (107,file=in_delr%del_ratio)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = 0
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (dr_db(0:imax))
          rewind (107)
          read (107,*) titldum
          
        do ii = 1, imax
          read (107,*,iostat=eof) dr_db(ii)
          if (eof < 0) exit
        end do
          close (107)
          exit
        end do
      end if
      
      return
      end subroutine dr_db_read