      subroutine dr_read

      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
      
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      integer :: eof, imax

      eof = 0
      imax = 0
      
      !read all delivery ratio data here - don't need a module
      inquire (file=in_delr%del_ratio, exist=i_exist)
      if (i_exist /= 0 .or. in_delr%del_ratio /= 'null') then
      do
        open (107,file=in_delr%del_ratio)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) mdr_sp
        allocate (dr(mdr_sp))
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        do ii = 1, mdr_sp
          read (107,*,iostat=eof) dr_om(i,ii)   ! read dr for every pesticide community
          if (eof < 0) exit
        end do
        exit
      end do
      end if
      return
      end subroutine dr_read