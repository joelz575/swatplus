      subroutine basin_print_codes_read
      
      use input_file_module
       
      character (len=500) :: header
      character (len=80) :: titldum
      integer :: eof
       
      eof = 0

      !! read weather codes
      inquire (file=in_sim%prt, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_sim%prt)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        read (107,*,iostat=eof) pco
        if (eof < 0) exit
        exit
      enddo
      endif
      close (107)
      
      if (pco%jd_start == 0) pco%jd_start = 1
      if (pco%jd_end == 0) pco%jd_end = 366
      if (pco%yr_start == 0) pco%yr_start = time%yrc
      if (pco%yr_end == 0) pco%yr_end = time%yrc + time%nbyr
      int_print = pco%interval - 1
            
      return
      end subroutine basin_print_codes_read           