      subroutine exco_read
    
      use hydrograph_module
      use input_file_module
      use organic_mineral_mass_module
 
      character (len=80) :: titldum, header
      character (len=16) :: namedum
      integer :: eof, imax

      eof = 0
      imax = 0
      
      !read all export coefficient data here - don't need a module
      inquire (file=in_exco%exco, exist=i_exist)
      if (i_exist /= 0) then
      do
        open (107,file=in_exco%exco)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
        imax = 0
          do while (eof >= 0)
            read (107,*,iostat=eof) i
            if (eof < 0) exit
            imax = amax1(imax,i)
            mexco_sp = mexco_sp + 1 
          end do
          
      allocate (exco(0:imax))
      rewind (107)
      read (107,*) titldum
      read (107,*) header
      
      do ii = 1, mexco_sp
        read (107,*,iostat=eof) i
        backspace (107)
        read (107,*,iostat=eof) k, exco_om(i,ii)   
        if (eof < 0) exit
      end do
      close (107)
      exit
      enddo
      endif
      return
      end subroutine exco_read