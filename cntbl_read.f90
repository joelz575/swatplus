      subroutine cntbl_read
      
      use input_file_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      mcn = 0
      
    !! read all curve number data from cn.tbl
      inquire (file=in_lum%cntable_lum, exist=i_exist)
      if (i_exist == 0 .or. in_lum%cntable_lum == 'null') then
        allocate (cn(0:0))
      else
      do
        open (107,file=in_lum%cntable_lum)
        read (107,*,iostat=eof) titldum
        if (eof < 0) exit
        read (107,*,iostat=eof) header
        if (eof < 0) exit
         do while (eof >= 0)
           read (107,*,iostat=eof) i
           if (eof < 0) exit
           imax = amax1(imax,i)
           mcn = mcn + 1
         end do
         
        allocate (cn(0:imax))
        rewind (107)
        read (107,*) titldum
        read (107,*) header
        
        do icno = 1, mcn
          read (107,*,iostat=eof) i
          backspace (107)
          read (107,*,iostat=eof) k, cn(i)
          if (eof < 0) exit
        end do
        exit
      enddo
      endif
        close(107)
      
      return 
      end subroutine cntbl_read