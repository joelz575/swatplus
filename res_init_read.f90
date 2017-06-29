      subroutine res_init_read
      
      use basin_module
      use input_file_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax
      real :: lnvol
      
      eof = 0
      imax = 0
      
      !read init
      inquire (file=in_res%init_res, exist=i_exist)
      if (i_exist == 0 .or. in_res%init_res == 'null') then
        allocate (res_init(0:0))
      else   
      do
       open (105,file=in_res%init_res)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%res_init = imax
      
      allocate (res_init(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
           
       do ires = 1, imax
         read (105,*,iostat=eof) titldum
         backspace (105)
         read (105,*,iostat=eof) res_init(ires)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif
      
      return
      end subroutine res_init_read