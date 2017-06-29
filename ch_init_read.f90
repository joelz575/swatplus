      subroutine ch_init_read
      
      use basin_module
      use input_file_module
      use jrw_datalib_module

      character (len=80) :: titldum, header
      integer :: eof, mon, i, imax

      eof = 0
      imax = 0
      
      inquire (file=in_cha%init, exist=i_exist)
      if (i_exist == 0 .or. in_cha%init == 'null') then
        allocate (ch_init(0:0))
      else   
      do
       open (105,file=in_cha%init)
       read (105,*,iostat=eof) titldum
       if (eof < 0) exit
       read (105,*,iostat=eof) header
       if (eof < 0) exit
        do while (eof == 0)
          read (105,*,iostat=eof) titldum
          if (eof < 0) exit
          imax = imax + 1
        end do
        
      db_mx%ch_init = imax
      
      allocate (ch_init(0:imax))
      rewind (105)
      read (105,*) titldum
      read (105,*) header
      
       do ich = 1, db_mx%ch_init
         read (105,*,iostat=eof) titldum
         backspace (105)
         read (105,*,iostat=eof) ch_init(ich)
         if (eof < 0) exit
       end do
       close (105)
      exit
      enddo
      endif

      return    
      end subroutine ch_init_read