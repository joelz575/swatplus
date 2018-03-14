      subroutine sdr_read
      
      use input_file_module
      use maximum_data_module
      use tiles_data_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: file
      integer :: eof, i, imax
      
      eof = 0
      imax = 0
      
      !! read all subsurface drainage data from sdr.dat
      inquire (file=in_str%tiledrain_str, exist=i_exist)
      if (i_exist == 0 .or. in_str%tiledrain_str == 'null') then
        allocate (sdr(0:0))
      else
        do
          open (107,file=in_str%tiledrain_str)
          read (107,*,iostat=eof) titldum
          if (eof < 0) exit
          read (107,*) header
          if (eof < 0) exit
          do while (eof == 0)
            read (107,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
          allocate (sdr(0:imax))
          
          rewind (107)
          read (107,*) titldum
          read (107,*) header  

          do isdr = 1, imax 
            read (107,*,iostat=eof) sdr(isdr)          
            if (eof < 0) exit
          end do

          exit
        enddo
      endif
      
      db_mx%sdr = imax
      
      close(107)
      
      return
      end subroutine sdr_read