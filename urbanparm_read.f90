      subroutine urbanparm_read
      
      use input_file_module
      use maximum_data_module
      use urban_data_module
      
      character (len=80) :: titldum
      character (len=80) :: header
      character (len=13) :: urbandb
      integer :: eof, imax
      
      eof = 0
      imax = 0
      
      inquire (file=in_parmdb%urban_urb, exist=i_exist)
      if (i_exist == 0 .or. in_parmdb%urban_urb == 'null') then
          allocate (urbdb(0:0))
      else
      do
        open (108,file=in_parmdb%urban_urb)
        read (108,*,iostat=eof) titldum
        if (eof < 0) exit
        read (108,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (108,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do
          
        allocate (urbdb(0:imax)) 
        
        rewind (108)
        read (108,*) titldum
        read (108,*) header
            
        do iu = 1, imax
           read (108,*,iostat=eof) urbdb(iu)
           if (eof < 0) exit
         end do
       exit
      enddo
      endif

      db_mx%urban = imax
      
      close (108)
      return
      end subroutine urbanparm_read